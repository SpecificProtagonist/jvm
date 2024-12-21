#![allow(unused)]
mod mutex;
mod thread_parking_lot;
mod thread_queue;

use std::{
    cell::RefCell,
    collections::{btree_map::*, BTreeMap},
    sync::atomic::{AtomicPtr, AtomicU32, Ordering::*},
    thread::current,
};

pub(crate) use thread_parking_lot::ParkingLot;

use crate::{
    miri_println,
    object::{Object, MONITOR_CONTENDED_BIT, MONITOR_LOCKED_BIT},
};

///! This module implements object monitors.
///!
///! In Java, each object implicitly has it's own monitor,
///! which consists of a reentrant mutex and a condition variable.
///! We don't want to allocate space for a monitor in every object when few
///! of them will use it. Therefore we allocate it only under contention.
///!
///! Allocating a monitor works, like in the `parking_lot` crate,
///! via a hash-map with bucket-level hashing keyed by the object address.
///!
///! The object header reserves two bits for this, stored in 32 bit
///! together with the compressed class pointer and (TODO) the hash metadata
///! (the later of which can change):
///! - MONITOR_LOCKED_BIT: Whether the monitor is currently contended.
///! - MONITOR_CONTENDED_BIT: Used to only check waiting thread if there is one
///!
///! Reentrancy is provided by a thread-local keeping track of how many times
///! this thread has entered the monitor.
///!
///! To acquire a lock, we increment the thread-local lock count.
///! If it was previously zero:
///! - Try to set the MONITOR_LOCKED_BIT
///! - if that succeeds, we're done
///! - lock the bucket
///! - try to set MONITOR_CONTENDED_BIT
///! - if that fails because the monitor was since unlocked,
///!   just lock it, unlock the bucket and succeed
///! - add this thread to the thread queue
///! - unlock the bucket
///! - park the thread (if the thread got unparked after
///!   unlocking the buckets, this returns immediately)
///!
///! To release a lock, decrement the thread-local lock count.
///! If it reaches zero:
///! - if the MONITOR_CONTENDED_BIT isn't set:
///!   - try to unset MONITOR_LOCKED_BIT
///!   - if that fails because MONITOR_CONTENDED_BIT was set, return to start
///! - else:
///!   - lock the bucket
///!   - clear the contended flag if there is only one waiting thread
///!   - wake one thread
///!   - release bucket

thread_local! {
    /// Tracks which objects are currently locked by this thread how many times.
    static LOCK_COUNT: RefCell<BTreeMap<usize, u32>> = const {RefCell::new(BTreeMap::new()) };
}

impl ParkingLot {
    pub fn monitor_enter(&self, mark_ref: &AtomicU32) {
        miri_println!("[{:?}] Enter", current().id());
        LOCK_COUNT.with_borrow_mut(|map| match map.entry(mark_ref.as_ptr().addr()) {
            Entry::Occupied(mut count) => {
                // Already locked
                *count.get_mut() = count.get_mut().saturating_add(1);
            }
            Entry::Vacant(vacant_entry) => {
                let mut mark = mark_ref.load(Relaxed);
                let unlocked = mark & !MONITOR_LOCKED_BIT;
                let locked = mark & MONITOR_LOCKED_BIT;
                if mark_ref
                    .compare_exchange(unlocked, locked, Acquire, Acquire)
                    .is_err()
                {
                    // Failed to acquire the lock
                    self.park_on_lock(mark_ref.as_ptr().addr(), || loop {
                        // We have the lock on the bucket, so no other thread can set the contended bit, but the monitor may have already been unlocked.
                        let mut mark = mark_ref.load(Relaxed);
                        match mark_ref.compare_exchange(
                            mark | MONITOR_LOCKED_BIT,
                            mark | MONITOR_LOCKED_BIT | MONITOR_CONTENDED_BIT,
                            Acquire,
                            Acquire,
                        ) {
                            Ok(_) => {
                                // Successfully set contended bit, go sleep
                                return true;
                            }
                            Err(_) => {
                                // Either the monitor was unlocked, or the hash flags changed
                                if mark_ref
                                    .compare_exchange(
                                        mark | !MONITOR_LOCKED_BIT,
                                        mark | MONITOR_LOCKED_BIT,
                                        Acquire,
                                        Acquire,
                                    )
                                    .is_ok()
                                {
                                    // We have the lock, no need to park
                                    return false;
                                }
                            }
                        }
                    });
                    // We only get unparked when we get passed the lock
                } else {
                    miri_println!("locked without parking");
                }
                // Sucessfully locked object
                vacant_entry.insert(1);
            }
        });
    }

    pub fn monitor_exit(&self, mark_ref: &AtomicU32) -> Result<(), LockNotHeld> {
        miri_println!("[{:?}] Exit", current().id());
        LOCK_COUNT.with_borrow_mut(|map| {
            let Entry::Occupied(mut entry) = map.entry(mark_ref.as_ptr().addr()) else {
                return Err(LockNotHeld);
            };
            *entry.get_mut() -= 1;
            if *entry.get() == 0 {
                entry.remove_entry();
                loop {
                    // Load because there is other data in the mark word
                    let mark = mark_ref.load(Relaxed);
                    if (mark & MONITOR_CONTENDED_BIT) != 0 {
                        if mark_ref
                            .compare_exchange(mark, mark & !MONITOR_LOCKED_BIT, AcqRel, Relaxed)
                            .is_ok()
                        {
                            return Ok(());
                        }
                    } else {
                        self.unpark_one(mark_ref.as_ptr().addr(), |empty| {
                            if empty {
                                loop {
                                    let mark = mark_ref.load(Relaxed);
                                    if mark_ref
                                        .compare_exchange(
                                            mark,
                                            mark & !MONITOR_CONTENDED_BIT,
                                            Relaxed,
                                            Relaxed,
                                        )
                                        .is_ok()
                                    {
                                        return;
                                    }
                                }
                            }
                        });
                        return Ok(());
                    }
                }
            }
            Ok(())
        })
    }
}

#[derive(Debug)]
pub struct LockNotHeld;

#[cfg(test)]
mod test {
    use std::sync::atomic::{AtomicU32, Ordering::*};

    use crate::{heap::Heap, monitor_2::ParkingLot};

    use super::Object;

    #[test]
    fn new_monitor_reentrancy() {
        let parking_lot = ParkingLot::new();
        let mark_ref = &AtomicU32::new(0);
        parking_lot.monitor_enter(mark_ref);
        parking_lot.monitor_enter(mark_ref);
        parking_lot.monitor_exit(mark_ref).unwrap();
        parking_lot.monitor_exit(mark_ref).unwrap();
    }

    #[test]
    fn new_monitor_contention() {
        let threads = 4;
        let per_thread = 10;

        let parking_lot = ParkingLot::new();
        let mark_ref = &AtomicU32::new(0);
        let sum = AtomicU32::new(0);

        std::thread::scope(|scope| {
            for _ in 0..threads {
                scope.spawn(|| {
                    for _ in 0..per_thread {
                        parking_lot.monitor_enter(mark_ref);
                        parking_lot.monitor_enter(mark_ref);
                        sum.store(sum.load(Relaxed) + 1, Relaxed);
                        parking_lot.monitor_exit(mark_ref).unwrap();
                        parking_lot.monitor_exit(mark_ref).unwrap();
                    }
                });
            }
        });

        parking_lot.monitor_enter(mark_ref);
        assert_eq!(sum.load(Relaxed), threads * per_thread);
    }
}
