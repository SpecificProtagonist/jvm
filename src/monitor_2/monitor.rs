#![allow(unused)]
mod thread_parking_lot;
mod thread_queue;

use std::{
    cell::{RefCell, RefMut},
    collections::{btree_map::*, BTreeMap},
    panic::Location,
};

use crate::sync::*;
use thread::current;
pub(crate) use thread_parking_lot::ParkingLot;

use crate::object::{Object, MONITOR_CONTENDED_BIT, MONITOR_LOCKED_BIT};

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
        loom_info!(object = (mark_ref as *const _ as usize), "enter");
        LOCK_COUNT.with(|cell| {
            let mut map = cell.borrow_mut();
            match map.entry((mark_ref as *const AtomicU32).addr()) {
                Entry::Occupied(mut count) => {
                    // Already locked
                    *count.get_mut() = count.get_mut().saturating_add(1);
                    loom_info!(new_count = count.get_mut(), "Reentrant");
                }
                Entry::Vacant(vacant_entry) => {
                    loom_info!("Not reentrant");
                    let mut mark = mark_ref.load(Relaxed);
                    let unlocked = mark & !MONITOR_LOCKED_BIT;
                    let locked = mark & MONITOR_LOCKED_BIT;
                    if mark_ref
                        .compare_exchange(unlocked, locked, Acquire, Acquire)
                        .is_err()
                    {
                        loom_info!("already locked by other thread");
                        // Failed to acquire the lock
                        self.park_on_lock((mark_ref as *const AtomicU32).addr(), || loop {
                            // We have the lock on the bucket, so no other thread can set the contended bit, but the monitor may have already been unlocked.
                            let mut mark = mark_ref.load(Relaxed);
                            match mark_ref.compare_exchange(
                                mark | MONITOR_LOCKED_BIT,
                                mark | MONITOR_LOCKED_BIT | MONITOR_CONTENDED_BIT,
                                Acquire,
                                Acquire,
                            ) {
                                Ok(_) => {
                                    loom_info!("set contended bit, going to park");
                                    // Successfully set contended bit, go park
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
                                        loom_info!("acquired lock, no need to park");
                                        // We have the lock, no need to park
                                        return false;
                                    }
                                }
                            }
                        });
                        // We only get unparked when we get passed the lock
                    } else {
                        loom_info!("locked without parking");
                    }
                    // Sucessfully locked object
                    vacant_entry.insert(1);
                }
            };
            drop(map);
        });
    }

    pub fn monitor_exit(&self, mark_ref: &AtomicU32) -> Result<(), LockNotHeld> {
        LOCK_COUNT.with(|cell| {
            let mut map = cell.borrow_mut();
            let Entry::Occupied(mut entry) = map.entry((mark_ref as *const AtomicU32).addr())
            else {
                loom_info!("lock not held");
                return Err(LockNotHeld);
            };
            *entry.get_mut() -= 1;
            loom_info!(
                object = (mark_ref as *const _ as usize),
                new_count = *entry.get(),
                "monitor_exit"
            );
            if *entry.get() == 0 {
                loom_info!("trying to unlock");
                entry.remove_entry();
                loop {
                    // Load because there is other data in the mark word
                    let mark = mark_ref.load(Relaxed);
                    if (mark & MONITOR_CONTENDED_BIT) == 0 {
                        if mark_ref
                            .compare_exchange(mark, mark & !MONITOR_LOCKED_BIT, AcqRel, Relaxed)
                            .is_ok()
                        {
                            loom_info!("uncontended, unlocking");
                            return Ok(());
                        }
                    } else {
                        loom_info!("unparking one");
                        self.unpark_one((mark_ref as *const AtomicU32).addr(), |empty| {
                            if empty {
                                loom_info!("queue empty, unlocking");
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
            drop(map);
            Ok(())
        })
    }
}

#[derive(Debug)]
pub struct LockNotHeld;

// TODO: Only build these if loom is active?
#[cfg(test)]
mod test {
    use crate::{heap::Heap, monitor_2::ParkingLot, sync::*};

    use super::Object;

    fn model<F>(fun: F)
    where
        F: Fn() + Sync + Send + 'static,
    {
        #[cfg(not(loom))]
        fun();
        #[cfg(loom)]
        loom::model(fun);
    }

    #[test]
    fn new_monitor_reentrancy() {
        model(|| {
            let parking_lot = ParkingLot::default();
            let mark_ref = &AtomicU32::new(0);
            parking_lot.monitor_enter(mark_ref);
            parking_lot.monitor_enter(mark_ref);
            parking_lot.monitor_exit(mark_ref).unwrap();
            parking_lot.monitor_exit(mark_ref).unwrap();
        });
    }

    #[test]
    fn new_monitor_contention() {
        model(|| {
            let threads = 2;
            let per_thread = 1;

            let parking_lot = Arc::new(ParkingLot::default());
            let mark_ref = Arc::new(AtomicU32::new(0));
            let sum = Arc::new(AtomicU32::new(0));

            let mut handles = Vec::new();
            // No scoped threads in loom…
            for _ in 0..threads {
                let parking_lot = parking_lot.clone();
                let mark_ref = mark_ref.clone();
                let sum = sum.clone();
                handles.push(thread::spawn(move || {
                    loom_info!("Start test thread");
                    for i in 0..per_thread {
                        loom_info!(iter = i, "");
                        parking_lot.monitor_enter(&mark_ref);
                        parking_lot.monitor_enter(&mark_ref);
                        sum.store(sum.load(Relaxed) + 1, Relaxed);
                        loom_info!(new = sum.load(Relaxed), "Mutate locked data");
                        parking_lot.monitor_exit(&mark_ref).unwrap();
                        parking_lot.monitor_exit(&mark_ref).unwrap();
                    }
                    loom_info!("Completed test thread");
                }));
            }
            for handle in handles {
                handle.join();
            }

            assert_eq!(sum.load(Relaxed), threads * per_thread);
        });
    }
}
