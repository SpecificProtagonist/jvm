//! This module implements object monitors.
//!
//! In Java, each object implicitly has it's own monitor,
//! which consists of a reentrant mutex and a condition variable.
//! We don't want to allocate space for a monitor in every object when few
//! of them will use it. Therefore we allocate it only under contention.
//!
//! Allocating a monitor works, like in the `parking_lot` crate,
//! via a hash-map with bucket-level hashing keyed by the object address.
//!
//! The object header reserves two bits for this, stored in 32 bit
//! together with the compressed class pointer and (TODO) the hash metadata
//! (the later of which can change):
//! - MONITOR_LOCKED_BIT: Whether the monitor is currently contended.
//! - MONITOR_CONTENDED_BIT: Used to only check waiting thread if there is one
//!
//! Reentrancy is provided by a thread-local keeping track of how many times
//! this thread has entered the monitor.
//!
//! To acquire a lock, we increment the thread-local lock count.
//! If it was previously zero:
//! - Try to set the MONITOR_LOCKED_BIT
//! - if that succeeds, we're done
//! - lock the bucket
//! - try to set MONITOR_CONTENDED_BIT
//! - if that fails because the monitor was since unlocked,
//!   just lock it, unlock the bucket and succeed
//! - add this thread to the thread queue
//! - unlock the bucket
//! - park the thread (if the thread got unparked after
//!   unlocking the buckets, this returns immediately)
//!
//! To release a lock, decrement the thread-local lock count.
//! If it reaches zero:
//! - if the MONITOR_CONTENDED_BIT isn't set:
//!   - try to unset MONITOR_LOCKED_BIT
//!   - if that fails because MONITOR_CONTENDED_BIT was set, return to start
//! - else:
//!   - lock the bucket
//!   - clear the contended flag if there is only one waiting thread
//!   - wake one thread
//!   - release bucket

pub mod maybe_loom;
mod thread_parking_lot;
mod thread_queue;

use alloc::collections::{btree_map::*, BTreeMap};
use core::cell::RefCell;
use core::num::NonZeroUsize;

use crate::miri_println;
use maybe_loom::*;

pub(crate) use thread_parking_lot::ParkingLot;

use crate::object::{MONITOR_CONTENDED_BIT, MONITOR_LOCKED_BIT};

thread_local! {
    /// Tracks which objects are currently locked by this thread how many times.
    static LOCK_COUNT: RefCell<BTreeMap<usize, u32>> = const {RefCell::new(BTreeMap::new()) };
}

impl ParkingLot {
    pub fn monitor_enter(&self, mark_ref: &AtomicU32) {
        let _tid = current().id();
        miri_println!("[{_tid:?}] enter {:?}", mark_ref as *const _);
        LOCK_COUNT.with(|cell| {
            match cell
                .borrow_mut()
                .entry((mark_ref as *const AtomicU32).addr())
            {
                Entry::Occupied(mut count) => {
                    // Already locked
                    *count.get_mut() = count.get_mut().saturating_add(1);
                    miri_println!("[{_tid:?}] reentrant → {}", count.get_mut());
                }
                Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(1);
                    // We don't know the target content of the mark word as it's
                    // shared with other functionality, so load it.
                    let mark = mark_ref.load(Relaxed);
                    let unlocked = mark & !MONITOR_LOCKED_BIT;
                    let locked = mark | MONITOR_LOCKED_BIT;
                    if mark_ref
                        .compare_exchange(unlocked, locked, Acquire, Relaxed)
                        .is_ok()
                    {
                        miri_println!("[{_tid:?}] locked without parking");
                    } else {
                        miri_println!("[{_tid:?}] failed to aquire lock");
                        // TODO: Add utility method for ref → NonZeroUsize to rust
                        let addr =
                            NonZeroUsize::new((mark_ref as *const AtomicU32).addr()).unwrap();
                        // Failed to acquire the lock,
                        // either because it was already locked or because the other data changed.
                        self.park_on_lock(addr, || loop {
                            // We have the lock on the bucket, so no other thread can set the contended bit,
                            // but the monitor may have already been unlocked.
                            let mark = mark_ref.load(Relaxed);
                            miri_println!("[{_tid:?}] loaded {mark}");
                            match mark_ref.compare_exchange(
                                mark | MONITOR_LOCKED_BIT,
                                mark | MONITOR_LOCKED_BIT | MONITOR_CONTENDED_BIT,
                                Acquire,
                                Acquire,
                            ) {
                                Ok(_) => {
                                    miri_println!("[{_tid:?}] set contended bit, going to park");
                                    // Successfully set contended bit, go park
                                    return true;
                                }
                                Err(_) => {
                                    // Either the monitor was unlocked, or the hash flags changed
                                    if mark_ref
                                        .compare_exchange(
                                            mark & !MONITOR_LOCKED_BIT,
                                            mark | MONITOR_LOCKED_BIT,
                                            Acquire,
                                            Acquire,
                                        )
                                        .is_ok()
                                    {
                                        miri_println!("[{_tid:?}] acquired lock, no need to park");
                                        // We have the lock, no need to park
                                        return false;
                                    }
                                }
                            }
                            // Failed to set the contended bit or take the lock, so try again
                        });
                        // We only get unparked when we get passed the lock
                        miri_println!("[{_tid:?}] unparked if parked → lock aquired");
                    }
                }
            };
        });
    }

    pub fn monitor_exit(&self, mark_ref: &AtomicU32) -> Result<(), LockNotHeld> {
        let _tid = current().id();
        LOCK_COUNT.with(|cell| {
            let mut map = cell.borrow_mut();
            let Entry::Occupied(mut entry) = map.entry((mark_ref as *const AtomicU32).addr())
            else {
                return Err(LockNotHeld);
            };
            *entry.get_mut() -= 1;
            miri_println!("[{_tid:?}] exit {:?}", mark_ref as *const _);
            if *entry.get() == 0 {
                miri_println!("[{_tid:?}] trying to unlock");
                entry.remove_entry();
                loop {
                    // Load because there is other data in the mark word
                    let mark = mark_ref.load(Relaxed);
                    if (mark & MONITOR_CONTENDED_BIT) == 0 {
                        if mark_ref
                            .compare_exchange(mark, mark & !MONITOR_LOCKED_BIT, AcqRel, Relaxed)
                            .is_ok()
                        {
                            miri_println!("[{_tid:?}] uncontended, unlocking");
                            return Ok(());
                        }
                    } else {
                        miri_println!("[{_tid:?}] contended → unpark one");
                        let addr =
                            NonZeroUsize::new((mark_ref as *const AtomicU32).addr()).unwrap();
                        self.unpark_one(addr, || {
                            // We hold a lock on the bucket, so no other thread can simultaneously set the contended bit
                            miri_println!("[{_tid:?}] queue empty, unsetting contended bit");
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

#[cfg(test)]
mod test {
    use super::maybe_loom::*;
    use alloc::sync::Arc;

    use crate::monitor::ParkingLot;

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
            let threads = 16;
            let objects = 8;
            let each = 8;

            let parking_lot = Arc::new(ParkingLot::default());
            let locks = Arc::new(
                (0..objects)
                    .map(|_| (AtomicU32::new(0), AtomicU32::new(0)))
                    .collect::<Vec<_>>(),
            );

            let mut handles = Vec::new();
            // No scoped threads in loom…
            for _ in 0..threads {
                let parking_lot = parking_lot.clone();
                let locks = locks.clone();
                handles.push(thread::spawn(move || {
                    for i in 0..objects * each {
                        let (object, sum) = &locks[i % locks.len()];
                        parking_lot.monitor_enter(object);
                        parking_lot.monitor_enter(object);
                        let value = sum.load(Relaxed) + 1;
                        sum.store(value, Relaxed);
                        parking_lot.monitor_exit(object).unwrap();
                        parking_lot.monitor_exit(object).unwrap();
                    }
                }));
            }
            for handle in handles {
                handle.join().unwrap();
            }

            for (_, sum) in locks.iter() {
                assert_eq!(sum.load(Relaxed) as usize, threads * each);
            }
        });
    }
}
