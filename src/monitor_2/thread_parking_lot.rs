#![allow(unused)]
use core::cell::{SyncUnsafeCell, UnsafeCell};

use super::thread_queue::ThreadQueue;
use crate::{default, heap::OBJECT_ALIGN, sync::*};

#[cfg(loom)]
use lock_api::RawMutex as _;

/// A thread parking lot.
pub(crate) struct ParkingLot {
    /// The current hashmap, will be replaced with a larger one when too full.
    ///
    /// SAFETY: This always points to the current map. An exclusive reference will never be taken.
    map: AtomicPtr<Map>,
    /// Taken while growing the map.
    grow_map_lock: RawMutex,
}

impl Default for ParkingLot {
    fn default() -> Self {
        let map = Box::into_raw(Box::new(Map {
            hash_bits: 0,
            buckets: Box::new([Bucket::default()]),
        }))
        .into();
        Self {
            map,
            grow_map_lock: RawMutex::INIT,
        }
    }
}

/// Fixed-size hashmap of waiting threads.
///
/// There is at most one entry per thread and speed is important,
/// therefore instead of open addressing or chaining, we grow the
/// map on collision.
struct Map {
    /// Number of low bits of the address hash used.
    hash_bits: u8,
    // Stored data.
    buckets: Box<[Bucket]>,
}

/// No item in this bucket
const BUCKET_EMPTY: usize = 0;

struct Bucket {
    object: UnsafeCell<usize>,
    lock: RawMutex,
    waiting_threads: SyncUnsafeCell<ThreadQueue>,
}

impl Default for Bucket {
    fn default() -> Self {
        Self {
            object: default(),
            lock: RawMutex::INIT,
            waiting_threads: default(),
        }
    }
}

impl Map {
    /// Index of the bucket where the element will be placed if there are no collisions.
    fn index(&self, object: usize) -> usize {
        // TODO: Does this work as a decent hash function?
        let base = object >> OBJECT_ALIGN.trailing_zeros();
        base.wrapping_add(base >> self.hash_bits) & ((1 << self.hash_bits) - 1)
    }
}

impl Bucket {
    /// SAFETY: Must have locked bucket and no references are held
    #[allow(clippy::mut_from_ref)]
    unsafe fn object(&self) -> &mut usize {
        (&self.object as *const _ as *mut usize).as_mut_unchecked()
    }

    /// SAFETY: Must have locked bucket and no references are held
    #[allow(clippy::mut_from_ref)]
    unsafe fn thread_queue(&self) -> &mut ThreadQueue {
        (&self.waiting_threads as *const _ as *mut ThreadQueue).as_mut_unchecked()
    }
}

impl ParkingLot {
    /// Park the current thread on `object` until it's unparked.
    /// `should_park` is called once, during which a lock for the object is held.
    /// If it returns `false`, cancels the parking.
    pub(super) fn park_on_lock(&self, object: usize, should_park: impl FnOnce() -> bool) {
        debug_assert!(object != 0);
        // SAFETY: We always store a valid map and never take a mutable reference to it
        let mut map = unsafe { (self.map.load(Acquire) as *const Map).as_ref_unchecked() };

        let index = map.index(object);

        let bucket = &map.buckets[index];
        bucket.lock.lock();

        // Was the map grown by another thread?
        let new_map = self.map.load(Acquire);
        if !core::ptr::eq(new_map, map) {
            unsafe {
                map.buckets[index].lock.unlock();
            }
            return self.park_on_lock(object, should_park);
        }

        // SAFETY: Bucket is locked
        let entry = unsafe { bucket.object() };
        if *entry == BUCKET_EMPTY {
            // New entry
            *entry = object;
        } else if *entry == object {
            // Already present
        } else {
            // Used by a different object → grow map
            unsafe { bucket.lock.unlock() };
            self.grow_map();
            self.park_on_lock(object, should_park);
            return;
        }

        // SAFETY: We've locked the bucket
        let unlock_bucket = || unsafe {
            map.buckets[index].lock.unlock();
        };

        // We hold the lock for the bucket, see if we should park
        if should_park() {
            // SAFETY: Bucket is locked
            unsafe {
                ThreadQueue::park(&map.buckets[index].waiting_threads, unlock_bucket);
            }
        } else {
            unlock_bucket();
        }
    }

    /// Unparks one thread.
    /// Runs the provided function after bucket locking but before unparking.
    /// That function will be called with `true` if there are no more waiting threads.
    pub(super) fn unpark_one(&self, object: usize, pre_unpark: impl FnOnce(bool)) {
        // SAFETY: We always store a valid map and never take a mutable reference to it
        let mut map = unsafe { (self.map.load(Acquire) as *const Map).as_ref_unchecked() };

        let index = map.index(object);
        let bucket = &map.buckets[index];
        bucket.lock.lock();

        // Was the map grown by another thread?
        let new_map = self.map.load(Acquire);
        if !core::ptr::eq(new_map, map) {
            // SAFETY: Bucket locked
            unsafe {
                map.buckets[index].lock.unlock();
            }
            return self.unpark_one(object, pre_unpark);
        }

        let mut end_index = index;

        unsafe {
            bucket.thread_queue().wake_one(|empty| {
                // Bucket is locked
                pre_unpark(empty);

                if empty {
                    // Remove entry
                    // SAFETY: Bucket locked
                    unsafe {
                        *map.buckets[index].object() = BUCKET_EMPTY;
                    }
                }
            })
        }

        // SAFETY: Bucket is locked
        unsafe {
            map.buckets[index].lock.unlock();
        }
    }

    /// Switches to a larger hashmap. Requires no buckets to be locked by this thread.
    fn grow_map(&self) {
        let old_ptr = self.map.load(Acquire);
        // SAFETY: We always store a valid map and never take a mutable reference to it
        let old = unsafe { (old_ptr as *const Map).as_ref_unchecked() };

        for bucket in &old.buckets {
            bucket.lock.lock();
        }

        let new_capacity = old.buckets.len() * 2;
        loom_info!(old_len = old.buckets.len(), new_capacity, "growing map");

        let mut items = (0..new_capacity)
            .map(|_| Bucket::default())
            .collect::<Vec<_>>()
            .into();

        let mut new_map = Map {
            hash_bits: new_capacity.trailing_zeros() as u8,
            buckets: items,
        };

        // Move entries over
        for old_bucket in &old.buckets {
            let addr =
                // SAFETY: Bucket is locked → exclusive access
                unsafe { *old_bucket.object() };
            if addr != BUCKET_EMPTY {
                let mut index = new_map.index(addr);
                // Search for right bucket
                loop {
                    let new_bucket = &new_map.buckets[index];
                    new_bucket.lock.lock();
                    // SAFETY: Bucket is locked
                    let entry = unsafe { new_bucket.object() };
                    if *entry == BUCKET_EMPTY {
                        *entry = addr;
                        unsafe {
                            *new_bucket.thread_queue() = core::mem::take(old_bucket.thread_queue());
                        }
                        break;
                    } else {
                        index += 1;
                    }
                }
            }
        }

        let new_map = Box::into_raw(Box::new(new_map));

        self.map.store(new_map, Release);

        loom_info!(
            old = old as *const _ as usize,
            new = new_map as usize,
            "written"
        );

        for bucket in &old.buckets {
            // SAFETY: All buckets are locked
            unsafe {
                bucket.lock.unlock();
            }
        }

        // We can't deallocate the old map yet because we don't know if another thread is still using it.
        // Tell miri we deliberately leak.
        Box::leak(unsafe { Box::from_raw(old_ptr) });
    }
}
