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
            count: 1.into(),
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

// Fixed-size open-addressing hashmap of waiting threads.
struct Map {
    /// Number of occupied buckets, including tombstones.
    count: AtomicU32,
    /// Number of low bits of the address hash used.
    hash_bits: u8,
    // Stored data.
    buckets: Box<[Bucket]>,
}

/// Map will have at most 1/LOAD_FACTOR_RECIPROCE occupied buckets.
/// This keeps probing short.
const LOAD_FACTOR_RECIPROCE: usize = 3;

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
    fn natural_index(&self, object: usize) -> usize {
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

        let count = map.count.fetch_add(1, AcqRel);
        if count as usize * LOAD_FACTOR_RECIPROCE > map.buckets.len() {
            self.grow_map();
            map = unsafe { (self.map.load(Relaxed) as *const Map).as_ref_unchecked() };
        }

        self.park_with_map(map, object, should_park);
    }

    fn park_with_map(&self, map: &Map, object: usize, should_park: impl FnOnce() -> bool) {
        let start_index = map.natural_index(object);
        let mut index = start_index;

        // Linear probing.
        // This will usually succeed at the first bucket.
        // TODO: Instead grow map when there is a collision? Needs good hash though.
        loop {
            let bucket = &map.buckets[index];
            bucket.lock.lock();
            // SAFETY: Bucket is locked
            let entry = unsafe { bucket.object() };
            if *entry == object {
                // Already present
                break;
            } else if *entry == BUCKET_EMPTY {
                // New entry
                *entry = object;
                break;
            } else {
                // Used by a different object
                index += 1;
            }
        }

        // Check whether still the same map
        let new_map = unsafe { self.map.load(Acquire).as_ref_unchecked() };
        if !std::ptr::eq(new_map, map) {
            for i in start_index..=index {
                unsafe {
                    map.buckets[i].lock.unlock();
                }
            }
            self.park_with_map(new_map, object, should_park);
            return;
        }

        let unlock_buckets = || {
            for i in start_index..=index {
                unsafe {
                    map.buckets[i].lock.unlock();
                }
            }
        };

        // We hold the lock for the bucket, see if we should park
        if should_park() {
            // SAFETY: Bucket is locked
            unsafe {
                ThreadQueue::park(&map.buckets[index].waiting_threads, unlock_buckets);
            }
        } else {
            unlock_buckets();
        }
    }

    /// Unparks one thread.
    /// Runs the provided function after bucket locking but before unparking.
    /// That function will be called with `true` if there are no more waiting threads.
    pub(super) fn unpark_one(&self, object: usize, pre_unpark: impl FnOnce(bool)) {
        // SAFETY: We always store a valid map and never take a mutable reference to it
        let mut map = unsafe { (self.map.load(Acquire) as *const Map).as_ref_unchecked() };

        let start_index = map.natural_index(object);
        let mut index = start_index;

        // Search for right bucket
        let bucket = loop {
            let bucket = &map.buckets[index];
            bucket.lock.lock();
            // SAFETY: Bucket is locked
            let entry = unsafe { bucket.object() };
            if *entry == object {
                // Found
                break Some(bucket);
            } else if *entry == BUCKET_EMPTY {
                // Not found.
                // TODO: This shouldn't happen anymore, double check this
                break None;
            } else {
                index += 1;
            }
        };

        // Check whether still the same map
        let new_map = self.map.load(Acquire);
        if !core::ptr::eq(new_map, map) {
            for i in start_index..=index {
                unsafe {
                    map.buckets[i].lock.unlock();
                }
            }
            return self.unpark_one(object, pre_unpark);
        }

        let mut end_index = index;

        if let Some(bucket) = bucket {
            unsafe {
                bucket.thread_queue().wake_one(|empty| {
                    pre_unpark(empty);

                    // Remove item, relocating any subsequent ones that were offset because this bucket was occupied.
                    if empty {
                        unsafe {
                            *map.buckets[index].object() = BUCKET_EMPTY;
                        }
                        let mut gap_index = index;
                        loop {
                            let bucket = &map.buckets[index];
                            bucket.lock.lock();
                            let entry = unsafe { bucket.object() };
                            if *entry == BUCKET_EMPTY {
                                break;
                            }
                            let natural = map.natural_index(*entry);
                            if natural <= gap_index {
                                unsafe {
                                    *map.buckets[gap_index].object() = *bucket.object();
                                    *bucket.object() = BUCKET_EMPTY;
                                    *map.buckets[gap_index].thread_queue() =
                                        core::mem::take(bucket.thread_queue());
                                }
                                gap_index = end_index;
                            }
                            end_index += 1;
                        }
                    }
                })
            }
        }

        for i in start_index..=end_index {
            unsafe {
                map.buckets[i].lock.unlock();
            }
        }
    }

    fn grow_map(&self) {
        self.grow_map_lock.lock();

        let old_ptr = self.map.load(Acquire);
        // SAFETY: We always store a valid map and never take a mutable reference to it
        let old = unsafe { (old_ptr as *const Map).as_ref_unchecked() };

        let new_capacity = old.buckets.len() + LOAD_FACTOR_RECIPROCE;
        loom_info!(old_len = old.buckets.len(), new_capacity, "growing map");

        let mut items = (0..new_capacity)
            .map(|_| Bucket::default())
            .collect::<Vec<_>>()
            .into();

        for bucket in &old.buckets {
            bucket.lock.lock();
        }

        let mut new_map = Map {
            count: 0.into(),
            hash_bits: new_capacity.trailing_zeros() as u8,
            buckets: items,
        };

        // Move entries over
        for old_bucket in &old.buckets {
            let addr =
                // SAFETY: Bucket is locked → exclusive access
                unsafe { *old_bucket.object() };
            if addr != BUCKET_EMPTY {
                let mut index = new_map.natural_index(addr);
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
            unsafe {
                bucket.lock.unlock();
            }
        }

        unsafe { Box::from_raw(old_ptr) };
        unsafe {
            self.grow_map_lock.unlock();
        }
    }
}
