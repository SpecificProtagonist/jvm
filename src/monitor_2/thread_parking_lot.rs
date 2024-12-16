#![allow(unused)]
// Implements a parking lot.
// We cannot use dashmap as we need to hold the lock while sleeping.

use std::{
    cell::UnsafeCell,
    sync::atomic::{AtomicPtr, AtomicU32, AtomicU8, Ordering::*},
    thread::Thread,
};

use crate::heap::OBJECT_ALIGN;

use super::{mutex::Mutex, thread_queue::ThreadQueue};

pub struct ParkingLot {
    map: AtomicPtr<Map>,
}

struct Map {
    /// Number of occupied buckets, including tombstones
    count: AtomicU32,
    /// Number of low bits of the address hash used
    hash_bits: u8,
    buckets: Box<[Bucket]>,
}

/// Map will have at most 1/LOAD_FACTOR_RECIPROCE occupied buckets
const LOAD_FACTOR_RECIPROCE: usize = 3;

/// No item in this bucket
const BUCKET_EMPTY: usize = 0;

#[derive(Default)]
struct Bucket {
    object: UnsafeCell<usize>,
    lock: Mutex,
    thread_queue: UnsafeCell<ThreadQueue>,
}

impl Map {
    fn natural_index(&self, object: usize) -> usize {
        // TODO: Choose an actually good hash function
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
        (&self.thread_queue as *const _ as *mut ThreadQueue).as_mut_unchecked()
    }
}

impl ParkingLot {
    pub fn new() -> Self {
        let map = Box::into_raw(Box::new(Map {
            count: 0.into(),
            hash_bits: 0,
            buckets: Box::new([]),
        }))
        .into();
        Self { map }
    }

    pub fn insert(&self, object: usize) {
        debug_assert!(object != 0);
        // SAFETY: We always store a valid map and never take a mutable reference to it
        let mut map = unsafe { (self.map.load(Acquire) as *const Map).as_ref_unchecked() };

        let count = map.count.fetch_add(1, AcqRel);

        if count as usize * LOAD_FACTOR_RECIPROCE == map.buckets.len() {
            self.grow_map(map);
            map = unsafe { (self.map.load(Relaxed) as *const Map).as_ref_unchecked() };
        }

        self.park(map, object);
    }

    fn park(&self, map: &Map, object: usize) {
        let start_index = map.natural_index(object);
        let mut index = start_index;

        // Search for right bucket
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
                index += 1;
            }
        }

        // Check whether still the same map
        let new_map = self.map.load(Acquire);
        if new_map != map as *const _ as _ {
            for i in start_index..=index {
                map.buckets[i].lock.unlock();
            }
            self.park(unsafe { new_map.as_ref_unchecked() }, object);
            return;
        }

        // SAFETY: Bucket is locked
        unsafe {
            map.buckets[index].thread_queue().push();
        }

        for i in start_index..=index {
            map.buckets[i].lock.unlock();
        }

        // Consume unpark token if thread has been unparked between bucket unlock
        // and now, otherwise wait for unpark token.
        std::thread::park();
    }

    fn unpark_one(&self, map: &Map, object: usize) {
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
                // This can happen because a thread waking from contention always sets the contended flag.
                // TODO: can we make this unreachable by returning whether there are more parked threads?
                break None;
            } else {
                index += 1;
            }
        };

        // Check whether still the same map
        let new_map = self.map.load(Acquire);
        if new_map != map as *const _ as _ {
            for i in start_index..=index {
                map.buckets[i].lock.unlock();
            }
            self.unpark_one(unsafe { new_map.as_ref_unchecked() }, object);
            return;
        }

        let empty = if let Some(bucket) = bucket {
            unsafe { bucket.thread_queue().wake_one_check_if_empty() }
        } else {
            true
        };

        let mut end_index = index;
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
                            std::mem::take(bucket.thread_queue());
                    }
                    gap_index = end_index;
                }
                end_index += 1;
            }
        }

        for i in start_index..=end_index {
            map.buckets[i].lock.unlock();
        }
    }

    fn grow_map(&self, old: &'static Map) {
        let new_capacity = old.buckets.len() + LOAD_FACTOR_RECIPROCE;

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
                            *new_bucket.thread_queue() = std::mem::take(old_bucket.thread_queue());
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

        for bucket in &old.buckets {
            bucket.lock.unlock();
        }
    }
}

#[cfg(test)]
mod test {
    use std::sync::atomic::AtomicU32;

    use super::ParkingLot;

    #[test]
    fn parking_lot() {}
}
