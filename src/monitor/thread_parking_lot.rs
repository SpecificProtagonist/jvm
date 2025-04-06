use core::cell::{SyncUnsafeCell, UnsafeCell};
use core::num::NonZeroUsize;

use super::{maybe_loom::*, thread_queue::ThreadQueue};
use crate::{default, heap::OBJECT_ALIGN, miri_println};

#[cfg(loom)]
use lock_api::RawMutex as _;

/// A map from an adress to a queue of threads waiting on that it.
pub(crate) struct ParkingLot {
    /// The current hashmap, will be replaced with a larger one when too full.
    ///
    /// SAFETY: This always points to the current map. An exclusive reference will never be taken.
    map: AtomicPtr<Map>,
}

impl Default for ParkingLot {
    fn default() -> Self {
        let map = Box::into_raw(Box::new(Map {
            hash_bits: 0,
            buckets: Box::new([Bucket::default()]),
        }))
        .into();
        Self { map }
    }
}

impl Drop for ParkingLot {
    fn drop(&mut self) {
        unsafe { drop(Box::from_raw(self.map.load(Ordering::Acquire))) };
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
    /// Stored data.
    // TODO: Flatten into map.
    // This is possible with repr(C) and some fiddling, but pretty annoying
    buckets: Box<[Bucket]>,
}

struct Bucket {
    object: UnsafeCell<Option<NonZeroUsize>>,
    lock: RawMutex,
    /// Queue of threads waiting to take the lock
    lock_queue: SyncUnsafeCell<ThreadQueue>,
    /// Queue of threads waiting to be notified
    notify_queue: SyncUnsafeCell<ThreadQueue>,
}

impl Default for Bucket {
    fn default() -> Self {
        Self {
            object: default(),
            lock: RawMutex::INIT,
            lock_queue: default(),
            notify_queue: default(),
        }
    }
}

impl Map {
    /// Index of the bucket where the element will be placed if there are no collisions.
    fn index(&self, object: NonZeroUsize) -> usize {
        // TODO: Does this work as a decent hash function?
        let base = object.get() >> OBJECT_ALIGN.trailing_zeros();
        base.wrapping_add(base >> self.hash_bits) & ((1 << self.hash_bits) - 1)
    }
}

impl Bucket {
    /// SAFETY: Must have locked bucket and no references are held
    #[allow(clippy::mut_from_ref)]
    unsafe fn object(&self) -> &mut Option<NonZeroUsize> {
        (&self.object as *const _ as *mut Option<NonZeroUsize>).as_mut_unchecked()
    }

    /// SAFETY: Must have locked bucket and no references are held
    #[allow(clippy::mut_from_ref)]
    unsafe fn lock_queue(&self) -> &mut ThreadQueue {
        (&self.lock_queue as *const _ as *mut ThreadQueue).as_mut_unchecked()
    }

    /// SAFETY: Must have locked bucket and no references are held
    #[allow(clippy::mut_from_ref)]
    unsafe fn notify_queue(&self) -> &mut ThreadQueue {
        (&self.notify_queue as *const _ as *mut ThreadQueue).as_mut_unchecked()
    }
}

impl ParkingLot {
    /// Park the current thread on `object` until it's unparked.
    /// `should_park` is called once, during which a lock for the object is held.
    /// If it returns `false`, cancels the parking.
    pub(super) fn park_on_lock(&self, object: NonZeroUsize, should_park: impl FnOnce() -> bool) {
        // SAFETY: We always store a valid map and never take a mutable reference to it
        let map = unsafe { (self.map.load(Acquire) as *const Map).as_ref_unchecked() };

        let index = map.index(object);
        miri_println!("[{:?}] index {index}", current().id());

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
        match entry {
            Some(entry) if *entry == object => {
                // Already present
            }
            Some(_) => {
                // Used by a different object → grow map
                unsafe { bucket.lock.unlock() };
                self.grow_map();
                self.park_on_lock(object, should_park);
                return;
            }
            None => {
                // New entry
                *entry = Some(object);
            }
        }

        // SAFETY: We've locked the bucket
        let unlock_bucket = || unsafe {
            map.buckets[index].lock.unlock();
        };

        // We hold the lock for the bucket, see if we should park
        if should_park() {
            // SAFETY: Bucket is locked
            unsafe {
                ThreadQueue::enqueue(map.buckets[index].lock_queue());
                unlock_bucket();
            }
            ThreadQueue::park();
        } else {
            unlock_bucket();
        }
    }

    /// Unparks one thread.
    /// Runs the provided function after bucket locking but before unparking,
    /// if there is only one thread waiting.
    pub(super) fn unpark_one(
        &self,
        object: NonZeroUsize,
        park_till_notified: bool,
        on_cleared: impl FnOnce(),
    ) {
        // SAFETY: We always store a valid map and never take a mutable reference to it
        let map = unsafe { (self.map.load(Acquire) as *const Map).as_ref_unchecked() };

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
            return self.unpark_one(object, park_till_notified, on_cleared);
        }

        // SAFETY: Bucket locked
        unsafe {
            if park_till_notified {
                ThreadQueue::enqueue(bucket.notify_queue());
            }
            bucket.lock_queue().wake_one(|| {
                on_cleared();
                // Remove entry
                *map.buckets[index].object() = None;
            });
        }
        if park_till_notified {
            ThreadQueue::park();
        }

        // SAFETY: Bucket is locked
        unsafe {
            map.buckets[index].lock.unlock();
        }
    }

    pub(super) fn notify_one(&self, object: NonZeroUsize) {
        // SAFETY: We always store a valid map and never take a mutable reference to it
        let map = unsafe { (self.map.load(Acquire) as *const Map).as_ref_unchecked() };

        let index = map.index(object);
        let bucket = &map.buckets[index];
        bucket.lock.lock();
        unsafe {
            bucket.notify_queue().wake_one(|| {});
        }
    }

    pub(super) fn notify_all(&self, object: NonZeroUsize) {
        // SAFETY: We always store a valid map and never take a mutable reference to it
        let map = unsafe { (self.map.load(Acquire) as *const Map).as_ref_unchecked() };

        let index = map.index(object);
        let bucket = &map.buckets[index];
        bucket.lock.lock();
        unsafe {
            bucket.notify_queue().wake_all();
        }
    }

    /// Switches to a larger hashmap. Requires no buckets to be locked by this thread.
    fn grow_map(&self) {
        let old_ptr = self.map.load(Acquire);
        // SAFETY: We always store a valid map and never take a mutable reference to it
        let old = unsafe { (old_ptr as *const Map).as_ref_unchecked() };

        let mut capacity = old.buckets.len();

        // Lock all buckets
        for old_bucket in &old.buckets {
            old_bucket.lock.lock();
        }

        // We may need to grow multiple times in case of conflict
        let new_map = 'grow: loop {
            capacity *= 2;
            let items: Box<[Bucket]> = (0..capacity)
                .map(|_| Bucket::default())
                .collect::<Vec<_>>()
                .into();

            miri_println!(
                "[{:?}] growing map: {:?}",
                current().id(),
                &items[0] as *const _
            );

            let new_map = Map {
                hash_bits: capacity.trailing_zeros() as u8,
                buckets: items,
            };

            // Move entries over
            for old_bucket in &old.buckets {
                // SAFETY: Bucket is locked → exclusive access
                if let Some(object) = unsafe { *old_bucket.object() } {
                    let index = new_map.index(object);
                    let new_bucket = &new_map.buckets[index];
                    if unsafe { new_bucket.object() }.replace(object).is_some() {
                        // Conflict, need larger size
                        continue 'grow;
                    }
                }
            }
            // Success
            break new_map;
        };

        // Move over thread queues
        for old_bucket in &old.buckets {
            // SAFETY: Bucket is locked → exclusive access
            if let Some(object) = unsafe { *old_bucket.object() } {
                let index = new_map.index(object);
                let new_bucket = &new_map.buckets[index];
                unsafe {
                    *new_bucket.lock_queue() = core::mem::take(old_bucket.lock_queue());
                }
            }
        }

        let new_map = Box::into_raw(Box::new(new_map));

        self.map.store(new_map, Release);

        for bucket in &old.buckets {
            // SAFETY: All buckets are locked
            unsafe {
                bucket.lock.unlock();
            }
        }

        // TODO: If we want to use miri's leak detection,
        // we need to stash the old map somewhere when miri is active
    }
}
