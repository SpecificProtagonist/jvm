//! This module is responsible for managing the jvm heap.
//!
//! The full heap is preallocated (which will use overcommit on some platforms).
//! TODO: on Windows, use VirtualAlloc/NtAllocateVirtualMemory
//! to reserve memory and only commit as needed.
//!
//! There's no GC (for now), so just this is a very simple (but threadsafe) bump allocator.

// Currently, provenance goes ignored.

use std::{
    alloc::Layout,
    num::NonZeroUsize,
    sync::atomic::{AtomicUsize, Ordering::*},
};

const HEAP_ALIGN: usize = 4096;

// TODO: keep a thread-local buffer to reduce locking
pub struct Heap {
    /// Points one past end
    heap_top: usize,
    heap_bottom: usize,
    /// Start of free space
    free_ptr: AtomicUsize,
    /// End of free space
    free_ptr_bottom: AtomicUsize,
}

impl Heap {
    pub fn new(max_heap: usize) -> Self {
        let max_heap = max_heap.min(backend::HEAP_SIZE_LIMIT) & !(HEAP_ALIGN - 1);
        let heap_bottom = unsafe {
            std::alloc::alloc_zeroed(Layout::from_size_align(max_heap, HEAP_ALIGN).unwrap())
                as usize
        };
        let heap_top = heap_bottom + max_heap;

        Self {
            heap_top,
            heap_bottom,
            free_ptr: heap_top.into(),
            free_ptr_bottom: heap_bottom.into(),
        }
    }

    pub fn alloc(&self, layout: Layout) -> NonZeroUsize {
        loop {
            let old_free_ptr = self.free_ptr.load(Relaxed);
            let new_free_ptr = (old_free_ptr - layout.size()) & !(layout.align() - 1);
            if self
                .free_ptr
                .compare_exchange(old_free_ptr, new_free_ptr, Release, Relaxed)
                .is_ok()
            {
                let old_bottom = self.free_ptr_bottom.load(Acquire);
                if new_free_ptr < old_bottom {
                    // TODO: OutOfMemoryError
                    panic!("Max JVM heap size exceeded");
                }
                return unsafe { NonZeroUsize::new_unchecked(new_free_ptr) };
            }
        }
    }

    pub fn alloc_permanent(&self, layout: Layout) -> NonZeroUsize {
        loop {
            let old_bottom = self.free_ptr_bottom.load(Relaxed);
            let new_bottom = (old_bottom + layout.size()) & !(layout.align() - 1);
            if self
                .free_ptr_bottom
                .compare_exchange(old_bottom, new_bottom, Release, Relaxed)
                .is_ok()
            {
                let old_free_ptr = self.free_ptr.load(Acquire);
                if new_bottom > old_free_ptr {
                    // TODO: OutOfMemoryError
                    panic!("Max JVM heap size exceeded");
                }
                return unsafe { NonZeroUsize::new_unchecked(old_bottom) };
            }
        }
    }

    pub fn alloc_permanent_typed<T>(&self, value: T) -> &'static T {
        let ptr = self.alloc_permanent(Layout::new::<T>()).get() as *mut T;
        unsafe {
            ptr.write(value);
            &*ptr
        }
    }
}

impl Drop for Heap {
    fn drop(&mut self) {
        unsafe {
            std::alloc::dealloc(
                self.heap_bottom as *mut u8,
                Layout::from_size_align(self.heap_top - self.heap_bottom, HEAP_ALIGN).unwrap(),
            );
        }
    }
}

/// Currently always 32bit, maybe add option for >32gb heaps later?
pub type CompressedPtr = u32;
/// Only used by field_storage for read/write
pub type AtomicCompressedPtr = std::sync::atomic::AtomicU32;

pub use backend::Ptrs;

#[cfg(target_pointer_width = "64")]
mod backend {
    use super::{CompressedPtr, Heap};

    pub const HEAP_SIZE_LIMIT: usize = 1 << 35;

    /// Used for compressing/decompressing pointers;
    /// ZST if compressed pointers are unavailable.
    #[derive(Copy, Clone)]
    pub struct Ptrs {
        base_ptr: usize,
    }

    impl Heap {
        #[inline]
        pub fn ptrs(&self) -> Ptrs {
            Ptrs {
                base_ptr: self.heap_bottom,
            }
        }
    }

    impl Ptrs {
        pub fn encode(&self, ptr: usize) -> CompressedPtr {
            ((ptr - self.base_ptr) >> 3) as u32
        }

        pub fn decode(&self, ptr: CompressedPtr) -> usize {
            ((ptr as usize) << 3) + self.base_ptr
        }
    }
}

#[cfg(target_pointer_width = "32")]
mod backend {
    use super::{CompressedPtr, Heap};

    pub const HEAP_SIZE_LIMIT: usize = usize::MAX;

    /// Used for compressing/decompressing pointers;
    /// ZST if compressed pointers are unavailable.
    #[derive(Copy, Clone)]
    pub struct Ptrs;

    impl Heap {
        #[inline]
        pub fn ptrs(&self) -> Ptrs {
            Ptrs
        }
    }

    impl Ptrs {
        pub fn encode(&self, ptr: usize) -> CompressedPtr {
            ptr as u32
        }

        pub fn decode(&self, ptr: CompressedPtr) -> usize {
            ptr as usize
        }
    }
}
