//! This module is responsible for managing the jvm heap.
//!
//! The full heap is preallocated (which will use overcommit on some platforms).
//! TODO: on Windows, use VirtualAlloc
//! to reserve memory and only commit as needed.
//! TODO: Miri (just target windows?)
//!
//! There's no GC (for now), so just this is a very simple (but threadsafe) bump allocator.
//!
//! Pointers are compressed to 32bit.
//! The upper 4 bits of a compressed class pointer are free, so they can be used for other purposes.

// Currently, provenance goes ignored.

use core::{
    num::NonZeroUsize,
    sync::atomic::{AtomicUsize, Ordering::*},
};

pub const OBJECT_ALIGN: usize = 8;
const CLASS_ALIGN: usize = 32;
const MAX_CLASS_ADDR: usize = (1 << 24) * CLASS_ALIGN;

// TODO: keep a thread-local buffer to reduce locking
pub struct Heap {
    /// Points one past end (todo: make this pointer)
    heap_top: usize,
    heap_bottom: usize,
    /// Start of free space
    free_ptr: AtomicUsize,
    /// End of free space
    free_ptr_bottom: AtomicUsize,
}

impl Heap {
    pub fn new(max_heap: usize) -> Self {
        let heap_bottom =
            memory_map::reserve_memory_range(max_heap.min(ptrs::HEAP_SIZE_LIMIT)) as usize;
        let heap_top = heap_bottom + max_heap;

        Self {
            heap_top,
            heap_bottom,
            free_ptr: heap_top.into(),
            free_ptr_bottom: heap_bottom.into(),
        }
    }

    pub fn alloc_object(&self, size: usize) -> NonZeroUsize {
        loop {
            let old_free_ptr = self.free_ptr.load(Relaxed);
            let new_free_ptr = (old_free_ptr - size) & !(OBJECT_ALIGN - 1);
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

    pub fn alloc_permanent(&self, size: usize, align: usize) -> NonZeroUsize {
        loop {
            let old_bottom = self.free_ptr_bottom.load(Relaxed);
            let new_bottom = (old_bottom + size + align - 1) & !(align - 1);
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
                #[cfg(target_pointer_width = "64")]
                {
                    if new_bottom - self.heap_bottom > MAX_CLASS_ADDR {
                        panic!("Max permanent gen size exceeded");
                    }
                }
                return unsafe { NonZeroUsize::new_unchecked(old_bottom) };
            }
        }
    }

    pub fn alloc_class(&self, value: Class) -> ClassRef {
        let ptr = self
            .alloc_permanent(core::mem::size_of::<Class>(), CLASS_ALIGN)
            .get() as *mut Class;
        unsafe {
            ptr.write(value);
            &*ptr
        }
    }
}

impl Drop for Heap {
    fn drop(&mut self) {
        unsafe {
            memory_map::unmap(
                self.heap_bottom as *mut u8,
                self.heap_top - self.heap_bottom,
            );
        }
    }
}

/// Currently always 32bit, maybe add option for >32gb heaps later?
pub type CompressedPtr = u32;
/// Only used by field_storage for read/write
pub type AtomicCompressedPtr = core::sync::atomic::AtomicU32;

pub use ptrs::Ptrs;

use crate::class::{Class, ClassRef};

mod memory_map {
    #[cfg(not(target_os = "windows"))]
    pub fn reserve_memory_range(size: usize) -> *mut u8 {
        use libc::{MAP_ANONYMOUS, MAP_FAILED, MAP_PRIVATE, PROT_READ, PROT_WRITE};

        let result = unsafe {
            libc::mmap(
                core::ptr::null_mut(),
                size,
                PROT_READ | PROT_WRITE,
                MAP_PRIVATE | MAP_ANONYMOUS,
                -1,
                0,
            )
        };
        if core::ptr::eq(result, MAP_FAILED) {
            panic!("Failed to reserve memory range for heap");
        }
        result as *mut u8
    }

    #[cfg(not(target_os = "windows"))]
    pub unsafe fn unmap(base: *mut u8, size: usize) {
        // We can't do anything useful about errors here
        libc::munmap(base as _, size);
    }

    #[cfg(target_os = "windows")]
    fn reserve_memory_range(size: usize) -> *mut u8 {
        // winapi::um::memoryapi::VirtualAlloc();
        todo!();
    }
}

#[cfg(target_pointer_width = "64")]
mod ptrs {

    use super::{CompressedPtr, Heap, CLASS_ALIGN, OBJECT_ALIGN};
    use crate::object::OBJ_FLAG_BITS;

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
        #[inline(always)]
        pub fn encode_obj(&self, ptr: usize) -> CompressedPtr {
            ((ptr - self.base_ptr) >> OBJECT_ALIGN.trailing_zeros()) as u32
        }

        #[inline(always)]
        pub fn decode_obj(&self, ptr: CompressedPtr) -> usize {
            ((ptr as usize) << OBJECT_ALIGN.trailing_zeros()) + self.base_ptr
        }

        #[inline(always)]
        pub fn encode_class(&self, ptr: usize) -> CompressedPtr {
            ((ptr - self.base_ptr) >> (CLASS_ALIGN.trailing_zeros() - OBJ_FLAG_BITS)) as u32
        }

        #[inline(always)]
        pub fn decode_class(&self, ptr: CompressedPtr) -> usize {
            let relevant = ptr & !((1 << OBJ_FLAG_BITS) - 1);
            ((relevant as usize) << (CLASS_ALIGN.trailing_zeros() - OBJ_FLAG_BITS)) + self.base_ptr
        }
    }
}

#[cfg(target_pointer_width = "32")]
mod ptrs {
    use super::{CompressedPtr, Heap, CLASS_ALIGN, OBJ_FLAG_BITS};
    use crate::object::OBJ_FLAG_BITS;

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
        #[inline(always)]
        pub fn encode_obj(&self, ptr: usize) -> CompressedPtr {
            ptr as u32
        }

        #[inline(always)]
        pub fn decode_obj(&self, ptr: CompressedPtr) -> usize {
            ptr as usize
        }

        #[inline(always)]
        pub fn encode_class(&self, ptr: usize) -> CompressedPtr {
            (ptr as u32) << (OBJ_FLAG_BITS - CLASS_ALIGN.trailing_zeros())
        }

        #[inline(always)]
        pub fn decode_class(&self, ptr: CompressedPtr) -> usize {
            (ptr as usize) >> (OBJ_FLAG_BITS - CLASS_ALIGN.trailing_zeros())
        }
    }
}
