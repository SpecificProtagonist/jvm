// This whole file (and the way I handle pointers in general) is a huge mess
// Things to look into: single chunk with overcommit
// - doesn't play nicely with compressed pointers
// - probably helps with gc when it's time to implement it

//! This module is responsible for managing the jvm heap and providing 32-bit pointers.
//! On 32-bit platforms we can just use normal pointers.
//! On amd64 Linux, we can use mmap with MAP_32BIT.
//! TODO: Instead use heap base pointer
//!
//! TODO: on Windows, use VirtualAlloc/NtAllocateVirtualMemory
//!
//! There's no GC (for now), so just this is a simple (but threadsafe) bump allocator.

// Currently, provenance goes ignored. This shouldn't be a problem, but TODO: check out the impact of this

use std::{
    alloc::Layout,
    num::NonZeroUsize,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Mutex,
    },
};

// TODO: make this configurable per JVM
// (and either also store in Backing or only allow configuration in JVM constructor)
const MIN_BLOCK_SIZE: usize = TYPICAL_PAGE_SIZE * 1024;
const TYPICAL_PAGE_SIZE: usize = 4096;

struct Block {
    addr: AtomicUsize,
    len: usize,
}

// TODO: keep a thread-local buffer to reduce locking
#[derive(Default)]
pub struct Heap {
    ptrs: PtrsBackend,
    /// Start of free part of current backing
    current_start: AtomicUsize,
    /// Points one past end
    current_end: AtomicUsize,
    backing: Mutex<Vec<Block>>,
}

impl Heap {
    /// Used for compressing/decompressing pointers;
    /// ZST if compressed pointers are unavailable.
    #[inline]
    pub fn ptrs(&self) -> Ptrs {
        self.ptrs.as_ref()
    }

    pub fn alloc(&self, layout: Layout) -> NonZeroUsize {
        loop {
            let start = self.current_start.load(Ordering::SeqCst);
            let end = self.current_end.load(Ordering::SeqCst);
            // Ensure alignment
            // For compressed pointers and a >4gb heap, leave the last three bits unset
            let align = layout.align().max(8);
            let addr = round_to_multiple(start, align);
            let next = addr.wrapping_add(layout.size());
            assert!(next > start);
            // Does the allocation fit inside the current block?
            if next <= end {
                // Different blocks can't start at the same address.
                // Therefore, if the address hasn't changed, we're still
                // in the same block. Otherwise try again.
                match self.current_start.compare_exchange(
                    start,
                    next,
                    Ordering::SeqCst,
                    Ordering::SeqCst,
                ) {
                    Ok(_) => return NonZeroUsize::new(addr).unwrap(),
                    Err(_) => continue,
                }
            } else {
                // Not enough space left

                let mut blocks = self.backing.lock().unwrap();
                // Has another thread already set a new block?
                if end != self.current_end.load(Ordering::SeqCst) {
                    continue;
                }
                // Make sure the new block large enough
                let block_size =
                    MIN_BLOCK_SIZE.max(round_to_multiple(layout.size(), TYPICAL_PAGE_SIZE));
                let addr = self.ptrs.alloc_block(block_size, end) as usize;

                // Extend the previous block if possible
                if addr == end {
                    blocks.last_mut().unwrap().len += block_size;
                    self.current_start.store(next, Ordering::SeqCst);
                    self.current_end
                        .store(end.wrapping_add(block_size), Ordering::SeqCst);
                    return NonZeroUsize::new(start).unwrap();
                } else {
                    blocks.push(Block {
                        addr: addr.into(),
                        len: block_size,
                    });
                    self.current_start
                        .store(addr.wrapping_add(layout.size()), Ordering::SeqCst);
                    self.current_end
                        .store(addr.wrapping_add(block_size), Ordering::SeqCst);
                    return NonZeroUsize::new(addr).unwrap();
                }
            }
        }
    }

    pub fn alloc_typed<T>(&self, value: T) -> &'static T {
        let ptr = self.alloc(Layout::new::<T>()).get() as *mut T;
        unsafe {
            ptr.write(value);
            &*ptr
        }
    }
}

fn round_to_multiple(value: usize, multiple: usize) -> usize {
    value.div_ceil(multiple) * multiple
}

impl Drop for Heap {
    fn drop(&mut self) {
        for block in self.backing.lock().unwrap().iter() {
            // SAFETY: dealloc with same addr/len as allocated
            unsafe {
                self.ptrs
                    .dealloc_block(block.addr.load(Ordering::SeqCst), block.len)
            }
        }
    }
}
pub use backend::*;

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
mod backend {
    use std::ops::Deref;

    pub type CompressedPtr = u32;
    /// Only used by field_storage for read/write
    pub type AtomicCompressedPtr = std::sync::atomic::AtomicU32;

    #[derive(Copy, Clone)]
    pub(crate) struct Ptrs;

    impl Deref for Ptrs {
        type Target = PtrsBackend;

        fn deref(&self) -> &Self::Target {
            &PtrsBackend
        }
    }

    #[derive(Default)]
    pub(crate) struct PtrsBackend;

    impl PtrsBackend {
        pub fn as_ref(&self) -> Ptrs {
            Ptrs
        }

        pub fn encode(&self, ptr: usize) -> CompressedPtr {
            ptr as u32
        }

        pub fn decode(&self, ptr: CompressedPtr) -> usize {
            ptr as usize
        }

        pub fn alloc_block(&self, len: usize, addr_hint: usize) -> *mut u8 {
            unsafe {
                let addr = libc::mmap(
                    addr_hint as *mut libc::c_void,
                    len,
                    libc::PROT_READ | libc::PROT_WRITE,
                    libc::MAP_PRIVATE | libc::MAP_ANONYMOUS | libc::MAP_32BIT,
                    -1,
                    0,
                );
                if addr == libc::MAP_FAILED {
                    // TODO: throw OutOfMemoryError
                    panic!(
                        "Failed to allocate heap: {}",
                        std::io::Error::last_os_error()
                    )
                }
                addr as *mut u8
            }
        }

        pub unsafe fn dealloc_block(&self, addr: usize, len: usize) {
            // Should not possibly fail, and if it does anyways, ignore it
            libc::munmap(addr as *mut libc::c_void, len);
        }
    }
}

#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
mod backend {
    use std::{alloc::Layout, ops::Deref};

    pub type CompressedPtr = usize;
    /// Only used by field_storage for read/write
    pub type AtomicCompressedPtr = std::sync::atomic::AtomicUsize;

    #[derive(Copy, Clone)]
    pub(crate) struct Ptrs;

    impl Deref for Ptrs {
        type Target = PtrsBackend;

        fn deref(&self) -> &Self::Target {
            &PtrsBackend
        }
    }

    #[derive(Default)]
    pub(crate) struct PtrsBackend;

    impl PtrsBackend {
        pub fn as_ref(&self) -> Ptrs {
            Ptrs
        }

        pub fn encode(&self, ptr: usize) -> CompressedPtr {
            ptr
        }

        pub fn decode(&self, ptr: CompressedPtr) -> usize {
            ptr
        }

        pub fn alloc_block(&self, len: usize, _addr_hint: usize) -> *mut u8 {
            unsafe {
                std::alloc::alloc_zeroed(
                    Layout::from_size_align(len, std::mem::align_of::<u64>()).unwrap(),
                )
            }
        }

        pub unsafe fn dealloc_block(&self, addr: usize, len: usize) {
            std::alloc::dealloc(
                addr as *mut u8,
                Layout::from_size_align(len, std::mem::align_of::<u64>()).unwrap(),
            )
        }
    }
}
