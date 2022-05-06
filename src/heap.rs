//! This module is responsible for managing the jvm heap and providing 32-bit pointers.
//! On 32-bit platforms we can just use normal pointers.
//! On amd64 Linux, we can use mmap with MAP_32BIT.
//! On other architectures (test with qemu) or when >4gb heap is required,
//! - check posix_mem_offset() or parse /proc/self/maps and mmap with MAP_FIXED_NOREPLACE
//! - in case of larger heap, shift pointers by log2(object_alignment) before use
//! On Windows, use VirtualAlloc/NtAllocateVirtualMemory
//! On an unsupported architecture, use usize for pointers & local/stack values
//!
//! There's no GC (for now), so just this is a simple (but threadsafe) bump allocator.

// Currently, provenance goes ignored. This shouldn't be a problem, but TODO: check out the impact of this

use parking_lot::Mutex;
use std::{
    alloc::Layout,
    marker::PhantomData,
    sync::atomic::{AtomicPtr, Ordering},
};

use backend::{alloc_block, dealloc_block};
pub use backend::{ptr_decode, ptr_encode, AtomicJVMPtr, JVMPtrSize};

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
mod backend {

    pub type JVMPtrSize = u32;
    /// Only used by field_storage for read/write
    pub type AtomicJVMPtr = std::sync::atomic::AtomicU32;

    pub fn ptr_encode(ptr: *mut u8) -> JVMPtrSize {
        ptr as usize as JVMPtrSize
    }

    pub fn ptr_decode(ptr: JVMPtrSize) -> *mut u8 {
        ptr as usize as *mut u8
    }

    // TODO: hint addr to try to alloc consecutive blocks
    // and don't throw away free space at end of previous block if successful
    pub fn alloc_block(len: usize) -> *mut u8 {
        unsafe {
            let addr = libc::mmap(
                std::ptr::null_mut(),
                len,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_SHARED | libc::MAP_ANONYMOUS | libc::MAP_32BIT,
                -1,
                0,
            );
            if addr == libc::MAP_FAILED {
                panic!(
                    "Failed to allocate heap: {}",
                    std::io::Error::last_os_error()
                )
            }
            addr as *mut u8
        }
    }

    pub unsafe fn dealloc_block(addr: *mut u8, len: usize) {
        // Should not possibly fail, and if it does anyways, ignore it
        libc::munmap(addr as *mut libc::c_void, len);
    }
}

#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
mod backend {

    pub type JVMPtrSize = usize;
    pub type AtomicJVMPtr = AtomicUsize;

    pub fn ptr_encode(ptr: *mut u8) -> JVMPtrSize {
        ptr
    }

    pub fn ptr_decode(ptr: JVMPtrSize) -> *mut u8 {
        ptr
    }

    fn alloc_backing(len: usize) -> *mut u8 {
        todo!()
    }

    fn dealloc_backing(addr: *mut u8, len: usize) {
        todo!()
    }
}

// TODO: make this configurable per JVM
// (and either also store in Backing or only allow configuration in JVM constructor)
const MIN_BLOCK_SIZE: usize = TYPICAL_PAGE_SIZE * 1024;
const TYPICAL_PAGE_SIZE: usize = 4096;

struct Block {
    addr: AtomicPtr<u8>,
    len: usize,
}

// TODO: maybe keep a thread-local buffer to reduce locking
#[derive(Default)]
pub struct Heap<'jvm> {
    /// Start of free part of current backing
    current_start: AtomicPtr<u8>,
    /// Points one past end
    current_end: AtomicPtr<u8>,
    backing: Mutex<Vec<Block>>,
    _marker: PhantomData<&'jvm ()>,
}

impl<'jvm> Heap<'jvm> {
    pub fn alloc(&self, layout: Layout) -> *mut u8 {
        loop {
            let start = self.current_start.load(Ordering::SeqCst);
            let end = self.current_end.load(Ordering::SeqCst);
            // Ensure alignment
            // For compressed pointers and a >4gb heap, leave the last thre bits unset
            let align = layout.align().max(8);
            let addr = round_to_multiple(start as usize, align) as *mut u8;
            let next = addr.wrapping_add(layout.size());
            assert!(next >= start);
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
                    Ok(_) => return addr,
                    Err(_) => continue,
                }
            } else {
                // Not enough space left

                let mut blocks = self.backing.lock();
                // Has another thread already set a new block?
                if end != self.current_end.load(Ordering::SeqCst) {
                    continue;
                }
                // Make sure the new block large enough
                let block_size =
                    MIN_BLOCK_SIZE.max(round_to_multiple(layout.size(), TYPICAL_PAGE_SIZE));
                let addr = alloc_block(block_size);
                blocks.push(Block {
                    addr: addr.into(),
                    len: block_size,
                });
                self.current_start
                    .store(addr.wrapping_add(layout.size()), Ordering::SeqCst);
                self.current_end
                    .store(addr.wrapping_add(block_size), Ordering::SeqCst);
                return addr;
            }
        }
    }

    pub fn alloc_typed<T>(&self, value: T) -> &'jvm T {
        let ptr = self.alloc(Layout::new::<T>()) as *mut T;
        unsafe {
            *ptr = value;
            &*ptr
        }
    }
}

fn round_to_multiple(value: usize, multiple: usize) -> usize {
    (value + multiple - 1) / multiple * multiple
}

impl Drop for Block {
    fn drop(&mut self) {
        // SAFETY: dealloc with same addr/len as allocated
        unsafe { dealloc_block(self.addr.load(Ordering::SeqCst), self.len) }
    }
}
