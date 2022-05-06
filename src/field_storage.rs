use std::{
    alloc::Layout,
    mem::size_of,
    sync::atomic::{AtomicI16, AtomicI32, AtomicI64, AtomicI8, Ordering},
};

use crate::heap::{self, AtomicJVMPtr, Heap, JVMPtrSize};

// TODO: use usize instead of u64 for size (consider alignment of Long/Double)
/// Contains pointer to heap allocation containing allocation size followed by space for fields/array items
/// Stored as usize instead of pointer because pointers don't impl Send+Sync
#[derive(Clone, Copy)]
#[repr(transparent)]
pub(crate) struct FieldStorage(pub JVMPtrSize);

macro_rules! access {
    ($read:ident, $write:ident, $atomic:ident, $typ:ident) => {
        #[allow(clippy::modulo_one)]
        pub fn $read(&self, offset: u32) -> Option<$typ> {
            if self.0 == 0
                || (offset as usize > self.size())
                    | (offset % (std::mem::size_of::<$typ>() as u32) > 0)
            {
                None
            } else {
                unsafe {
                    Some(
                        (&*(heap::ptr_decode(self.0).offset(offset as isize) as *const $atomic))
                            .load(Ordering::Relaxed),
                    )
                }
            }
        }

        #[allow(clippy::modulo_one)]
        pub fn $write(&self, offset: u32, value: $typ) -> Option<()> {
            if self.0 == 0
                || (offset as usize > self.size())
                    | (offset % (std::mem::size_of::<$typ>() as u32) > 0)
            {
                None
            } else {
                unsafe {
                    (&*(heap::ptr_decode(self.0).offset(offset as isize) as *const $atomic))
                        .store(value, Ordering::Relaxed)
                }
                Some(())
            }
        }
    };
}

impl FieldStorage {
    pub fn new(heap: &Heap, size: usize) -> Self {
        Self(unsafe {
            let ptr = heap.alloc(layout(size));
            // Store allocation size for safety checks
            *(ptr as *mut u64) = size as u64;
            heap::ptr_encode(ptr.wrapping_add(size_of::<u64>()))
        })
    }

    pub fn ptr(self) -> JVMPtrSize {
        self.0
    }

    /// Size usable for fields, does not include size necessary for storing size
    pub fn size(self) -> usize {
        unsafe { *(heap::ptr_decode(self.0) as *const u64).offset(-1) as usize }
    }

    access!(read_i8, write_i8, AtomicI8, i8);
    access!(read_i16, write_i16, AtomicI16, i16);
    access!(read_i32, write_i32, AtomicI32, i32);
    access!(read_i64, write_i64, AtomicI64, i64);
    access!(read_ptr, write_ptr, AtomicJVMPtr, JVMPtrSize);

    pub fn read_f32(&self, offset: u32) -> Option<f32> {
        self.read_i32(offset).map(|i| f32::from_bits(i as u32))
    }

    pub fn write_f32(&self, offset: u32, value: f32) -> Option<()> {
        self.write_i32(offset, value.to_bits() as i32)
    }

    pub fn read_f64(&self, offset: u32) -> Option<f64> {
        self.read_i64(offset).map(|i| f64::from_bits(i as u64))
    }

    pub fn write_f64(&self, offset: u32, value: f64) -> Option<()> {
        self.write_i64(offset, value.to_bits() as i64)
    }
}

unsafe fn layout(size: usize) -> Layout {
    Layout::from_size_align_unchecked(size_of::<u64>() + size, 8)
}
