use std::{
    alloc::{alloc_zeroed, dealloc, handle_alloc_error, Layout},
    mem::size_of,
    sync::atomic::{AtomicI16, AtomicI32, AtomicI64, AtomicI8, AtomicUsize, Ordering},
};

macro_rules! access {
    ($read:ident,$write:ident,$atomic:ident,$size:expr,$typ:ident) => {
        pub fn $read(&self, offset: u32) -> Option<$typ> {
            if self.0.is_null() || (offset as usize > self.size()) | (offset % $size > 0) {
                None
            } else {
                unsafe {
                    Some(
                        (&*(self.0.offset(offset as isize) as *const $atomic))
                            .load(Ordering::Relaxed),
                    )
                }
            }
        }
        pub fn $write(&self, offset: u32, value: $typ) -> Option<()> {
            if self.0.is_null() || (offset as usize > self.size()) | (offset % $size > 0) {
                None
            } else {
                unsafe {
                    (&*(self.0.offset(offset as isize) as *const $atomic))
                        .store(value, Ordering::Relaxed)
                }
                Some(())
            }
        }
    };
}

/// Contains pointer to heap allocation containing allocation size followed by space for fields/array items
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct FieldStorage(*const u8);

impl FieldStorage {
    pub fn new(size: usize) -> Self {
        Self(unsafe {
            let layout = layout(size);
            let ptr = alloc_zeroed(layout) as *const u8;
            if ptr == std::ptr::null() {
                handle_alloc_error(layout)
            }
            // Store allocation size for safety checks
            *(ptr as *mut u64) = size as u64;
            ptr.offset(size_of::<u64>() as isize)
        })
    }

    pub fn addr(self) -> usize {
        self.0 as usize
    }

    /// Size usable for fields, does not include size necessary for storing size
    pub fn size(self) -> usize {
        unsafe { *(self.0 as *const u64).offset(-1) as usize }
    }

    /// Safety: Must only be called once, and called with the layout used to create this
    pub unsafe fn delete(self) {
        dealloc(
            (self.0 as *const u64).offset(-1) as *mut u8,
            layout(self.size() as usize),
        )
    }

    access!(read_i8, write_i8, AtomicI8, 1, i8);
    access!(read_i16, write_i16, AtomicI16, 2, i16);
    access!(read_i32, write_i32, AtomicI32, 4, i32);
    access!(read_i64, write_i64, AtomicI64, 8, i64);
    access!(
        read_usize,
        write_usize,
        AtomicUsize,
        size_of::<usize>() as u32,
        usize
    );

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
    Layout::from_size_align_unchecked(size_of::<u64>() + size, size_of::<u64>())
}
