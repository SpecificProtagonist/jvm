use std::{
    alloc::Layout,
    mem::size_of,
    sync::atomic::{AtomicI16, AtomicI32, AtomicI64, AtomicI8, AtomicU32, AtomicU64, Ordering},
};

use crate::{
    exception,
    heap::{self, AtomicJVMPtr, Heap, JVMPtr, JVMPtrNonNull},
    object, JVMResult, JVM,
};

// TODO: use usize instead of u64 for size (consider alignment of Long/Double)
/// Contains pointer to heap allocation containing allocation size followed by space for fields/array items
#[derive(Clone, Copy)]
#[repr(transparent)]
pub(crate) struct FieldStorage(JVMPtrNonNull);

macro_rules! access {
    ($read:ident, $write:ident, $array_read:ident, $array_write:ident, $array_read_freestanding:ident, $array_write_freestanding:ident, $atomic:ident, $typ:ident, $convert_read:path, $convert_write:ident) => {
        /// SAFETY: Must be in-bound & correctly alligned
        #[inline]
        pub unsafe fn $read(&self, offset: usize, volatile: bool) -> $typ {
            $convert_read(
                (&*(heap::ptr_decode(self.0.into()).add(offset) as *const $atomic)).load(
                    if volatile {
                        Ordering::Acquire
                    } else {
                        Ordering::Relaxed
                    },
                ),
            )
        }

        /// SAFETY: Must be in-bound & correctly alligned
        #[inline]
        pub unsafe fn $write(&self, offset: usize, value: $typ, volatile: bool) {
            (&*(heap::ptr_decode(self.0.into()).add(offset) as *const $atomic)).store(
                value.$convert_write(),
                if volatile {
                    Ordering::Release
                } else {
                    Ordering::Relaxed
                },
            )
        }

        #[inline]
        pub fn $array_read<'a>(&self, jvm: &JVM<'a>, index: i32) -> JVMResult<'a, $typ> {
            let offset = object::header_size() + index as usize * std::mem::size_of::<$typ>();
            if (index < 0) | (offset >= self.size()) {
                Err(exception(jvm, "ArrayIndexOutOfBoundsException"))
            } else {
                Ok(unsafe { self.$read(offset, false) })
            }
        }

        #[inline]
        pub fn $array_write<'a>(
            &self,
            jvm: &JVM<'a>,
            index: i32,
            value: $typ,
        ) -> JVMResult<'a, ()> {
            let offset = object::header_size() + index as usize * std::mem::size_of::<$typ>();
            if (index < 0) | (offset >= self.size()) {
                Err(exception(jvm, "ArrayIndexOutOfBoundsException"))
            } else {
                Ok(unsafe { self.$write(offset, value, false) })
            }
        }

        #[inline]
        pub fn $array_read_freestanding<'a>(&self, index: i32) -> $typ {
            let offset = object::header_size() + index as usize * std::mem::size_of::<$typ>();
            if (index < 0) | (offset >= self.size()) {
                panic!("Array index out of bounds")
            } else {
                unsafe { self.$read(offset, false) }
            }
        }

        #[inline]
        #[allow(unused)]
        pub fn $array_write_freestanding<'a>(&self, index: i32, value: $typ) {
            let offset = object::header_size() + index as usize * std::mem::size_of::<$typ>();
            if (index < 0) | (offset >= self.size()) {
                panic!("Array index out of bounds")
            } else {
                unsafe { self.$write(offset, value, false) }
            }
        }
    };
}

impl FieldStorage {
    pub fn new(heap: &Heap, size: usize) -> Self {
        Self(unsafe {
            let ptr = heap.alloc(layout(size)) as *mut u8;
            // Store allocation size for safety checks
            *(ptr as *mut u64) = size as u64;
            JVMPtrNonNull::new_unchecked(heap::ptr_encode(ptr.wrapping_add(size_of::<u64>())))
        })
    }

    /// SAFETY: Must have been allocated by a FieldStorage
    pub unsafe fn from_ptr(ptr: JVMPtrNonNull) -> Self {
        Self(ptr)
    }

    pub fn ptr(self) -> JVMPtrNonNull {
        self.0
    }

    /// Size usable for fields, does not include size necessary for storing size
    pub fn size(self) -> usize {
        // SAFETY: Always points to a valid FieldStorage
        unsafe { *(heap::ptr_decode(self.0.into()) as *const u64).offset(-1) as usize }
    }

    // Sadly concat_idents doesn't help here
    access!(
        read_i8,
        write_i8,
        array_read_i8,
        array_write_i8,
        array_read_i8_freestanding,
        array_write_i8_freestanding,
        AtomicI8,
        i8,
        std::convert::identity,
        into
    );
    access!(
        read_i16,
        write_i16,
        array_read_i16,
        array_write_i16,
        array_read_i16_freestanding,
        array_write_i16_freestanding,
        AtomicI16,
        i16,
        std::convert::identity,
        into
    );
    access!(
        read_i32,
        write_i32,
        array_read_i32,
        array_write_i32,
        array_read_i32_freestanding,
        array_write_i32_freestanding,
        AtomicI32,
        i32,
        std::convert::identity,
        into
    );
    access!(
        read_i64,
        write_i64,
        array_read_i64,
        array_write_i64,
        array_read_i64_freestanding,
        array_write_i64_freestanding,
        AtomicI64,
        i64,
        std::convert::identity,
        into
    );
    access!(
        read_ptr,
        write_ptr,
        array_read_ptr,
        array_write_ptr,
        array_read_ptr_freestanding,
        array_write_ptr_freestanding,
        AtomicJVMPtr,
        JVMPtr,
        std::convert::identity,
        into
    );
    access!(
        read_f32,
        write_f32,
        array_read_f32,
        array_write_f32,
        array_read_f32_freestanding,
        array_write_f32_freestanding,
        AtomicU32,
        f32,
        f32::from_bits,
        to_bits
    );
    access!(
        read_f64,
        write_f64,
        array_read_f64,
        array_write_f64,
        array_read_f64_freestanding,
        array_write_f64_freestanding,
        AtomicU64,
        f64,
        f64::from_bits,
        to_bits
    );
}

unsafe fn layout(size: usize) -> Layout {
    Layout::from_size_align_unchecked(size_of::<u64>() + size, 8)
}
