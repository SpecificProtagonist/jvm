use core::{
    mem::size_of,
    num::NonZeroUsize,
    ops::Deref,
    sync::atomic::{Ordering::*, *},
};

use crate::{
    class::{Class, ClassRef},
    heap::{AtomicCompressedPtr, CompressedPtr, Heap, Ptrs},
    jvm::{throwable, JvmResult},
    monitor::maybe_loom,
    typ::Type,
    value::Value,
    AccessFlags, Jvm,
};

// Object layout (little endian):
// +—+—+—+—+—+—+—+—+—————————
// |M|Class|  Len  | Items…
// | |     |  Fields…
// +—+—+—+—+—————————————————
const OFFSET_MARK: usize = 0;
pub const OFFSET_FIELDS: usize = size_of::<CompressedPtr>();
const OFFSET_ARRAY_LENGTH: usize = OFFSET_FIELDS;
const OFFSET_ARRAY_ITEMS: usize = OFFSET_ARRAY_LENGTH + size_of::<i32>();
/// Number of free low bits in a compressed class ptr
pub const OBJ_FLAG_BITS: u32 = 4;
pub const MONITOR_LOCKED_BIT: u32 = 1;
pub const MONITOR_CONTENDED_BIT: u32 = 2;

const _: () = assert!(OFFSET_ARRAY_ITEMS % 8 == 0);

#[derive(Clone, Copy, Eq, PartialEq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub(crate) struct Object(pub NonZeroUsize);

impl Object {
    pub(crate) fn new_uninitialized(heap: &Heap, class: ClassRef) -> Self {
        unsafe {
            let ptr = heap.alloc_object(class.object_size);
            let class_ptr = heap.ptrs().encode_class(class as *const _ as usize);
            ((ptr.get() + OFFSET_MARK) as *const AtomicCompressedPtr)
                .as_ref_unchecked()
                .store(class_ptr, Relaxed);
            Self(ptr)
        }
    }

    pub(crate) fn new_array(heap: &Heap, class: ClassRef, length: i32) -> Self {
        unsafe {
            let item_size = class.element_type.as_ref().unwrap().size();
            let size = OFFSET_ARRAY_ITEMS + length as usize * item_size;
            let ptr = heap.alloc_object(size);
            let class_ptr = heap.ptrs().encode_class(class as *const _ as usize);
            ((ptr.get() + OFFSET_MARK) as *const AtomicCompressedPtr)
                .as_ref_unchecked()
                .store(class_ptr, Relaxed);
            ((ptr.get() + OFFSET_ARRAY_LENGTH) as *const AtomicI32)
                .as_ref_unchecked()
                .store(length, Relaxed);
            Self(ptr)
        }
    }

    pub fn from_addr(addr: usize) -> Option<Self> {
        NonZeroUsize::new(addr).map(Self)
    }

    #[inline]
    pub(crate) fn class(self, ptrs: Ptrs) -> ClassRef {
        let compressed = unsafe {
            ((self.0.get() + OFFSET_MARK) as *const AtomicU32)
                .as_ref_unchecked()
                .load(Relaxed)
        };

        unsafe { (ptrs.decode_class(compressed) as *const Class).as_ref_unchecked() }
    }

    pub(crate) fn mark_word(self) -> &'static maybe_loom::AtomicU32 {
        unsafe { ((self.0.get() + OFFSET_MARK) as *const maybe_loom::AtomicU32).as_ref_unchecked() }
    }

    pub fn fields(self) -> FieldStorage {
        FieldStorage(self.0.get())
    }

    pub(crate) fn array_length(self) -> i32 {
        unsafe {
            ((self.0.get() + OFFSET_ARRAY_LENGTH) as *const AtomicI32)
                .as_ref_unchecked()
                .load(Relaxed)
        }
    }

    #[inline]
    fn array_item<T>(self, index: i32) -> Option<*const T> {
        if index >= self.array_length() {
            None
        } else {
            Some((self.0.get() + OFFSET_ARRAY_ITEMS + index as usize * size_of::<T>()) as *const T)
        }
    }

    pub fn array_read<T: Data>(self, jvm: &Jvm, index: i32) -> JvmResult<T> {
        if let Some(item) = self.array_item::<T>(index) {
            Ok(T::read(jvm.heap.ptrs(), item, Relaxed))
        } else {
            Err(throwable(jvm, "ArrayIndexOutOfBoundsException", None))
        }
    }

    pub fn array_write<T: Data>(self, jvm: &Jvm, index: i32, value: T) -> JvmResult<()> {
        if let Some(item) = self.array_item::<T>(index) {
            T::write(jvm.heap.ptrs(), item, value, Relaxed);
            Ok(())
        } else {
            Err(throwable(jvm, "ArrayIndexOutOfBoundsException", None))
        }
    }

    #[inline]
    pub fn is_array(self, ptrs: Ptrs) -> bool {
        self.class(ptrs).element_type.is_some()
    }

    #[track_caller]
    fn array_read_panic<T: Data>(self, ptrs: Ptrs, index: i32) -> T {
        if let Some(item) = self.array_item::<T>(index) {
            T::read(ptrs, item, Relaxed)
        } else {
            panic!("Array index out of bounds")
        }
    }

    /// Returns either the value at index
    ///
    /// # Panics
    /// if the object is not an array or the index is out of bounds
    #[track_caller]
    pub fn array_read_value(self, ptrs: Ptrs, index: i32) -> Value {
        let Some(elem_type) = self.class(ptrs).element_type.as_ref() else {
            panic!("Tried to read array item of non-array object");
        };
        match elem_type {
            Type::Boolean | Type::Byte => {
                (self.array_read_panic::<i8>(ptrs, index) as u8 as i32).into()
            }
            Type::Short | Type::Char => {
                (self.array_read_panic::<i16>(ptrs, index) as u16 as i32).into()
            }
            Type::Int => self.array_read_panic::<i32>(ptrs, index).into(),
            Type::Long => self.array_read_panic::<i64>(ptrs, index).into(),
            Type::Float => self.array_read_panic::<f32>(ptrs, index).into(),
            Type::Double => self.array_read_panic::<f64>(ptrs, index).into(),
            Type::Ref(_) => Object::from_addr(self.array_read_panic::<usize>(ptrs, index)).into(),
        }
    }

    /// Sets an array element. If the element type is boolean, byte, short or char, the `JVMValue::Int` is truncated
    /// Panics if object is not an array, the element type mismatches or the index is out of bounds
    pub fn array_write_value(self, ptrs: Ptrs, index: i32, value: Value) {
        #[track_caller]
        fn write<T: Data>(ptrs: Ptrs, obj: Object, index: i32, value: T) {
            if let Some(item) = obj.array_item::<T>(index) {
                T::write(ptrs, item, value, Relaxed)
            } else {
                panic!("Array index out of bounds")
            }
        }
        if let Some(element_type) = &self.class(ptrs).element_type {
            match (element_type, value) {
                (Type::Boolean | Type::Byte, Value::Int(value)) => {
                    write::<i8>(ptrs, self, index, value as u8 as i8)
                }
                (Type::Short | Type::Char, Value::Int(value)) => {
                    write::<i16>(ptrs, self, index, value as u16 as i16)
                }
                (Type::Int, Value::Int(value)) => write::<i32>(ptrs, self, index, value),
                (Type::Long, Value::Long(value)) => write::<i64>(ptrs, self, index, value),
                (Type::Float, Value::Float(value)) => write::<f32>(ptrs, self, index, value),
                (Type::Float, Value::Double(value)) => write::<f64>(ptrs, self, index, value),
                (Type::Ref(typ), Value::Ref(value)) => {
                    if let Some(obj) = value {
                        if !obj.class(ptrs).assignable_to(typ) {
                            panic!("element type mismatch")
                        }
                    }
                    write::<usize>(ptrs, self, index, value.map(|o| o.0.get()).unwrap_or(0))
                }
                _ => panic!("element type mismatch"),
            }
        } else {
            panic!("Tried to read array item of non-array object");
        }
    }

    pub(crate) fn format_short(self, ptrs: Ptrs) -> String {
        struct Fmt(Object, Ptrs);
        impl core::fmt::Debug for Fmt {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                fmt_object(self.1, self.0, f, false)
            }
        }
        format!("{:?}", Fmt(self, ptrs))
    }
}

impl core::fmt::Debug for Object {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "@{:x}", self.0)
    }
}

pub(crate) fn fmt_object(
    ptrs: Ptrs,
    object: Object,
    f: &mut core::fmt::Formatter<'_>,
    full: bool,
) -> core::fmt::Result {
    let class = object.class(ptrs);

    // Prettier printing for strings
    if class.name.deref() == "java.lang.String" {
        let Value::Ref(chars) = class
            .field("chars", Type::Ref("char[]".into()))
            .expect("java.lang.String missing field char[] chars")
            .instance_get(ptrs, object)
        else {
            unreachable!()
        };
        if let Some(chars) = chars {
            write!(f, "\"")?;
            let length = chars.array_length();
            for char in char::decode_utf16(
                (0..length).map(|i| chars.array_read_panic::<i16>(ptrs, i) as u16),
            ) {
                write!(f, "{}", char.unwrap_or(char::REPLACEMENT_CHARACTER))?
            }
            return write!(f, "\"");
        } else {
            return write!(f, "null");
        }
    }

    // Arrays
    if let Some(base) = &class.element_type {
        let length = object.array_length();
        write!(f, "{}[", base)?;
        for i in 0..length {
            write!(f, "{}, ", object.array_read_value(ptrs, i))?
        }
        return write!(f, "]");
    }

    // Ordinary objects
    write!(f, "{}{{", class.name)?;
    let mut first = true;
    for field in class.iter_fields() {
        if !field.access_flags.contains(AccessFlags::STATIC) {
            if !first {
                write!(f, ", ")?;
            }
            first = false;
            write!(f, "{}: ", field.nat.name)?;
            let data = object.fields();
            match field.nat.typ {
                Type::Boolean | Type::Byte => {
                    write!(f, "{}", data.read::<i8>(ptrs, field.byte_offset, false))?
                }
                Type::Short => write!(f, "{}", data.read::<i16>(ptrs, field.byte_offset, false))?,
                Type::Char => write!(
                    f,
                    "u+{:x}",
                    data.read::<i16>(ptrs, field.byte_offset, false)
                )?,
                Type::Int => write!(f, "{}", data.read::<i32>(ptrs, field.byte_offset, false))?,
                Type::Long => write!(f, "{}", data.read::<i64>(ptrs, field.byte_offset, false))?,
                Type::Float => write!(f, "{}", data.read::<f32>(ptrs, field.byte_offset, false))?,
                Type::Double => write!(f, "{}", data.read::<f64>(ptrs, field.byte_offset, false))?,
                Type::Ref(_) => {
                    let addr = data.read::<usize>(ptrs, field.byte_offset, false);
                    if let Some(obj) = Object::from_addr(addr) {
                        if full {
                            fmt_object(ptrs, obj, f, true)?
                        } else {
                            write!(
                                f,
                                "@{:x}",
                                data.read::<usize>(ptrs, field.byte_offset, false)
                            )?
                        }
                    } else {
                        write!(f, "null")?
                    }
                }
            }
        }
    }
    write!(f, "}}")?;
    Ok(())
}

impl core::fmt::Pointer for Object {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "@{:x}", self.0)
    }
}

const _: () = assert!(core::mem::size_of::<Object>() == core::mem::size_of::<Option<Object>>());

pub trait Data {
    fn read(ptrs: Ptrs, ptr: *const Self, ordering: Ordering) -> Self;
    fn write(ptrs: Ptrs, ptr: *const Self, value: Self, ordering: Ordering);
}

impl Data for i8 {
    #[inline]
    fn read(_: Ptrs, ptr: *const Self, ordering: Ordering) -> Self {
        unsafe { (ptr as *const AtomicI8).as_ref_unchecked().load(ordering) }
    }

    #[inline]
    fn write(_: Ptrs, ptr: *const Self, value: Self, ordering: Ordering) {
        unsafe {
            (ptr as *const AtomicI8)
                .as_ref_unchecked()
                .store(value, ordering)
        }
    }
}

impl Data for i16 {
    #[inline]
    fn read(_: Ptrs, ptr: *const Self, ordering: Ordering) -> Self {
        unsafe { (ptr as *const AtomicI16).as_ref_unchecked().load(ordering) }
    }

    #[inline]
    fn write(_: Ptrs, ptr: *const Self, value: Self, ordering: Ordering) {
        unsafe {
            (ptr as *const AtomicI16)
                .as_ref_unchecked()
                .store(value, ordering)
        }
    }
}

impl Data for i32 {
    #[inline]
    fn read(_: Ptrs, ptr: *const Self, ordering: Ordering) -> Self {
        unsafe { (ptr as *const AtomicI32).as_ref_unchecked().load(ordering) }
    }

    #[inline]
    fn write(_: Ptrs, ptr: *const Self, value: Self, ordering: Ordering) {
        unsafe {
            (ptr as *const AtomicI32)
                .as_ref_unchecked()
                .store(value, ordering)
        }
    }
}

impl Data for i64 {
    #[inline]
    fn read(_: Ptrs, ptr: *const Self, ordering: Ordering) -> Self {
        unsafe { (ptr as *const AtomicI64).as_ref_unchecked().load(ordering) }
    }

    #[inline]
    fn write(_: Ptrs, ptr: *const Self, value: Self, ordering: Ordering) {
        unsafe {
            (ptr as *const AtomicI64)
                .as_ref_unchecked()
                .store(value, ordering)
        }
    }
}

impl Data for usize {
    #[inline]
    fn read(ptrs: Ptrs, ptr: *const Self, ordering: Ordering) -> Self {
        let compressed = unsafe {
            (ptr as *const AtomicCompressedPtr)
                .as_ref_unchecked()
                .load(ordering)
        };
        ptrs.decode_obj(compressed)
    }

    #[inline]
    fn write(ptrs: Ptrs, ptr: *const Self, value: Self, ordering: Ordering) {
        unsafe {
            (ptr as *const AtomicCompressedPtr)
                .as_ref_unchecked()
                .store(ptrs.encode_obj(value), ordering)
        }
    }
}

impl Data for f32 {
    #[inline]
    fn read(ptrs: Ptrs, ptr: *const Self, ordering: Ordering) -> Self {
        f32::from_bits(i32::read(ptrs, ptr as *const i32, ordering) as u32)
    }

    #[inline]
    fn write(ptrs: Ptrs, ptr: *const Self, value: Self, ordering: Ordering) {
        i32::write(ptrs, ptr as *const i32, value.to_bits() as i32, ordering);
    }
}

impl Data for f64 {
    #[inline]
    fn read(ptrs: Ptrs, ptr: *const Self, ordering: Ordering) -> Self {
        f64::from_bits(i64::read(ptrs, ptr as *const i64, ordering) as u64)
    }

    #[inline]
    fn write(ptrs: Ptrs, ptr: *const Self, value: Self, ordering: Ordering) {
        i64::write(ptrs, ptr as *const i64, value.to_bits() as i64, ordering);
    }
}

#[derive(Clone, Copy)]
pub(crate) struct FieldStorage(pub usize);

impl FieldStorage {
    pub const EMPTY: FieldStorage = Self(0);

    #[inline]
    pub(crate) fn read<T: Data>(self, ptrs: Ptrs, offset: usize, volatile: bool) -> T {
        T::read(
            ptrs,
            (self.0 as *const T).wrapping_byte_add(offset),
            if volatile { Acquire } else { Relaxed },
        )
    }

    #[inline]
    pub(crate) fn write<T: Data>(self, ptrs: Ptrs, offset: usize, value: T, volatile: bool) {
        T::write(
            ptrs,
            (self.0 as *const T).wrapping_byte_add(offset),
            value,
            if volatile { Release } else { Relaxed },
        )
    }
}
