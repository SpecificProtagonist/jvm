use std::{
    alloc::Layout,
    mem::size_of,
    ops::Deref,
    sync::atomic::{
        AtomicI16, AtomicI32, AtomicI64, AtomicI8,
        Ordering::{self, *},
    },
};

use crate::{
    class::{Class, ClassRef},
    heap::{self, AtomicJvmPtr, Heap, JvmPtr, JvmPtrNonNull},
    jvm::{throwable, JvmResult},
    monitor::Monitor,
    typ::Type,
    value::Value,
    AccessFlags, Jvm,
};

// 64 bit arch:
// +—+—+—+—+—+—+—+—+—+—+—+—+—+—+—+—+—+—+—+—+———————
// |     Class     |    Monitor    |  Len  | Items…
// |               |               |  Fields…
// +—+—+—+—+—+—+—+—+—+—+—+—+—+—+—+—+———————————————
//
// 32 bit arch or compressed pointers:
// +—+—+—+—+—+—+—+—+—+—+—+—+—+—+—+—+———————
// | Class |    Monitor    |  Len  | Items…
// |       |               |  Fields…
// +—+—+—+—+—+—+—+—+—+—+—+—+———————————————
const OFFSET_CLASS: usize = 0;
const OFFSET_MONITOR: usize = size_of::<JvmPtr>();
pub const OFFSET_FIELDS: usize = OFFSET_MONITOR + size_of::<Monitor>();
const OFFSET_ARRAY_LENGTH: usize = OFFSET_FIELDS;
const OFFSET_ARRAY_ITEMS: usize = OFFSET_ARRAY_LENGTH + size_of::<i32>();

#[derive(Clone, Copy)]
#[repr(transparent)]
pub(crate) struct Object(JvmPtrNonNull);

impl Object {
    pub(crate) fn new_uninitialized(heap: &Heap, class: ClassRef) -> Self {
        unsafe {
            let ptr = heap.alloc(Layout::from_size_align_unchecked(class.object_size, 8));
            let class_ptr = heap::ptr_encode(class as *const Class as *mut u8);
            ((ptr + OFFSET_CLASS) as *const AtomicJvmPtr)
                .as_ref_unchecked()
                .store(class_ptr, Relaxed);
            Self(heap::ptr_encode(ptr as *mut u8).try_into().unwrap())
        }
    }

    pub(crate) fn new_array(heap: &Heap, class: ClassRef, length: i32) -> Self {
        unsafe {
            let size =
                OFFSET_ARRAY_ITEMS + length as usize * class.element_type.as_ref().unwrap().size();
            let ptr = heap.alloc(Layout::from_size_align_unchecked(size, 8));
            let class_ptr = heap::ptr_encode(class as *const Class as *mut u8);
            ((ptr + OFFSET_CLASS) as *const AtomicJvmPtr)
                .as_ref_unchecked()
                .store(class_ptr, Relaxed);
            ((ptr + OFFSET_ARRAY_LENGTH) as *const AtomicI32)
                .as_ref_unchecked()
                .store(length, Relaxed);
            Self(heap::ptr_encode(ptr as *mut u8).try_into().unwrap())
        }
    }

    pub(crate) fn from_ptr(addr: JvmPtr) -> Option<Self> {
        JvmPtrNonNull::new(addr).map(Self)
    }

    #[inline]
    fn deref(self) -> *const u8 {
        heap::ptr_decode(self.0.into())
    }

    #[inline]
    pub(crate) fn class(self) -> ClassRef {
        unsafe {
            &*(heap::ptr_decode(*(self.deref().byte_add(OFFSET_CLASS) as *const JvmPtr))
                as *const Class)
        }
    }

    pub(crate) fn ptr(self) -> JvmPtrNonNull {
        self.0
    }

    pub fn fields(self) -> FieldStorage {
        FieldStorage(self.deref() as usize)
    }

    pub(crate) fn array_length(self) -> i32 {
        unsafe {
            (self.deref().byte_add(OFFSET_ARRAY_LENGTH) as *const AtomicI32)
                .as_ref_unchecked()
                .load(Relaxed)
        }
    }

    #[inline]
    fn array_item<T>(self, index: i32) -> Option<*const T> {
        if index >= self.array_length() {
            None
        } else {
            Some(
                (self
                    .deref()
                    .wrapping_byte_add(OFFSET_ARRAY_ITEMS + index as usize * size_of::<T>()))
                    as *const T,
            )
        }
    }

    pub fn array_read<T: Data>(self, jvm: &Jvm, index: i32) -> JvmResult<T> {
        if let Some(item) = self.array_item::<T>(index) {
            Ok(T::read(item, Relaxed))
        } else {
            Err(throwable(jvm, "ArrayIndexOutOfBoundsException"))
        }
    }

    pub fn array_write<T: Data>(self, jvm: &Jvm, index: i32, value: T) -> JvmResult<()> {
        if let Some(item) = self.array_item::<T>(index) {
            T::write(item, value, Relaxed);
            Ok(())
        } else {
            Err(throwable(jvm, "ArrayIndexOutOfBoundsException"))
        }
    }

    #[inline]
    pub fn is_array(self) -> bool {
        self.class().element_type.is_some()
    }

    #[track_caller]
    fn array_read_panic<T: Data>(self, index: i32) -> T {
        if let Some(item) = self.array_item::<T>(index) {
            T::read(item, Relaxed)
        } else {
            panic!("Array index out of bounds")
        }
    }

    /// Returns either the value at index
    /// # Panics
    /// if the object is not an array or the index is out of bounds
    #[track_caller]
    pub fn array_read_value(self, index: i32) -> Value {
        let Some(elem_type) = self.class().element_type.as_ref() else {
            panic!("Tried to read array item of non-array object");
        };
        match elem_type {
            Type::Boolean | Type::Byte => (self.array_read_panic::<i8>(index) as u8 as i32).into(),
            Type::Short | Type::Char => (self.array_read_panic::<i16>(index) as u16 as i32).into(),
            Type::Int => self.array_read_panic::<i32>(index).into(),
            Type::Long => self.array_read_panic::<i64>(index).into(),
            Type::Float => self.array_read_panic::<f32>(index).into(),
            Type::Double => self.array_read_panic::<f64>(index).into(),
            Type::Ref(_) => Object::from_ptr(self.array_read_panic::<JvmPtr>(index)).into(),
        }
    }

    /// Sets an array element. If the element type is boolean, byte, short or char, the `JVMValue::Int` is truncated
    /// Panics if object is not an array, the element type mismatches or the index is out of bounds
    pub fn array_write_value(self, index: i32, value: Value) {
        #[track_caller]
        fn write<T: Data>(obj: Object, index: i32, value: T) {
            if let Some(item) = obj.array_item::<T>(index) {
                T::write(item, value, Relaxed)
            } else {
                panic!("Array index out of bounds")
            }
        }
        if let Some(element_type) = &self.class().element_type {
            match (element_type, value) {
                (Type::Boolean | Type::Byte, Value::Int(value)) => {
                    write::<i8>(self, index, value as u8 as i8)
                }
                (Type::Short | Type::Char, Value::Int(value)) => {
                    write::<i16>(self, index, value as u16 as i16)
                }
                (Type::Int, Value::Int(value)) => write::<i32>(self, index, value),
                (Type::Long, Value::Long(value)) => write::<i64>(self, index, value),
                (Type::Float, Value::Float(value)) => write::<f32>(self, index, value),
                (Type::Float, Value::Double(value)) => write::<f64>(self, index, value),
                (Type::Ref(typ), Value::Ref(value)) => {
                    if let Some(obj) = value {
                        if !obj.class().assignable_to(typ) {
                            panic!("element type mismatch")
                        }
                    }
                    write::<JvmPtr>(
                        self,
                        index,
                        value.map(|o| o.ptr().into()).unwrap_or(heap::NULL_PTR),
                    )
                }
                _ => panic!("element type mismatch"),
            }
        } else {
            panic!("Tried to read array item of non-array object");
        }
    }

    pub(crate) fn monitor(self) -> &'static Monitor {
        unsafe {
            (self.deref().wrapping_byte_add(OFFSET_MONITOR) as *const Monitor).as_ref_unchecked()
        }
    }

    pub(crate) fn format_short(self) -> String {
        struct Fmt(Object);
        impl std::fmt::Debug for Fmt {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                fmt_object(self.0, f, false)
            }
        }
        format!("{:?}", Fmt(self))
    }
}

impl Eq for Object {
    fn assert_receiver_is_total_eq(&self) {}
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl std::hash::Hash for Object {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::hash::Hash::hash(&self.ptr(), state)
    }
}

fn fmt_object(object: Object, f: &mut std::fmt::Formatter<'_>, full: bool) -> std::fmt::Result {
    let class = object.class();

    // Prettier printing for strings
    if class.name.deref() == "java.lang.String" {
        let Value::Ref(chars) = class
            .field("chars", Type::Ref("char[]".into()))
            .expect("java.lang.String missing field char[] chars")
            .instance_get(object)
        else {
            unreachable!()
        };
        if let Some(chars) = chars {
            write!(f, "\"")?;
            let length = chars.array_length();
            for char in
                char::decode_utf16((0..length).map(|i| chars.array_read_panic::<i16>(i) as u16))
            {
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
            write!(f, "{}, ", object.array_read_value(i))?
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
                    write!(f, "{}", data.read::<i8>(field.byte_offset, false))?
                }
                Type::Short => write!(f, "{}", data.read::<i16>(field.byte_offset, false))?,
                Type::Char => write!(f, "u+{:x}", data.read::<i16>(field.byte_offset, false))?,
                Type::Int => write!(f, "{}", data.read::<i32>(field.byte_offset, false))?,
                Type::Long => write!(f, "{}", data.read::<i64>(field.byte_offset, false))?,
                Type::Float => write!(f, "{}", data.read::<f32>(field.byte_offset, false))?,
                Type::Double => write!(f, "{}", data.read::<f64>(field.byte_offset, false))?,
                Type::Ref(_) => {
                    let ptr = data.read::<JvmPtr>(field.byte_offset, false);
                    if let Some(obj) = Object::from_ptr(ptr) {
                        if full {
                            fmt_object(obj, f, true)?
                        } else {
                            write!(f, "@{:x}", data.read::<JvmPtr>(field.byte_offset, false))?
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

impl std::fmt::Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt_object(*self, f, true)
    }
}

impl std::fmt::Pointer for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{:x}", self.ptr())
    }
}

const _: () = assert!(std::mem::size_of::<Object>() == std::mem::size_of::<Option<Object>>());

pub trait Data {
    fn read(ptr: *const Self, ordering: Ordering) -> Self;
    fn write(ptr: *const Self, value: Self, ordering: Ordering);
}

impl Data for i8 {
    #[inline]
    fn read(ptr: *const Self, ordering: Ordering) -> Self {
        unsafe { (ptr as *const AtomicI8).as_ref_unchecked().load(ordering) }
    }

    #[inline]
    fn write(ptr: *const Self, value: Self, ordering: Ordering) {
        unsafe {
            (ptr as *const AtomicI8)
                .as_ref_unchecked()
                .store(value, ordering)
        }
    }
}

impl Data for i16 {
    #[inline]
    fn read(ptr: *const Self, ordering: Ordering) -> Self {
        unsafe { (ptr as *const AtomicI16).as_ref_unchecked().load(ordering) }
    }

    #[inline]
    fn write(ptr: *const Self, value: Self, ordering: Ordering) {
        unsafe {
            (ptr as *const AtomicI16)
                .as_ref_unchecked()
                .store(value, ordering)
        }
    }
}

impl Data for i32 {
    #[inline]
    fn read(ptr: *const Self, ordering: Ordering) -> Self {
        unsafe { (ptr as *const AtomicI32).as_ref_unchecked().load(ordering) }
    }

    #[inline]
    fn write(ptr: *const Self, value: Self, ordering: Ordering) {
        unsafe {
            (ptr as *const AtomicI32)
                .as_ref_unchecked()
                .store(value, ordering)
        }
    }
}

impl Data for i64 {
    #[inline]
    fn read(ptr: *const Self, ordering: Ordering) -> Self {
        unsafe { (ptr as *const AtomicI64).as_ref_unchecked().load(ordering) }
    }

    #[inline]
    fn write(ptr: *const Self, value: Self, ordering: Ordering) {
        unsafe {
            (ptr as *const AtomicI64)
                .as_ref_unchecked()
                .store(value, ordering)
        }
    }
}

impl Data for JvmPtr {
    #[inline]
    fn read(ptr: *const Self, ordering: Ordering) -> Self {
        unsafe {
            (ptr as *const AtomicJvmPtr)
                .as_ref_unchecked()
                .load(ordering)
        }
    }

    #[inline]
    fn write(ptr: *const Self, value: Self, ordering: Ordering) {
        unsafe {
            (ptr as *const AtomicJvmPtr)
                .as_ref_unchecked()
                .store(value, ordering)
        }
    }
}

impl Data for f32 {
    #[inline]
    fn read(ptr: *const Self, ordering: Ordering) -> Self {
        f32::from_bits(i32::read(ptr as *const i32, ordering) as u32)
    }

    #[inline]
    fn write(ptr: *const Self, value: Self, ordering: Ordering) {
        i32::write(ptr as *const i32, value.to_bits() as i32, ordering);
    }
}

impl Data for f64 {
    #[inline]
    fn read(ptr: *const Self, ordering: Ordering) -> Self {
        f64::from_bits(i64::read(ptr as *const i64, ordering) as u64)
    }

    #[inline]
    fn write(ptr: *const Self, value: Self, ordering: Ordering) {
        i64::write(ptr as *const i64, value.to_bits() as i64, ordering);
    }
}

#[derive(Clone, Copy)]
pub(crate) struct FieldStorage(pub usize);

impl FieldStorage {
    pub const EMPTY: FieldStorage = Self(0);

    #[inline]
    pub(crate) fn read<T: Data>(self, offset: usize, volatile: bool) -> T {
        T::read(
            (self.0 as *const T).wrapping_byte_add(offset),
            if volatile { Acquire } else { Relaxed },
        )
    }

    #[inline]
    pub(crate) fn write<T: Data>(self, offset: usize, value: T, volatile: bool) {
        T::write(
            (self.0 as *const T).wrapping_byte_add(offset),
            value,
            if volatile { Release } else { Relaxed },
        )
    }
}
