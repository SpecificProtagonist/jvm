use std::{
    mem::{align_of, size_of},
    ops::Deref,
};

use crate::{
    class::{Class, ClassPtr},
    field_storage::FieldStorage,
    heap::{self, JvmPtr, JvmPtrNonNull},
    typ::Type,
    value::Value,
    AccessFlags,
};

// TODO: Maybe omit size in case of non-array objects? (What about deallocation?)
// TODO: These are handed out to the user. When garbage collection gets implemented, only hand them out in a non-copy wrapper.
// Object layout:
// size (u64, handled by FieldStorage)
// JvmPtrSize to RefType
// fields (sorted by alignment) | array elements
#[derive(Clone, Copy)]
#[repr(transparent)]
pub(crate) struct Object {
    pub(crate) ptr: FieldStorage,
}

impl Object {
    #[inline]
    pub fn class(self) -> ClassPtr {
        unsafe { &*(heap::ptr_decode(self.ptr.read_ptr(0, false)) as *const Class) }
    }

    pub fn ptr(self) -> JvmPtrNonNull {
        self.ptr.ptr()
    }

    /// SAFETY: Must be either null, or a valid pointer to an object
    pub(crate) unsafe fn from_ptr(addr: JvmPtr) -> Option<Self> {
        JvmPtrNonNull::new(addr).map(|addr| Self {
            ptr: FieldStorage::from_ptr(addr),
        })
    }

    pub fn is_array(self) -> bool {
        self.class().element_type.is_some()
    }

    pub fn array_len(self) -> Option<i32> {
        self.class().element_type.as_ref().map(|base| {
            ((self.ptr.size() - crate::object::header_size()) / base.layout().size()) as i32
        })
    }

    /// Returns either the value at index
    /// # Panics
    /// if the object is not an array or the index is out of bounds
    pub fn array_read(self, index: i32) -> Value {
        match self.class().element_type {
            None => panic!("Tried to read array item of non-array object"),
            Some(Type::Boolean) | Some(Type::Byte) => {
                (self.ptr.array_read_i8_freestanding(index) as u8 as i32).into()
            }
            Some(Type::Short) | Some(Type::Char) => {
                (self.ptr.array_read_i16_freestanding(index) as u16 as i32).into()
            }
            Some(Type::Int) => self.ptr.array_read_i32_freestanding(index).into(),
            Some(Type::Long) => self.ptr.array_read_i64_freestanding(index).into(),
            Some(Type::Float) => self.ptr.array_read_f32_freestanding(index).into(),
            Some(Type::Double) => self.ptr.array_read_f64_freestanding(index).into(),
            Some(Type::Ref(_)) => unsafe {
                Object::from_ptr(self.ptr.array_read_ptr_freestanding(index)).into()
            },
        }
    }

    /// Sets an array element. If the element type is boolean, byte, short or char, the `JVMValue::Int` is truncated
    /// Panics if object is not an array, the element type mismatches or the index is out of bounds
    pub fn array_write(self, index: i32, value: Value) {
        if let Some(element_type) = &self.class().element_type {
            match (element_type, value) {
                (Type::Boolean | Type::Byte, Value::Int(value)) => self
                    .ptr
                    .array_write_i8_freestanding(index, value as u8 as i8),
                (Type::Short | Type::Char, Value::Int(value)) => self
                    .ptr
                    .array_write_i16_freestanding(index, value as u16 as i16),
                (Type::Int, Value::Int(value)) => {
                    self.ptr.array_write_i32_freestanding(index, value)
                }
                (Type::Long, Value::Long(value)) => {
                    self.ptr.array_write_i64_freestanding(index, value)
                }
                (Type::Float, Value::Float(value)) => {
                    self.ptr.array_write_f32_freestanding(index, value)
                }
                (Type::Float, Value::Double(value)) => {
                    self.ptr.array_write_f64_freestanding(index, value)
                }
                (Type::Ref(typ), Value::Ref(value)) => {
                    if let Some(obj) = value {
                        if !obj.class().assignable_to(typ) {
                            panic!("element type mismatch")
                        }
                    }
                    self.ptr.array_write_ptr_freestanding(
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
        self.ptr.ptr() == other.ptr.ptr()
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
            let length = chars.array_len().unwrap();
            write!(f, "\"")?;
            for char in char::decode_utf16(
                (0..length).map(|i| chars.ptr.array_read_i16_freestanding(i) as u16),
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
        let length = object.array_len().unwrap();
        write!(f, "{}[", base)?;
        for i in 0..length {
            write!(f, "{}, ", object.array_read(i))?
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
            unsafe {
                match field.nat.typ {
                    Type::Boolean | Type::Byte => {
                        write!(f, "{}", object.ptr.read_i8(field.byte_offset, false))?
                    }
                    Type::Short => write!(f, "{}", object.ptr.read_i16(field.byte_offset, false))?,
                    Type::Char => {
                        write!(f, "u+{:x}", object.ptr.read_i16(field.byte_offset, false))?
                    }
                    Type::Int => write!(f, "{}", object.ptr.read_i32(field.byte_offset, false))?,
                    Type::Long => write!(f, "{}", object.ptr.read_i64(field.byte_offset, false))?,
                    Type::Float => write!(f, "{}", object.ptr.read_f32(field.byte_offset, false))?,
                    Type::Double => write!(f, "{}", object.ptr.read_f64(field.byte_offset, false))?,
                    Type::Ref(_) => {
                        let ptr = object.ptr.read_ptr(field.byte_offset, false);
                        if let Some(obj) = Object::from_ptr(ptr) {
                            if full {
                                fmt_object(obj, f, true)?
                            } else {
                                write!(f, "@{:x}", object.ptr.read_ptr(field.byte_offset, false))?
                            }
                        } else {
                            write!(f, "null")?
                        }
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

/// Size of an object with no fields/an empty array, not counting FieldData's length header
pub fn header_size() -> usize {
    // Afaik the alignment of 64-bit ints is usize even on 32-bit systems,
    // but better make sure of this (as there would be unaligned accesses elsewise):
    size_of::<JvmPtr>().max(align_of::<u64>())
}

#[test]
fn object_reference_niche() {
    assert_eq!(
        std::mem::size_of::<Object>(),
        std::mem::size_of::<Option<Object>>()
    )
}
