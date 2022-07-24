use std::{
    marker::PhantomData,
    mem::{align_of, size_of},
};

use crate::{
    field_storage::FieldStorage,
    heap::{self, JVMPtr, JVMPtrNonNull},
    AccessFlags, Class, JVMValue, Typ,
};

impl<'a> PartialEq for Object<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr.ptr() == other.ptr.ptr()
    }
}

// TODO: Maybe omit size in case of non-array objects
/// Object layout:
/// size (u64, handled by FieldStorage)
/// JVMPtrSize to RefType
/// fields (sorted by alignment) | array elements
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Object<'a> {
    pub(crate) ptr: FieldStorage,
    /// The object may only live as long as the JVM
    pub(crate) _marker: PhantomData<&'a ()>,
}

impl<'a> Object<'a> {
    pub fn class(self) -> &'a Class<'a> {
        unsafe { &*(heap::ptr_decode(self.ptr.read_ptr(0, false)) as *const Class) }
    }

    pub fn ptr(self) -> JVMPtrNonNull {
        self.ptr.ptr()
    }

    /// SAFETY: Must be either null, or a valid pointer to an object
    pub(crate) unsafe fn from_ptr(addr: JVMPtr) -> Option<Self> {
        JVMPtrNonNull::new(addr).map(|addr| Self {
            ptr: FieldStorage::from_ptr(addr),
            _marker: Default::default(),
        })
    }

    pub fn array_len(self) -> Option<i32> {
        self.class().element_type.map(|base| {
            ((self.ptr.size() - crate::object::header_size()) / base.layout().size()) as i32
        })
    }

    /// Returns either the value at index
    /// # Panics
    /// if the object is not an array or the index is out of bounds
    pub fn array_read(self, index: i32) -> JVMValue<'a> {
        match self.class().element_type {
            None => panic!("not an array"),
            Some(Typ::Bool) | Some(Typ::Byte) => {
                JVMValue::Int(self.ptr.array_read_i8_freestanding(index) as u8 as i32)
            }
            Some(Typ::Short) | Some(Typ::Char) => {
                JVMValue::Int(self.ptr.array_read_i16_freestanding(index) as u16 as i32)
            }
            Some(Typ::Int) => JVMValue::Int(self.ptr.array_read_i32_freestanding(index)),
            Some(Typ::Long) => JVMValue::Long(self.ptr.array_read_i64_freestanding(index)),
            Some(Typ::Float) => JVMValue::Float(self.ptr.array_read_f32_freestanding(index)),
            Some(Typ::Double) => JVMValue::Double(self.ptr.array_read_f64_freestanding(index)),
            Some(Typ::Ref(_)) => JVMValue::Ref(unsafe {
                Object::from_ptr(self.ptr.array_read_ptr_freestanding(index))
            }),
        }
    }

    // TODO: array_write
}

impl<'a> std::fmt::Debug for Object<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let class = self.class();

        // Arrays
        if let Some(base) = class.element_type {
            let length = self.array_len().unwrap();
            // Prettier printing for strings
            if base == Typ::Char {
                write!(f, "\"")?;
                for char in char::decode_utf16(
                    (0..length).map(|i| self.ptr.array_read_i16_freestanding(i) as u16),
                ) {
                    write!(f, "{}", char.unwrap_or(char::REPLACEMENT_CHARACTER))?
                }
                return write!(f, "\"");
            } else {
                write!(f, "{}[", base)?;
                for i in 0..length {
                    write!(f, "{}, ", self.array_read(i))?
                }
                return write!(f, "]");
            }
        }

        // Ordinary objects
        write!(f, "{}{{", class.name)?;
        let mut first = true;
        for field in class.fields.values() {
            if !field.access_flags.contains(AccessFlags::STATIC) {
                if !first {
                    write!(f, ", ")?;
                }
                first = false;
                write!(f, "{}: ", field.nat.name)?;
                unsafe {
                    match field.nat.typ {
                        Typ::Bool | Typ::Byte => {
                            write!(f, "{}", self.ptr.read_i8(field.byte_offset, false))?
                        }
                        Typ::Short => write!(f, "{}", self.ptr.read_i16(field.byte_offset, false))?,
                        Typ::Char => {
                            write!(f, "u+{:x}", self.ptr.read_i16(field.byte_offset, false))?
                        }
                        Typ::Int => write!(f, "{}", self.ptr.read_i32(field.byte_offset, false))?,
                        Typ::Long => write!(f, "{}", self.ptr.read_i64(field.byte_offset, false))?,
                        Typ::Float => write!(f, "{}", self.ptr.read_f32(field.byte_offset, false))?,
                        Typ::Double => {
                            write!(f, "{}", self.ptr.read_f64(field.byte_offset, false))?
                        }
                        Typ::Ref(_) => {
                            write!(f, "@{:x}", self.ptr.read_ptr(field.byte_offset, false))?
                        }
                    }
                }
            }
        }
        write!(f, "}}")?;
        Ok(())
    }
}

/// Size of an object with no fields/an empty array, not counting FieldData's length header
pub fn header_size() -> usize {
    // Afaik the alignment of 64-bit ints is usize even on 32-bit systems,
    // but better make sure of this (as there would be unaligned accesses elsewise):
    size_of::<JVMPtr>().max(align_of::<u64>())
}
