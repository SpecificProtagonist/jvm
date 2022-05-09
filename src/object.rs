use std::{
    marker::PhantomData,
    mem::{align_of, size_of},
};

use crate::{
    field_storage::FieldStorage,
    heap::{self, JVMPtrSize},
    AccessFlags, Class, Typ,
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
    pub fn null(self) -> bool {
        self.ptr.ptr() == 0
    }

    pub fn class(self) -> &'a Class<'a> {
        unsafe { &*(heap::ptr_decode(self.ptr.read_ptr(0).unwrap()) as *const Class) }
    }

    pub fn ptr(self) -> JVMPtrSize {
        self.ptr.ptr()
    }

    pub(crate) unsafe fn from_ptr(addr: JVMPtrSize) -> Self {
        Self {
            ptr: FieldStorage(addr),
            _marker: Default::default(),
        }
    }
}

impl<'a> std::fmt::Debug for Object<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let class = self.class();
        match class.element_type {
            Some(base) => {
                write!(f, "{}[]{{…}}", base)?;
            }
            None => {
                write!(f, "{}{{", class.name)?;
                let mut first = true;
                for field in class.fields.values() {
                    if !field.access_flags.contains(AccessFlags::STATIC) {
                        if !first {
                            write!(f, ", ")?;
                        }
                        first = false;
                        write!(f, "{}: ", field.name)?;
                        match field.descriptor {
                            Typ::Bool | Typ::Byte => {
                                write!(f, "{}", self.ptr.read_i8(field.byte_offset).unwrap())?
                            }
                            Typ::Short => {
                                write!(f, "{}", self.ptr.read_i16(field.byte_offset).unwrap())?
                            }
                            Typ::Char => {
                                write!(f, "u+{:x}", self.ptr.read_i16(field.byte_offset).unwrap())?
                            }
                            Typ::Int => {
                                write!(f, "{}", self.ptr.read_i32(field.byte_offset).unwrap())?
                            }
                            Typ::Long => {
                                write!(f, "{}", self.ptr.read_i64(field.byte_offset).unwrap())?
                            }
                            Typ::Float => {
                                write!(f, "{}", self.ptr.read_f32(field.byte_offset).unwrap())?
                            }
                            Typ::Double => {
                                write!(f, "{}", self.ptr.read_f64(field.byte_offset).unwrap())?
                            }
                            Typ::Ref(_) => write!(f, "obj")?,
                        }
                    }
                }
                write!(f, "}}")?;
            }
        }
        Ok(())
    }
}

/// Size of an object with no fields/an empty array, not counting FieldData's length header
pub fn header_size() -> usize {
    // Afaik the alignment of 64-bit ints is usize even on 32-bit systems,
    // but better make sure of this (as there would be unaligned accesses elsewise):
    size_of::<JVMPtrSize>().max(align_of::<u64>())
}
