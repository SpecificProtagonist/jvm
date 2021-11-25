use std::{marker::PhantomData, mem::size_of};

use crate::{field_storage::FieldStorage, AccessFlags, RefType, Typ};

impl<'a> PartialEq for Object<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.data.addr() == other.data.addr()
    }
}

/// contains the ClassOrArray, followed by the fields/items
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Object<'a> {
    pub(crate) data: FieldStorage,
    pub(crate) _marker: PhantomData<&'a ()>,
}

impl<'a> Object<'a> {
    pub fn null(self) -> bool {
        self.data.addr() == 0
    }

    pub fn class(self) -> &'a RefType<'a> {
        unsafe { std::mem::transmute(self.data.read_usize(0).unwrap()) }
    }
}

impl<'a> std::fmt::Debug for Object<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.class() {
            RefType::Array { base, .. } => {
                write!(f, "{}[]{{…}}", base)?;
            }
            RefType::Class(class) => {
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
                                write!(f, "{}", self.data.read_i8(field.byte_offset).unwrap())?
                            }
                            Typ::Short => {
                                write!(f, "{}", self.data.read_i16(field.byte_offset).unwrap())?
                            }
                            Typ::Char => {
                                write!(f, "u+{:x}", self.data.read_i16(field.byte_offset).unwrap())?
                            }
                            Typ::Int => {
                                write!(f, "{}", self.data.read_i32(field.byte_offset).unwrap())?
                            }
                            Typ::Long => {
                                write!(f, "{}", self.data.read_i64(field.byte_offset).unwrap())?
                            }
                            Typ::Float => {
                                write!(f, "{}", self.data.read_f32(field.byte_offset).unwrap())?
                            }
                            Typ::Double => {
                                write!(f, "{}", self.data.read_f64(field.byte_offset).unwrap())?
                            }
                            Typ::Ref(_) => write!(f, "obj")?,
                        }
                    }
                }
            }
        }
        Ok(())
    }
}

pub fn min_object_size() -> usize {
    size_of::<&RefType>()
}
