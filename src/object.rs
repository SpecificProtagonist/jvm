use std::alloc::Layout;

use crate::{class::Class, field_storage::FieldStorage, AccessFlags, Typ};

pub type Object = &'static ObjectData;

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        *self as *const ObjectData == *other as *const ObjectData
    }
}

pub struct ObjectData {
    pub data: FieldStorage,
}

impl ObjectData {
    // TODO: arrays
    pub(crate) fn class(&self) -> &Class {
        unsafe { std::mem::transmute(self.data.read_usize(0)) }
    }
}

impl std::fmt::Debug for ObjectData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{{", self.class().name)?;
        let mut first = true;
        for field in self.class().fields.values() {
            if !field.access_flags.contains(AccessFlags::STATIC) {
                if !first {
                    write!(f, ", ")?;
                }
                first = false;
                write!(f, "{}: ", field.name)?;
                // SAFETY: field.byte_offset has guaranteed correct alignment for type
                unsafe {
                    match field.descriptor {
                        Typ::Bool | Typ::Byte => {
                            write!(f, "{}", self.data.read_u8(field.byte_offset) as i8)?
                        }
                        Typ::Short => {
                            write!(f, "{}", self.data.read_u16(field.byte_offset) as i16)?
                        }
                        Typ::Char => {
                            write!(f, "u+{:x}", self.data.read_u16(field.byte_offset) as i16)?
                        }
                        Typ::Int => write!(f, "{}", self.data.read_u32(field.byte_offset) as i32)?,
                        Typ::Long => write!(f, "{}", self.data.read_u64(field.byte_offset) as i64)?,
                        Typ::Float => write!(
                            f,
                            "{}",
                            f32::from_bits(self.data.read_u32(field.byte_offset))
                        )?,
                        Typ::Double => write!(
                            f,
                            "{}",
                            f64::from_bits(self.data.read_u64(field.byte_offset))
                        )?,
                        Typ::Class(_) => write!(f, "obj")?,
                        Typ::Array { dimensions, .. } => write!(f, "array[{}]", dimensions)?,
                    }
                }
            }
        }
        Ok(())
    }
}

pub fn min_object_layout() -> Layout {
    Layout::new::<&Class>()
}
