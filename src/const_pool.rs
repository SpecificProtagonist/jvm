use std::cell::Cell;

use crate::{
    class::{Class, Field, MethodDescriptor, MethodNaT},
    IntStr, JVM,
};
use anyhow::{bail, Result};

// Differentiating runtime- and on-disk const pool shouldn't be neccessary
// although it would allow a speedup
pub(crate) struct ConstPool<'a> {
    pub items: Vec<Cell<ConstPoolItem<'a>>>,
}

#[derive(Clone, Copy)]
pub(crate) enum ConstPoolItem<'a> {
    Utf8(IntStr<'a>),
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    Class(u16),
    Field(Field<'a>),
    Method {
        class: &'a Class<'a>,
        nat: MethodNaT<'a>,
    },
    InterfaceMethodRef {
        class: u16,
        nat: u16,
    },
    NameAndType {
        name: u16,
        descriptor: u16,
    },
    FieldRef {
        class: u16,
        nat: u16,
    },
    MethodRef {
        class: u16,
        nat: u16,
    },
    RawString(u16),
    // Accoring to the spec, long and doubles taking two entries was a design mistake
    PlaceholderAfterLongOrDoubleEntryOrForEntryZero,
}

impl<'a> ConstPool<'a> {
    pub fn get_int(&self, index: u16) -> Result<i32> {
        match self.items.get(index as usize).map(Cell::get) {
            Some(ConstPoolItem::Integer(int)) => Ok(int),
            _ => bail!("Constant pool index {} not Integer", index),
        }
    }

    pub fn get_long(&self, index: u16) -> Result<i64> {
        match self.items.get(index as usize).map(Cell::get) {
            Some(ConstPoolItem::Long(long)) => Ok(long),
            _ => bail!("Constant pool index {} not Long", index),
        }
    }

    pub fn get_float(&self, index: u16) -> Result<f32> {
        match self.items.get(index as usize).map(Cell::get) {
            Some(ConstPoolItem::Float(float)) => Ok(float),
            _ => bail!("Constant pool index {} not Float", index),
        }
    }

    pub fn get_double(&self, index: u16) -> Result<f64> {
        match self.items.get(index as usize).map(Cell::get) {
            Some(ConstPoolItem::Double(double)) => Ok(double),
            _ => bail!("Constant pool index {} not Double", index),
        }
    }

    pub fn get_utf8(&self, index: u16) -> Result<IntStr<'a>> {
        match self.items.get(index as usize).map(Cell::get) {
            Some(ConstPoolItem::Utf8(string)) => Ok(string),
            _ => bail!("Constant pool index {} not Utf8", index),
        }
    }

    pub fn get_class(&self, index: u16) -> Result<IntStr<'a>> {
        match self.items.get(index as usize).map(Cell::get) {
            Some(ConstPoolItem::Class(index)) => self.get_utf8(index),
            _ => bail!("Constant pool index {} not Class", index),
        }
    }

    pub fn get_method<'b>(
        &'b self,
        jvm: &'b JVM<'a>,
        index: u16,
    ) -> Result<(&'a Class<'a>, MethodNaT<'a>)> {
        match self.items.get(index as usize).map(Cell::get) {
            Some(ConstPoolItem::Method { class, nat }) => Ok((class, nat)),
            Some(ConstPoolItem::MethodRef { class, nat }) => {
                let class = jvm.resolve_class(self.get_class(class)?)?;
                let (name, descriptor) = self.get_raw_nat(nat)?;
                let typ = crate::parse::parse_method_descriptor(jvm, descriptor)?;
                // SAFETY: elements of the arena are at the same pos as long as the arena exists
                let typ = unsafe {
                    &*(jvm.method_descriptor_storage.alloc(typ) as *const MethodDescriptor)
                };
                let nat = MethodNaT { name, typ };
                self.items[index as usize].set(ConstPoolItem::Method { class, nat });
                Ok((class, nat))
            }
            _ => bail!("Constant pool index {} not a method", index,),
        }
    }

    pub fn get_field(&self, jvm: &JVM<'a>, index: u16) -> Result<Field<'a>> {
        match self.items.get(index as usize).map(Cell::get) {
            Some(ConstPoolItem::Field(field)) => Ok(field),
            Some(ConstPoolItem::FieldRef { class, nat }) => {
                let class = jvm.resolve_class(self.get_class(class)?)?;
                let (name, typ) = self.get_raw_nat(nat)?;
                let (typ, _) = crate::parse::parse_field_descriptor(jvm, typ, 0)?;
                if let Some(field) = class.field(name, typ) {
                    self.items[index as usize].set(ConstPoolItem::Field(field));
                    Ok(field)
                } else {
                    bail!(
                        "Class {} does not have field {} of correct type",
                        class.name,
                        name
                    );
                }
            }
            _ => bail!("Constant pool index {} not a field", index,),
        }
    }

    fn get_raw_nat(&self, index: u16) -> Result<(IntStr<'a>, IntStr<'a>)> {
        match self.items.get(index as usize).map(Cell::get) {
            Some(ConstPoolItem::NameAndType { name, descriptor }) => {
                Ok((self.get_utf8(name)?, self.get_utf8(descriptor)?))
            }
            _ => bail!("Invalid class constant pool item index"),
        }
    }
}
