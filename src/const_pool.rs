use crate::{FieldRef, Id, MethodRef};
use anyhow::{anyhow, bail, Context, Result};

// Differentiating runtime- and on-disk const pool shouldn't be neccessary
// although it would allow a speedup
pub struct ConstPool(pub Vec<ConstPoolItem>);

#[derive(Debug)]
pub enum ConstPoolItem {
    Utf8(Id<String>),
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    Class(u16),
    FieldRef(FieldRef),
    MethodRef(MethodRef),
    InterfaceMethodRef { class: u16, nat: u16 },
    PlaceholderAfterLongOrDoubleEntryOrForEntryZero,
    // These don't end up in the run-time constant pool,
    // but are needed for parsing because there's no
    // guarantee that the referenced items come first
    RawNameAndType { name: u16, descriptor: u16 },
    RawFieldRef { class: u16, nat: u16 },
    RawMethodRef { class: u16, nat: u16 },
    RawString(u16),
}

impl ConstPool {
    pub fn get_utf8(&self, index: u16) -> Result<Id<String>> {
        let item = self.0.get(index as usize);
        if let Some(ConstPoolItem::Utf8(string)) = item {
            Ok(*string)
        } else {
            bail!(
                "Constant pool index {}: Expected Utf8, got {:?}",
                index,
                item
            )
        }
    }

    pub fn get_class(&self, index: u16) -> Result<Id<String>> {
        let item = self.0.get(index as usize);
        if let Some(ConstPoolItem::Class(index)) = item {
            self.get_utf8(*index)
        } else {
            bail!(
                "Constant pool index {}: Expected Class, got {:?}",
                index,
                item
            )
        }
    }

    pub fn get_method(&self, index: u16) -> Result<&MethodRef> {
        let item = self.0.get(index as usize);
        if let Some(ConstPoolItem::MethodRef(method)) = item {
            Ok(method)
        } else {
            bail!(
                "Constant pool index {}: Expected Method, got {:?}",
                index,
                item
            )
        }
    }

    pub fn get_field(&self, index: u16) -> Result<&FieldRef> {
        let item = self.0.get(index as usize);
        if let Some(ConstPoolItem::FieldRef(field)) = item {
            Ok(field)
        } else {
            bail!(
                "Constant pool index {}: Expected Field, got {:?}",
                index,
                item
            )
        }
    }

    pub fn get_raw_nat(&self, index: u16) -> Result<(Id<String>, Id<String>)> {
        if let Some(ConstPoolItem::RawNameAndType { name, descriptor }) = self.0.get(index as usize)
        {
            Ok((self.get_utf8(*name)?, self.get_utf8(*descriptor)?))
        } else {
            bail!("Invalid class constant pool item index")
        }
    }
}
