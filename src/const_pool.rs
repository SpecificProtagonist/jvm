use crate::{FieldRef, IntStr, MethodRef};
use anyhow::{bail, Result};

// Differentiating runtime- and on-disk const pool shouldn't be neccessary
// although it would allow a speedup
pub(crate) struct ConstPool<'a>(pub Vec<ConstPoolItem<'a>>);

#[derive(Debug)]
pub(crate) enum ConstPoolItem<'a> {
    Utf8(IntStr<'a>),
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    Class(u16),
    FieldRef(FieldRef<'a>),
    MethodRef(MethodRef<'a>),
    InterfaceMethodRef { class: u16, nat: u16 },
    // Accoring to the spec, long and doubles taking two entries was a design mistake
    PlaceholderAfterLongOrDoubleEntryOrForEntryZero,
    // These don't end up in the run-time constant pool,
    // but are needed for parsing because there's no
    // guarantee that the referenced items come before the references
    RawNameAndType { name: u16, descriptor: u16 },
    RawFieldRef { class: u16, nat: u16 },
    RawMethodRef { class: u16, nat: u16 },
    RawString(u16),
}

impl<'a> ConstPool<'a> {
    pub fn get_utf8(&self, index: u16) -> Result<IntStr<'a>> {
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

    pub fn get_class(&self, index: u16) -> Result<IntStr<'a>> {
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

    pub fn get_raw_nat(&self, index: u16) -> Result<(IntStr<'a>, IntStr<'a>)> {
        if let Some(ConstPoolItem::RawNameAndType { name, descriptor }) = self.0.get(index as usize)
        {
            Ok((self.get_utf8(*name)?, self.get_utf8(*descriptor)?))
        } else {
            bail!("Invalid class constant pool item index")
        }
    }
}
