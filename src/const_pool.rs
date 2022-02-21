use crate::{class::Field, AccessFlags, IntStr, Method, MethodNaT, RefType, JVM};
use anyhow::{anyhow, bail, Result};
use std::cell::Cell;

// Differentiating runtime- and on-disk const pool shouldn't be neccessary
// although it would allow a speedup
#[derive(Debug)]
pub(crate) struct ConstPool<'a> {
    pub items: Vec<Cell<ConstPoolItem<'a>>>,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum ConstPoolItem<'a> {
    Utf8(IntStr<'a>),
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    Class(u16),
    Field(&'a Field<'a>),
    StaticMethod(&'a Method<'a>),
    // Class is neccessary for invoke_special
    VirtualMethod(&'a RefType<'a>, MethodNaT<'a>),
    InterfaceMethodRef { class: u16, nat: u16 },
    NameAndType { name: u16, descriptor: u16 },
    FieldRef { class: u16, nat: u16 },
    MethodRef { class: u16, nat: u16 },
    RawString(u16),
    // Accoring to the spec, long and doubles taking two entries was a design mistake
    PlaceholderAfterLongOrDoubleEntryOrForEntryZero,
}

impl<'a> ConstPool<'a> {
    pub fn resolve(&self, jvm: &JVM<'a>) -> Result<()> {
        for item in &self.items {
            match item.get() {
                ConstPoolItem::MethodRef { class, nat } => {
                    let mut class = self.get_class(class)?;
                    // Arrays inherit methods from Object
                    if class.0.starts_with('[') {
                        class = jvm.intern_str("java/lang/Object");
                    }
                    let class = jvm.resolve_class(class)?;
                    let (name, descriptor) = self.get_raw_nat(nat)?;
                    let typ = crate::parse::parse_method_descriptor(jvm, descriptor)?;
                    let typ = jvm.method_descriptor_storage.lock().unwrap().alloc(typ);
                    let method = class
                        .method(name, typ)
                        .ok_or_else(|| anyhow!("Method not found"))?;
                    if method.access_flags.contains(AccessFlags::STATIC) {
                        item.set(ConstPoolItem::StaticMethod(method));
                    } else {
                        // TODO: check if returning java/lang/Object for arrays is correct for invokespecial
                        item.set(ConstPoolItem::VirtualMethod(class, MethodNaT { name, typ }));
                    }
                }
                ConstPoolItem::FieldRef { class, nat } => {
                    let class = match jvm.resolve_class(self.get_class(class)?)? {
                        RefType::Class(class) => class,
                        RefType::Array { .. } => bail!("Tried to get field of array"),
                    };
                    let (name, typ) = self.get_raw_nat(nat)?;
                    let (typ, _) = crate::parse::parse_field_descriptor(jvm, typ, 0)?;
                    if let Some(field) = class.field(name, typ) {
                        item.set(ConstPoolItem::Field(field));
                    } else {
                        bail!(
                            "Class {} does not have field {} of correct type",
                            class.name,
                            name
                        );
                    }
                }
                _ => (),
            }
        }
        Ok(())
    }

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

    pub fn get_static_method<'b>(&'b self, jvm: &'b JVM<'a>, index: u16) -> Result<&'a Method<'a>> {
        match self.items.get(index as usize).map(Cell::get) {
            Some(ConstPoolItem::StaticMethod(method)) => Ok(method),
            other => bail!(
                "Constant pool index {} not a static method (found {:?})",
                index,
                other
            ),
        }
    }

    pub fn get_virtual_method<'b>(
        &'b self,
        jvm: &'b JVM<'a>,
        index: u16,
    ) -> Result<(&'a RefType<'a>, MethodNaT<'a>)> {
        match self.items.get(index as usize).map(Cell::get) {
            Some(ConstPoolItem::VirtualMethod(class, nat)) => Ok((class, nat)),
            Some(ConstPoolItem::MethodRef { class, nat }) => {
                let class = jvm.resolve_class(self.get_class(class)?)?;

                let (name, descriptor) = self.get_raw_nat(nat)?;
                let typ = crate::parse::parse_method_descriptor(jvm, descriptor)?;
                let typ = jvm.method_descriptor_storage.lock().unwrap().alloc(typ);
                let nat = MethodNaT { name, typ };
                let method = class
                    .method(name, typ)
                    .ok_or_else(|| anyhow!("Method not found"))?;
                if method.access_flags.contains(AccessFlags::STATIC) {
                    bail!("Method {} is static", nat)
                }
                // TODO: check if returning java/lang/Object for arrays is correct for invokespecial
                self.items[index as usize].set(ConstPoolItem::VirtualMethod(class, nat));
                Ok((class, nat))
            }
            _ => bail!("Constant pool index {} not a method", index,),
        }
    }

    pub fn get_field(&self, jvm: &JVM<'a>, index: u16) -> Result<&'a Field<'a>> {
        match self.items.get(index as usize).map(Cell::get) {
            Some(ConstPoolItem::Field(field)) => Ok(field),
            other => bail!(
                "Constant pool index {index} not a field (found {:?})",
                other
            ),
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
