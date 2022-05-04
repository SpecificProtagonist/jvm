use crate::{class::Field, AccessFlags, Class, IntStr, Method, MethodNaT, JVM};
use anyhow::{anyhow, bail, Result};

// Differentiating runtime- and on-disk const pool shouldn't be neccessary
// although it would allow a speedup
#[derive(Debug, Default)]
pub(crate) struct ConstPool<'a> {
    pub items: Vec<ConstPoolItem<'a>>,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum ConstPoolItem<'a> {
    Utf8(IntStr<'a>),
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    Class(&'a Class<'a>),
    Field(&'a Field<'a>),
    StaticMethod(&'a Method<'a>),
    // Class is neccessary for invoke_special
    VirtualMethod(&'a Class<'a>, MethodNaT<'a>),
    InterfaceMethodRef { class: u16, nat: u16 },
    NameAndType { name: u16, descriptor: u16 },
    FieldRef { class: u16, nat: u16 },
    MethodRef { class: u16, nat: u16 },
    RawString(u16),
    RawClass(u16),
    // Accoring to the spec, long and doubles taking two entries was a design mistake
    PlaceholderAfterLongOrDoubleEntryOrForEntryZero,
}

impl<'a> ConstPool<'a> {
    /// Must be called prior to initialization
    pub fn resolve(&mut self, jvm: &JVM<'a>) -> Result<()> {
        for i in 0..self.items.len() {
            if let ConstPoolItem::RawClass(index) = self.items[i] {
                self.items[i] = ConstPoolItem::Class(jvm.resolve_class(self.get_utf8(index)?)?);
            }
        }
        for i in 0..self.items.len() {
            match self.items[i] {
                ConstPoolItem::MethodRef { class, nat } => {
                    // Arrays inherit methods from Object
                    let class = self.get_class(class)?;
                    let (name, descriptor) = self.get_raw_nat(nat)?;
                    let typ = crate::parse::parse_method_descriptor(jvm, descriptor)?;
                    let typ = jvm.method_descriptor_storage.lock().alloc(typ);
                    let method = class
                        .methods
                        .get(&MethodNaT { name, typ })
                        .ok_or_else(|| anyhow!("Method not found"))?;
                    if method.access_flags.contains(AccessFlags::STATIC) {
                        self.items[i] = ConstPoolItem::StaticMethod(method);
                    } else {
                        // TODO: check if returning java/lang/Object for arrays is correct for invokespecial
                        self.items[i] =
                            ConstPoolItem::VirtualMethod(class, MethodNaT { name, typ });
                    }
                }
                ConstPoolItem::FieldRef { class, nat } => {
                    let class = self.get_class(class)?;
                    let (name, typ) = self.get_raw_nat(nat)?;
                    let (typ, _) = crate::parse::parse_field_descriptor(jvm, typ, 0)?;
                    if let Some(field) = class.field(name, typ) {
                        self.items[i] = ConstPoolItem::Field(field);
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
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Integer(int)) => Ok(*int),
            _ => bail!("Constant pool index {} not Integer", index),
        }
    }

    pub fn get_long(&self, index: u16) -> Result<i64> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Long(long)) => Ok(*long),
            _ => bail!("Constant pool index {} not Long", index),
        }
    }

    pub fn get_float(&self, index: u16) -> Result<f32> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Float(float)) => Ok(*float),
            _ => bail!("Constant pool index {} not Float", index),
        }
    }

    pub fn get_double(&self, index: u16) -> Result<f64> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Double(double)) => Ok(*double),
            _ => bail!("Constant pool index {} not Double", index),
        }
    }

    pub fn get_utf8(&self, index: u16) -> Result<IntStr<'a>> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Utf8(string)) => Ok(*string),
            _ => bail!("Constant pool index {} not Utf8", index),
        }
    }

    /// Cannot be called after resolution
    pub fn get_unresolved_class(&self, index: u16) -> Result<IntStr<'a>> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::RawClass(index)) => self.get_utf8(*index),
            _ => bail!("Constant pool index {} not Class", index),
        }
    }

    /// Requires resolution first
    pub fn get_class(&self, index: u16) -> Result<&'a Class<'a>> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Class(class)) => Ok(class),
            _ => bail!("Constant pool index {} not Class", index),
        }
    }

    /// Requires resolution first
    pub fn get_static_method<'b>(&'b self, index: u16) -> Result<&'a Method<'a>> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::StaticMethod(method)) => Ok(method),
            other => bail!(
                "Constant pool index {} not a static method (found {:?})",
                index,
                other
            ),
        }
    }

    /// Requires resolution first
    pub fn get_virtual_method(&self, index: u16) -> Result<(&'a Class<'a>, MethodNaT<'a>)> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::VirtualMethod(class, nat)) => Ok((class, *nat)),
            _ => bail!("Constant pool index {} not a virtual method", index,),
        }
    }

    /// Requires resolution first
    pub fn get_field(&self, index: u16) -> Result<&'a Field<'a>> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Field(field)) => Ok(field),
            other => bail!(
                "Constant pool index {index} not a field (found {:?})",
                other
            ),
        }
    }

    fn get_raw_nat(&self, index: u16) -> Result<(IntStr<'a>, IntStr<'a>)> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::NameAndType { name, descriptor }) => {
                Ok((self.get_utf8(*name)?, self.get_utf8(*descriptor)?))
            }
            _ => bail!("Invalid class constant pool item index"),
        }
    }
}
