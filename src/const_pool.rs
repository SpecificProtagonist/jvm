use crate::{
    exception,
    field::{Field, FieldNaT},
    object::Object,
    parse, AccessFlags, Class, IntStr, JVMResult, Method, MethodNaT, JVM,
};

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

fn cfe<'a>(jvm: &JVM<'a>) -> Object<'a> {
    exception(jvm, "ClassFormatError")
}

impl<'a> ConstPool<'a> {
    /// Must be called prior to initialization
    pub fn resolve(&mut self, jvm: &JVM<'a>) -> JVMResult<'a, ()> {
        for i in 0..self.items.len() {
            if let ConstPoolItem::RawClass(index) = self.items[i] {
                self.items[i] =
                    ConstPoolItem::Class(jvm.resolve_class(self.get_utf8(jvm, index)?)?);
            }
        }
        for i in 0..self.items.len() {
            match self.items[i] {
                ConstPoolItem::MethodRef { class, nat } => {
                    // Arrays inherit methods from Object
                    let class = self.get_class(jvm, class)?;
                    let (name, descriptor) = self.get_raw_nat(jvm, nat)?;
                    let typ = parse::parse_method_descriptor(jvm, descriptor)?;
                    let typ = jvm.method_descriptor_storage.lock().alloc(typ);
                    let method = class
                        .methods
                        .get(&MethodNaT { name, typ })
                        .ok_or_else(|| exception(jvm, "NoSuchMethodError"))?;
                    if method.access_flags.contains(AccessFlags::STATIC) {
                        self.items[i] = ConstPoolItem::StaticMethod(method);
                    } else {
                        // TODO: check if returning java/lang/Object for arrays is correct for invokespecial
                        self.items[i] =
                            ConstPoolItem::VirtualMethod(class, MethodNaT { name, typ });
                    }
                }
                ConstPoolItem::FieldRef { class, nat } => {
                    let class = self.get_class(jvm, class)?;
                    let (name, typ) = self.get_raw_nat(jvm, nat)?;
                    let (typ, _) = parse::parse_field_descriptor(jvm, typ, 0)?;
                    if let Some(field) = class.fields.get(&FieldNaT { name, typ }) {
                        self.items[i] = ConstPoolItem::Field(field);
                    } else {
                        return Err(exception(jvm, "NoSuchFieldError"));
                    }
                }
                _ => (),
            }
        }
        Ok(())
    }

    pub fn get_int(&self, jvm: &JVM<'a>, index: u16) -> JVMResult<'a, i32> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Integer(int)) => Ok(*int),
            _ => Err(cfe(jvm)),
        }
    }

    pub fn get_long(&self, jvm: &JVM<'a>, index: u16) -> JVMResult<'a, i64> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Long(long)) => Ok(*long),
            _ => Err(cfe(jvm)),
        }
    }

    pub fn get_float(&self, jvm: &JVM<'a>, index: u16) -> JVMResult<'a, f32> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Float(float)) => Ok(*float),
            _ => Err(cfe(jvm)),
        }
    }

    pub fn get_double(&self, jvm: &JVM<'a>, index: u16) -> JVMResult<'a, f64> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Double(double)) => Ok(*double),
            _ => Err(cfe(jvm)),
        }
    }

    pub fn get_utf8(&self, jvm: &JVM<'a>, index: u16) -> JVMResult<'a, IntStr<'a>> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Utf8(string)) => Ok(*string),
            _ => Err(cfe(jvm)),
        }
    }

    /// Cannot be called after resolution
    pub fn get_unresolved_class(&self, jvm: &JVM<'a>, index: u16) -> JVMResult<'a, IntStr<'a>> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::RawClass(index)) => self.get_utf8(jvm, *index),
            _ => Err(cfe(jvm)),
        }
    }

    /// Requires resolution first
    pub fn get_class(&self, jvm: &JVM<'a>, index: u16) -> JVMResult<'a, &'a Class<'a>> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Class(class)) => Ok(class),
            _ => Err(cfe(jvm)),
        }
    }

    /// Requires resolution first
    pub fn get_static_method<'b>(
        &'b self,
        jvm: &JVM<'a>,
        index: u16,
    ) -> JVMResult<'a, &'a Method<'a>> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::StaticMethod(method)) => Ok(method),
            _ => Err(cfe(jvm)),
        }
    }

    /// Requires resolution first
    pub fn get_virtual_method(
        &self,
        jvm: &JVM<'a>,
        index: u16,
    ) -> JVMResult<'a, (&'a Class<'a>, MethodNaT<'a>)> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::VirtualMethod(class, nat)) => Ok((class, *nat)),
            _ => Err(cfe(jvm)),
        }
    }

    /// Requires resolution first
    pub fn get_field(&self, jvm: &JVM<'a>, index: u16) -> JVMResult<'a, &'a Field<'a>> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Field(field)) => Ok(field),
            _ => Err(cfe(jvm)),
        }
    }

    fn get_raw_nat(&self, jvm: &JVM<'a>, index: u16) -> JVMResult<'a, (IntStr<'a>, IntStr<'a>)> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::NameAndType { name, descriptor }) => {
                Ok((self.get_utf8(jvm, *name)?, self.get_utf8(jvm, *descriptor)?))
            }
            _ => Err(cfe(jvm)),
        }
    }
}
