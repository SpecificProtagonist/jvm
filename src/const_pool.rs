use std::sync::Arc;

use crate::{
    class::Class,
    field::{Field, FieldNaT},
    jvm::{exception, JVMResult, Jvm},
    method::Method,
    method::MethodNaT,
    object::Object,
    parse,
    typ::Typ,
    AccessFlags,
};

// Differentiating runtime- and on-disk const pool shouldn't be neccessary
// although it would allow a speedup
#[derive(Debug, Default)]
pub(crate) struct ConstPool {
    pub items: Vec<ConstPoolItem>,
}

#[derive(Clone, Debug)]
pub(crate) enum ConstPoolItem {
    Utf8(Arc<str>),
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    String(Object),
    Class(&'static Class),
    Field(&'static Field),
    StaticMethod(&'static Method),
    // Class is neccessary for invoke_special
    VirtualMethod(&'static Class, MethodNaT<'static>),
    InterfaceMethodRef { class: u16, nat: u16 },
    NameAndType { name: u16, descriptor: u16 },
    FieldRef { class: u16, nat: u16 },
    MethodRef { class: u16, nat: u16 },
    RawString(u16),
    RawClass(u16),
    // Accoring to the spec, long and doubles taking two entries was a design mistake
    PlaceholderAfterLongOrDoubleEntryOrForEntryZero,
}

fn cfe(jvm: &Jvm) -> Object {
    exception(jvm, "ClassFormatError")
}

impl ConstPool {
    /// Must be called prior to initialization
    pub fn resolve(&mut self, jvm: &Jvm) -> JVMResult<()> {
        for i in 0..self.items.len() {
            if let ConstPoolItem::RawClass(index) = self.items[i] {
                self.items[i] =
                    ConstPoolItem::Class(jvm.resolve_class(&self.get_utf8(jvm, index)?)?);
            }
        }
        for i in 0..self.items.len() {
            match self.items[i] {
                ConstPoolItem::MethodRef { class, nat } => {
                    // Arrays inherit methods from Object
                    let class = self.get_class(jvm, class)?;
                    let (name, descriptor) = self.get_raw_nat(jvm, nat)?;
                    let typ = parse::parse_method_descriptor(jvm, &descriptor)?;
                    let typ = jvm.method_descriptor_storage.lock().alloc(typ);
                    let method = class
                        .methods
                        .get(&MethodNaT {
                            name: name.clone(),
                            typ,
                        })
                        .ok_or_else(|| exception(jvm, "NoSuchMethodError"))?;
                    if method.access_flags.contains(AccessFlags::STATIC) {
                        self.items[i] = ConstPoolItem::StaticMethod(method);
                    } else {
                        // TODO: check if returning java.lang.Object for arrays is correct for invokespecial
                        self.items[i] =
                            ConstPoolItem::VirtualMethod(class, MethodNaT { name, typ });
                    }
                }
                ConstPoolItem::FieldRef { class, nat } => {
                    let class = self.get_class(jvm, class)?;
                    let (name, typ) = self.get_raw_nat(jvm, nat)?;
                    let (typ, _) = parse::parse_typ_descriptor(jvm, &typ, 0)?;
                    if let Some(field) = class.fields.get(&FieldNaT { name, typ }) {
                        self.items[i] = ConstPoolItem::Field(field);
                    } else {
                        return Err(exception(jvm, "NoSuchFieldError"));
                    }
                }
                ConstPoolItem::RawString(index) => {
                    // The spec doesn't describe when these strings should get instantiated, so let's just do it here
                    let string = self.get_utf8(jvm, index)?;
                    // TODO: move string object creation routine to jvm (for interning)
                    let length = string.encode_utf16().count();
                    let backing_array = jvm.create_array(&Typ::Char, length as i32);
                    for (index, char) in string.encode_utf16().enumerate() {
                        backing_array
                            .ptr
                            .array_write_i16(jvm, index as i32, char as i16)?
                    }
                    let string_class = jvm.resolve_class("java.lang.String")?;
                    let obj = jvm.create_object(string_class);
                    let field =
                        string_class.field("chars", Typ::Ref("char[]".into())).expect("API requirement: java.lang.String must contain an instance field `chars` of type `char[]`");
                    field.instance_set(obj, Some(backing_array).into());
                    self.items[i] = ConstPoolItem::String(obj)
                }
                _ => (),
            }
        }
        Ok(())
    }

    pub fn get_int(&self, jvm: &Jvm, index: u16) -> JVMResult<i32> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Integer(int)) => Ok(*int),
            _ => Err(cfe(jvm)),
        }
    }

    pub fn get_long(&self, jvm: &Jvm, index: u16) -> JVMResult<i64> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Long(long)) => Ok(*long),
            _ => Err(cfe(jvm)),
        }
    }

    pub fn get_float(&self, jvm: &Jvm, index: u16) -> JVMResult<f32> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Float(float)) => Ok(*float),
            _ => Err(cfe(jvm)),
        }
    }

    pub fn get_double(&self, jvm: &Jvm, index: u16) -> JVMResult<f64> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Double(double)) => Ok(*double),
            _ => Err(cfe(jvm)),
        }
    }

    pub fn get_utf8(&self, jvm: &Jvm, index: u16) -> JVMResult<Arc<str>> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Utf8(string)) => Ok(string.clone()),
            _ => Err(cfe(jvm)),
        }
    }

    pub fn get_string(&self, jvm: &Jvm, index: u16) -> JVMResult<Object> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::String(string)) => Ok(*string),
            _ => Err(cfe(jvm)),
        }
    }

    /// Cannot be called after resolution
    /// Converts binary to normal names
    pub fn get_unresolved_class(&self, jvm: &Jvm, index: u16) -> JVMResult<String> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::RawClass(index)) => self
                .get_utf8(jvm, *index)
                .map(|name| name.replace('/', ".")),
            _ => Err(cfe(jvm)),
        }
    }

    /// Requires resolution first
    pub fn get_class(&self, jvm: &Jvm, index: u16) -> JVMResult<&'static Class> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Class(class)) => Ok(class),
            _ => Err(cfe(jvm)),
        }
    }

    /// Requires resolution first
    pub fn get_static_method(&self, jvm: &Jvm, index: u16) -> JVMResult<&'static Method> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::StaticMethod(method)) => Ok(method),
            _ => Err(cfe(jvm)),
        }
    }

    /// Requires resolution first
    pub fn get_virtual_method(
        &self,
        jvm: &Jvm,
        index: u16,
    ) -> JVMResult<(&'static Class, MethodNaT)> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::VirtualMethod(class, nat)) => Ok((class, nat.clone())),
            _ => Err(cfe(jvm)),
        }
    }

    /// Requires resolution first
    pub fn get_field(&self, jvm: &Jvm, index: u16) -> JVMResult<&'static Field> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Field(field)) => Ok(field),
            _ => Err(cfe(jvm)),
        }
    }

    fn get_raw_nat(&self, jvm: &Jvm, index: u16) -> JVMResult<(Arc<str>, Arc<str>)> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::NameAndType { name, descriptor }) => {
                Ok((self.get_utf8(jvm, *name)?, self.get_utf8(jvm, *descriptor)?))
            }
            _ => Err(cfe(jvm)),
        }
    }
}
