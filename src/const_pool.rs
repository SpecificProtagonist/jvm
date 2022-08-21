use crate::{
    exception,
    field::{Field, FieldNaT},
    object::Object,
    parse, AccessFlags, Class, IntStr, JVMResult, Method, MethodNaT, Typ, JVM,
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
    String(Object<'a>),
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
                ConstPoolItem::RawString(index) => {
                    // The spec doesn't describe when these strings should get instantiated, so let's just do it here
                    let string = self.get_utf8(jvm, index)?;
                    let length = string.0.encode_utf16().count();
                    let char_array = jvm.resolve_class("[C")?;
                    let backing_array = jvm.create_array(char_array, length as i32);
                    for (index, char) in string.0.encode_utf16().enumerate() {
                        backing_array
                            .ptr
                            .array_write_i16(jvm, index as i32, char as i16)?
                    }
                    let string_class = jvm.resolve_class("java/lang/String")?;
                    let obj = jvm.create_object(string_class);
                    let field =
                        string_class.field(jvm, "chars", Typ::Ref(char_array.name)).expect("API requirement: java.lang.String must contain an instance field `chars` of type `char[]`");
                    field.instance_set(obj, Some(backing_array).into());
                    self.items[i] = ConstPoolItem::String(obj)
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

    pub fn get_string(&self, jvm: &JVM<'a>, index: u16) -> JVMResult<'a, Object<'a>> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::String(string)) => Ok(*string),
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
