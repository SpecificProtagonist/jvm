use alloc::sync::Arc;

use crate::{
    class::ClassRef,
    field::{Field, FieldNaT},
    jvm::{error, JvmResult},
    method::{MethodNaT, MethodNaTBorrow, MethodRef},
    object::Object,
    parse,
    typ::Type,
    unreachable_verified, AccessFlags, Jvm,
};

// Differentiating runtime- and on-disk const pool shouldn't be neccessary
// although it would allow a speedup
#[derive(Default)]
pub(crate) struct ConstPool {
    pub items: Vec<ConstPoolItem>,
}

#[derive(Clone)]
pub(crate) enum ConstPoolItem {
    Utf8(Arc<str>),
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    String(Object),
    Class(ClassRef),
    Field(&'static Field),
    StaticMethod(MethodRef),
    // Class is neccessary for invoke_special
    VirtualMethod(ClassRef, MethodNaT),
    InterfaceMethod(ClassRef, MethodNaT),
    NameAndType { name: u16, descriptor: u16 },
    // Only present before resolve
    FieldRef { class: u16, nat: u16 },
    // Only present before resolve
    MethodRef { class: u16, nat: u16 },
    // Only present before resolve
    InterfaceMethodRef { class: u16, nat: u16 },
    // Only present before resolve
    RawString(u16),
    RawClass(u16),
    // Accoring to the spec, long and doubles taking two entries was a design mistake
    PlaceholderAfterLongOrDoubleEntryOrForEntryZero,
}

fn cfe(jvm: &Jvm) -> Object {
    error(jvm, "ClassFormatError", "const pool")
}

impl ConstPool {
    /// Must be called prior to initialization
    pub fn resolve(&mut self, jvm: &Jvm, this_class: ClassRef) -> JvmResult<()> {
        for i in 0..self.items.len() {
            if let ConstPoolItem::RawClass(_) = self.items[i] {
                let name = self.get_unresolved_class(jvm, i as u16)?;
                let resolved = jvm.resolve_class_internal(&name)?;
                if !this_class.can_access_class(resolved) {
                    return Err(error(
                        jvm,
                        "IllegalAccessError",
                        &format!("can't access {name} from {this_class}"),
                    ));
                }
                self.items[i] = ConstPoolItem::Class(resolved);
            }
        }
        for i in 0..self.items.len() {
            match self.items[i] {
                ConstPoolItem::MethodRef { class, nat } => {
                    // Arrays inherit methods from Object
                    let class = self.get_class(jvm, class)?;
                    if class.is_interface() {
                        return Err(error(jvm, "IncompatibleClassChangeError", ""));
                    }
                    let (name, descriptor) = self.get_raw_nat(jvm, nat)?;
                    let typ = parse::parse_method_descriptor(jvm, &descriptor)?;
                    if name.starts_with('<') & ((&*name != "<init>") | typ.returns.is_some()) {
                        return Err(cfe(jvm));
                    }
                    let nat = MethodNaT { name, typ };
                    let method = class
                        .methods
                        .get(&MethodNaTBorrow {
                            name: &nat.name,
                            typ: &nat.typ,
                        })
                        .ok_or_else(|| error(jvm, "NoSuchMethodError", &format!("{nat:?}")))?;
                    if !class.is_member_accessible_from(method.access_flags, this_class) {
                        return Err(error(jvm, "IllegalAccessError", ""));
                    }
                    if method.access_flags.contains(AccessFlags::STATIC) {
                        self.items[i] = ConstPoolItem::StaticMethod(method);
                    } else {
                        // TODO: check if returning java.lang.Object for arrays is correct for invokespecial
                        self.items[i] = ConstPoolItem::VirtualMethod(class, nat);
                    }
                }
                ConstPoolItem::InterfaceMethodRef { class, nat } => {
                    // TODO: Do full resolution now (5.4.3.4)
                    let class = self.get_class(jvm, class)?;
                    if !class.is_interface() {
                        return Err(error(jvm, "IncompatibleClassChangeError", ""));
                    }
                    let (name, descriptor) = self.get_raw_nat(jvm, nat)?;
                    let typ = parse::parse_method_descriptor(jvm, &descriptor)?;
                    self.items[i] = ConstPoolItem::InterfaceMethod(class, MethodNaT { name, typ });
                }
                ConstPoolItem::FieldRef { class, nat } => {
                    let class = self.get_class(jvm, class)?;
                    let (name, typ) = self.get_raw_nat(jvm, nat)?;
                    let (typ, _) = parse::parse_typ_descriptor(jvm, &typ, 0)?;
                    let nat = FieldNaT { name, typ };
                    if let Some(field) = class.fields.get(&nat) {
                        if !class.is_member_accessible_from(field.access_flags, this_class) {
                            return Err(error(jvm, "IllegalAccessError", ""));
                        }
                        self.items[i] = ConstPoolItem::Field(field);
                    } else {
                        return Err(error(jvm, "NoSuchFieldError", &format!("{nat:?}")));
                    }
                }
                ConstPoolItem::RawString(index) => {
                    // The spec doesn't describe when these strings should get instantiated, so let's just do it here
                    let string = self.get_utf8(jvm, index)?;
                    // TODO: move string object creation routine to jvm (for interning)
                    let length = string.encode_utf16().count();
                    let backing_array = jvm.create_array_of(&Type::Char, length as i32);
                    for (index, char) in string.encode_utf16().enumerate() {
                        backing_array.array_write::<i16>(jvm, index as i32, char as i16)?
                    }
                    let string_class = jvm.resolve_class_internal("java.lang.String")?;
                    let obj = jvm.create_object_uninitialized(string_class);
                    let field =
                        string_class.field("chars", Type::Ref("char[]".into())).expect("API requirement: java.lang.String must contain an instance field `chars` of type `char[]`");
                    field.instance_set(jvm.heap.ptrs(), obj, Some(backing_array).into());
                    self.items[i] = ConstPoolItem::String(obj)
                }
                _ => (),
            }
        }
        Ok(())
    }

    pub fn get_int(&self, jvm: &Jvm, index: u16) -> JvmResult<i32> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Integer(int)) => Ok(*int),
            _ => Err(cfe(jvm)),
        }
    }

    pub fn get_long(&self, jvm: &Jvm, index: u16) -> JvmResult<i64> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Long(long)) => Ok(*long),
            _ => Err(cfe(jvm)),
        }
    }

    pub fn get_float(&self, jvm: &Jvm, index: u16) -> JvmResult<f32> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Float(float)) => Ok(*float),
            _ => Err(cfe(jvm)),
        }
    }

    pub fn get_double(&self, jvm: &Jvm, index: u16) -> JvmResult<f64> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Double(double)) => Ok(*double),
            _ => Err(cfe(jvm)),
        }
    }

    pub fn get_utf8(&self, jvm: &Jvm, index: u16) -> JvmResult<Arc<str>> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Utf8(string)) => Ok(string.clone()),
            _ => Err(cfe(jvm)),
        }
    }

    pub fn get_string(&self, jvm: &Jvm, index: u16) -> JvmResult<Object> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::String(string)) => Ok(*string),
            _ => Err(cfe(jvm)),
        }
    }

    /// Cannot be called after resolution
    /// Converts binary to normal names
    pub fn get_unresolved_class(&self, jvm: &Jvm, index: u16) -> JvmResult<String> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::RawClass(index)) => {
                let mut name = self.get_utf8(jvm, *index)?.replace('/', ".");

                while name.starts_with('[') {
                    let (component, end) = parse::parse_typ_descriptor(jvm, name.as_str(), 1)?;
                    if end != name.len() {
                        return Err(error(jvm, "ClassFormatError", &name));
                    }
                    name = format!("{component}[]");
                }

                Ok(name)
            }
            _ => Err(cfe(jvm)),
        }
    }

    /// Requires resolution first
    pub fn get_class(&self, jvm: &Jvm, index: u16) -> JvmResult<ClassRef> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Class(class)) => Ok(class),
            _ => Err(cfe(jvm)),
        }
    }

    /// Requires resolution first
    pub fn get_static_method(&self, jvm: &Jvm, index: u16) -> JvmResult<MethodRef> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::StaticMethod(method)) => Ok(method),
            _ => Err(cfe(jvm)),
        }
    }

    /// Requires resolution first
    pub fn get_virtual_method(&self, jvm: &Jvm, index: u16) -> JvmResult<(ClassRef, &MethodNaT)> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::VirtualMethod(class, nat)) => Ok((class, nat)),
            _ => Err(cfe(jvm)),
        }
    }

    /// Requires resolution first
    pub fn get_interface_method(&self, jvm: &Jvm, index: u16) -> JvmResult<(ClassRef, &MethodNaT)> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::InterfaceMethod(class, nat)) => Ok((class, nat)),
            _ => Err(cfe(jvm)),
        }
    }

    /// Requires resolution first
    pub fn get_field(&self, jvm: &Jvm, index: u16) -> JvmResult<&'static Field> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Field(field)) => Ok(field),
            _ => Err(cfe(jvm)),
        }
    }

    fn get_raw_nat(&self, jvm: &Jvm, index: u16) -> JvmResult<(Arc<str>, Arc<str>)> {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::NameAndType { name, descriptor }) => {
                Ok((self.get_utf8(jvm, *name)?, self.get_utf8(jvm, *descriptor)?))
            }
            _ => Err(cfe(jvm)),
        }
    }

    /// Requires verification first
    pub fn get_class_verified(&self, index: u16) -> ClassRef {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Class(class)) => class,
            _ => unreachable_verified(),
        }
    }

    /// Requires verification first
    pub fn get_field_verified(&self, index: u16) -> &'static Field {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::Field(field)) => field,
            _ => unreachable!(),
        }
    }

    /// Requires verification first
    pub fn get_virtual_method_verified(&self, index: u16) -> (ClassRef, &MethodNaT) {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::VirtualMethod(class, nat)) => (class, nat),
            _ => unreachable_verified(),
        }
    }

    /// Requires verification first
    pub fn get_static_method_verified(&self, index: u16) -> MethodRef {
        match self.items.get(index as usize) {
            Some(ConstPoolItem::StaticMethod(method)) => method,
            _ => unreachable_verified(),
        }
    }
}
