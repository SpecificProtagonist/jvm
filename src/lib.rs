use anyhow::{anyhow, bail, Context, Result};
use object::{Object, ObjectData};
use std::{
    alloc::Layout, collections::HashMap, fmt::Debug, fs::File, hash::Hash, io::Read, path::PathBuf,
};

mod class;
mod const_pool;
mod datastructures;
mod field_storage;
pub mod interp;
mod object;
mod parse;

use class::*;
pub use const_pool::*;
use datastructures::*;
use field_storage::FieldStorage;

/// This leakes memory (perfectly fine for a binary, usually not for a lib)
// TODO: use typed-arena (together with Pin, ...) instead of Box::leak
// TODO: Garbage collection for objects
pub struct JVM {
    class_path: Vec<PathBuf>,
    // TODO: intern string objects instead
    strings: Interner<String>,
    types: Interner<Typ>,
    classes: Vec<Class>,
    classes_by_name: HashMap<Id<String>, Class>,
    /// As far as I can tell, JVM supports field overloading
    fields: HashMap<FieldRef, Field>,
    methods: HashMap<MethodRef, Method>,
}

bitflags::bitflags! {
    pub struct AccessFlags: u16 {
        const STATIC = 0x0008;
        const FINAL = 0x0010;
    }
}

// Remove Id<Field> & Id<Method>?
/// This field doesn't necessariy exist and will need to be resolved
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldRef {
    class: Id<String>,
    name: Id<String>,
    typ: Id<Typ>,
}

/// This Method doesn't necessarily exist and will need to be resolved
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MethodRef {
    class: Id<String>,
    name: Id<String>,
    typ: MethodDescriptor,
}

impl MethodRef {
    pub fn display(&self, jvm: &JVM) -> String {
        format!(
            "[{} {} {:?}]",
            jvm.strings.get(self.class),
            jvm.strings.get(self.name),
            &self.typ
        )
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MethodDescriptor(Vec<Id<Typ>>, Option<Id<Typ>>);

// Type ids of primitives, interned upon JVM construction
const BOOL: Id<Typ> = Id::with_index(0);
const BYTE: Id<Typ> = Id::with_index(1);
const SHORT: Id<Typ> = Id::with_index(2);
const CHAR: Id<Typ> = Id::with_index(3);
const INT: Id<Typ> = Id::with_index(4);
const LONG: Id<Typ> = Id::with_index(5);
const FLOAT: Id<Typ> = Id::with_index(6);
const DOUBLE: Id<Typ> = Id::with_index(7);

// TODO: impl Copy?
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Typ {
    Bool,
    Byte,
    Short,
    Char,
    Int,
    Long,
    Float,
    Double,
    Class(Id<String>),
    Array { base: Id<Typ>, dimensions: u8 },
}

impl Typ {
    fn layout(&self) -> Layout {
        use Typ::*;
        match self {
            Bool | Byte => Layout::new::<u8>(),
            Short | Char => Layout::new::<u16>(),
            Int | Float => Layout::new::<u32>(),
            Long | Double => Layout::new::<u64>(),
            Class(..) | Array { .. } => Layout::new::<usize>(),
        }
    }
}

pub struct Code {
    max_stack: u16,
    max_locals: u16,
    bytes: Vec<u8>,
}

impl JVM {
    pub fn new(class_path: Vec<PathBuf>) -> Self {
        let mut jvm = Self {
            class_path,
            strings: Default::default(),
            types: Default::default(),
            classes: Default::default(),
            classes_by_name: Default::default(),
            fields: Default::default(),
            methods: Default::default(),
        };

        // Intern primitives so we have const ids for convenience
        assert_eq!(jvm.types.intern(Typ::Bool), BOOL);
        assert_eq!(jvm.types.intern(Typ::Byte), BYTE);
        assert_eq!(jvm.types.intern(Typ::Short), SHORT);
        assert_eq!(jvm.types.intern(Typ::Char), CHAR);
        assert_eq!(jvm.types.intern(Typ::Int), INT);
        assert_eq!(jvm.types.intern(Typ::Long), LONG);
        assert_eq!(jvm.types.intern(Typ::Float), FLOAT);
        assert_eq!(jvm.types.intern(Typ::Double), DOUBLE);

        jvm
    }

    pub fn resolve_class(&mut self, name: Id<String>) -> Result<Class> {
        if let Some(class_ref) = self.classes_by_name.get(&name) {
            return Ok(*class_ref);
        }

        let mut bytes = None;
        for path in &self.class_path {
            // TODO: package folder structure

            let mut path = path.clone();
            path.push(self.strings.get(name).rsplit('/').next().unwrap());
            path.set_extension("class");

            if path.exists() {
                let mut file = Vec::new();
                File::open(path)
                    .with_context(|| format!("Trying to load class {}", self.strings.get(name)))?
                    .read_to_end(&mut file)?;
                bytes = Some(file);
                break;
            }
        }
        let bytes =
            bytes.ok_or_else(|| anyhow!("No class def found for {}", self.strings.get(name)))?;

        let class = parse::read_class_file(&bytes, &mut self.types, &mut self.strings)?;

        if name != class.name {
            bail!("Class name did not math file name")
        }

        if class.version.0 > 52 {
            bail!(
                "Class version {}.{} > 45.3 not supported yet",
                class.version.0,
                class.version.1
            )
        }

        self.classes.push(class);
        self.classes_by_name.insert(name, class);

        // Register fields & methods
        for field in &class.fields {
            self.fields.insert(
                FieldRef {
                    class: name,
                    name: field.name,
                    typ: field.descriptor,
                },
                Field { class, meta: field },
            );
        }
        for method in &class.methods {
            self.methods.insert(
                MethodRef {
                    class: name,
                    name: method.name,
                    typ: method.descriptor.clone(),
                },
                Method {
                    class,
                    meta: method,
                },
            );
        }

        // Superclasses loading & verification
        if let Some(super_class_name) = class.super_class {
            let super_class = self.resolve_class(super_class_name)?;
            if super_class.access_flags.contains(AccessFlags::FINAL) {
                bail!(
                    "Tried to subclass final class {}",
                    self.strings.get(super_class_name)
                )
            }
        } else if class.name != self.strings.intern_str("java/lang/Object") {
            bail!("Class has no superclass")
        }

        Ok(class)
    }

    pub fn resolve_field(&mut self, field_ref: FieldRef) -> Result<Field> {
        let class = self.resolve_class(field_ref.class)?;
        if let Some(field) = self.fields.get(&field_ref) {
            return Ok(*field);
        } else {
            let super_class = class.super_class.ok_or_else(|| {
                anyhow!(
                    "Failed to resolve field {}",
                    self.strings.get(field_ref.name)
                )
            })?;
            self.resolve_field(FieldRef {
                class: super_class,
                ..field_ref
            })
        }
    }

    pub fn create_object(&mut self, class: Class) -> Object {
        let layout = class.object_layout;
        let data = FieldStorage::new(layout);
        unsafe { data.write_usize(0, class as *const ClassMeta as usize) }
        Box::leak(ObjectData { data }.into())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use interp::{LocalValue, ReturnValue};

    #[test]
    fn invoke_static() -> Result<()> {
        let mut jvm = JVM::new(vec!["classes".into(), "test_classes/pass".into()]);
        let method = MethodRef {
            class: jvm.strings.intern("InvokeStatic".into()),
            name: jvm.strings.intern("add".into()),
            typ: MethodDescriptor(vec![INT], Some(INT)),
        };
        let args = [LocalValue::Int(1)];
        assert_eq!(interp::run(&mut jvm, &method, &args)?, ReturnValue::Int(43));
        Ok(())
    }

    #[test]
    fn field_access() -> Result<()> {
        let mut jvm = JVM::new(vec!["classes".into(), "test_classes/pass".into()]);
        let set_method = MethodRef {
            class: jvm.strings.intern("FieldAccess".into()),
            name: jvm.strings.intern("set".into()),
            typ: MethodDescriptor(vec![INT], None),
        };
        let get_method = MethodRef {
            class: jvm.strings.intern("FieldAccess".into()),
            name: jvm.strings.intern("get".into()),
            typ: MethodDescriptor(vec![], Some(INT)),
        };
        assert_eq!(
            interp::run(&mut jvm, &set_method, &[LocalValue::Int(42)])?,
            ReturnValue::Void
        );
        assert_eq!(
            interp::run(&mut jvm, &get_method, &[])?,
            ReturnValue::Int(42)
        );
        Ok(())
    }
}
