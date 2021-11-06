use anyhow::{anyhow, bail, Context, Result};
use object::{Object, ObjectData};
use std::{
    alloc::Layout,
    collections::{HashMap, HashSet},
    fmt::Debug,
    fs::File,
    hash::Hash,
    io::Read,
    path::PathBuf,
    sync::RwLock,
};
use typed_arena::Arena;

mod class;
mod const_pool;
mod field_storage;
pub mod interp;
mod object;
mod parse;
mod type_interner;

use class::*;
use field_storage::FieldStorage;
use type_interner::*;

// TODO: Garbage collection for objects
// TODO: figure out if I need to keep safety in mind when dropping
/// JVM instance. This cannot be moved because it borrows from itself.
pub struct JVM<'a> {
    string_storage: Arena<String>,
    class_storage: Arena<Class<'a>>,
    class_path: Vec<PathBuf>,
    // TODO: intern string objects instead
    strings: RwLock<HashSet<&'a str>>,
    types: RwLock<TypInterner<'a>>,
    classes: RwLock<HashMap<IntStr<'a>, &'a Class<'a>>>,
    /// As far as I can tell, JVM supports field overloading
    fields: RwLock<HashMap<FieldRef<'a>, Field<'a>>>,
    methods: RwLock<HashMap<MethodRef<'a>, Method<'a>>>,
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
pub struct FieldRef<'a> {
    class: IntStr<'a>,
    name: IntStr<'a>,
    typ: TypId,
}

/// This Method doesn't necessarily exist and will need to be resolved
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MethodRef<'a> {
    class: IntStr<'a>,
    name: IntStr<'a>,
    typ: MethodDescriptor,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MethodDescriptor(Vec<TypId>, Option<TypId>);

// TODO: impl Copy?
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Typ<'a> {
    Bool,
    Byte,
    Short,
    Char,
    Int,
    Long,
    Float,
    Double,
    Class(IntStr<'a>),
    Array { base: TypId, dimensions: u8 },
}

impl<'a> Typ<'a> {
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

#[derive(Debug, Clone, Copy, Hash, Eq)]
struct IntStr<'a>(&'a str);

impl<'a> PartialEq for IntStr<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0 as *const str == other.0 as *const str
    }
}

impl<'a> std::fmt::Display for IntStr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

impl<'a> JVM<'a> {
    pub fn new(class_path: Vec<PathBuf>) -> Self {
        Self {
            string_storage: Default::default(),
            class_storage: Default::default(),
            class_path,
            strings: Default::default(),
            types: Default::default(),
            classes: Default::default(),
            fields: Default::default(),
            methods: Default::default(),
        }
        .into()
    }

    fn intern_str<'b>(&'b self, str: &'b str) -> IntStr<'a> {
        let guard = self.strings.read().unwrap();
        IntStr(if let Some(str) = guard.get(str) {
            *str
        } else {
            drop(guard);
            let storage = &self.string_storage as *const Arena<String>;
            // SAFETY: Strings inserted into the arena are valid as long as the arena exists,
            // even if the reference to it is invalidated
            let str = unsafe { &*storage }.alloc(str.into());
            self.strings.write().unwrap().insert(str);
            str
        })
    }

    fn intern_type(&self, typ: Typ<'a>) -> TypId {
        self.types.write().unwrap().intern(typ)
    }

    fn resolve_class<'b>(&'b self, name: IntStr<'a>) -> Result<&'a Class<'a>> {
        if let Some(class_ref) = self.classes.read().unwrap().get(&name) {
            return Ok(*class_ref);
        }

        let mut bytes = None;
        for path in &self.class_path {
            // TODO: package folder structure

            let mut path = path.clone();
            path.push(name.0);
            path.set_extension("class");

            if path.exists() {
                let mut file = Vec::new();
                File::open(path)
                    .with_context(|| format!("Trying to load class {}", name))?
                    .read_to_end(&mut file)?;
                bytes = Some(file);
                break;
            }
        }
        let bytes = bytes.ok_or_else(|| anyhow!("No class def found for {}", name))?;

        let class = parse::read_class_file(&bytes, &self)?;

        if name != class.name {
            bail!("Class name did not match file name")
        }

        if class.version.0 > 52 {
            bail!(
                "Class version {}.{} > 45.3 not supported yet",
                class.version.0,
                class.version.1
            )
        }

        let mut guard = self.classes.write().unwrap();
        // Check if loaded by another thread in the meantime
        if let Some(class) = guard.get(&name) {
            return Ok(*class);
        }
        let class_storage = &self.class_storage as *const Arena<Class>;
        // SAFETY: Classes inserted into the arena are valid as long as the arena exists,
        // even if the reference to it is invalidated
        let class = unsafe { &*class_storage }.alloc(class);
        guard.insert(name, class);
        drop(guard);

        // Register fields & methods
        for field in &class.fields {
            self.fields.write().unwrap().insert(
                FieldRef {
                    class: name,
                    name: field.name,
                    typ: field.descriptor,
                },
                Field { class, meta: field },
            );
        }
        for method in &class.methods {
            self.methods.write().unwrap().insert(
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
                bail!("Tried to subclass final class {}", super_class_name)
            }

            // Check for circularity
            let mut current = &*class;
            let mut check = super_class;
            while let Some(current_super) = current.super_class {
                if check == current {
                    bail!("Class circularity")
                }
                if let Some(check_super) = check.super_class {
                    check = self.resolve_class(check_super)?;
                    if check == current {
                        bail!("Class circularity")
                    }
                    if let Some(check_super) = check.super_class {
                        check = self.resolve_class(check_super)?;
                    } else {
                        break;
                    }
                } else {
                    break;
                }
                current = self.resolve_class(current_super)?;
            }
        } else if class.name != self.intern_str("java/lang/Object") {
            bail!("Class has no superclass")
        }

        Ok(class)
    }

    fn resolve_field<'b>(&'b self, field_ref: FieldRef<'a>) -> Result<Field<'a>> {
        let class = self.resolve_class(field_ref.class)?;
        if let Some(field) = self.fields.read().unwrap().get(&field_ref) {
            return Ok(*field);
        } else {
            let super_class = class
                .super_class
                .ok_or_else(|| anyhow!("Failed to resolve field {}", field_ref.name))?;
            self.resolve_field(FieldRef {
                class: super_class,
                ..field_ref
            })
        }
    }

    fn create_object(&self, class: &Class) -> Object {
        let layout = class.object_layout;
        let data = FieldStorage::new(layout);
        unsafe { data.write_usize(0, class as *const Class as usize) }
        Box::leak(ObjectData { data }.into())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use interp::{LocalValue, ReturnValue};

    #[test]
    fn circular_loading() {
        let jvm = JVM::new(vec!["classes".into(), "test_classes/fail".into()]);
        let name = jvm.intern_str("CircularA");
        assert!(jvm.resolve_class(name).is_err());
    }

    #[test]
    fn invoke_static() -> Result<()> {
        let jvm = JVM::new(vec!["classes".into(), "test_classes/pass".into()]);
        let int = jvm.intern_type(Typ::Int);
        let method = MethodRef {
            class: jvm.intern_str("InvokeStatic"),
            name: jvm.intern_str("add"),
            typ: MethodDescriptor(vec![int], Some(int)),
        };
        let args = [LocalValue::Int(1)];
        assert_eq!(interp::run(&jvm, &method, &args)?, ReturnValue::Int(43));

        fn test(jvm: JVM) {
            let int = jvm.intern_type(Typ::Int);
            let method = MethodRef {
                class: jvm.intern_str("InvokeStatic"),
                name: jvm.intern_str("add"),
                typ: MethodDescriptor(vec![int], Some(int)),
            };
            let args = [LocalValue::Int(1)];
            assert_eq!(
                interp::run(&jvm, &method, &args).unwrap(),
                ReturnValue::Int(43)
            );
        }
        test(jvm);

        Ok(())
    }

    #[test]
    fn field_access() -> Result<()> {
        let jvm = JVM::new(vec!["classes".into(), "test_classes/pass".into()]);
        let int = jvm.intern_type(Typ::Int);
        let set_method = MethodRef {
            class: jvm.intern_str("FieldAccess".into()),
            name: jvm.intern_str("set".into()),
            typ: MethodDescriptor(vec![int], None),
        };
        let get_method = MethodRef {
            class: jvm.intern_str("FieldAccess".into()),
            name: jvm.intern_str("get".into()),
            typ: MethodDescriptor(vec![], Some(int)),
        };
        assert_eq!(
            interp::run(&jvm, &set_method, &[LocalValue::Int(42)])?,
            ReturnValue::Void
        );
        assert_eq!(interp::run(&jvm, &get_method, &[])?, ReturnValue::Int(42));
        Ok(())
    }
}
