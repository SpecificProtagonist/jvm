use anyhow::{anyhow, bail, Context, Result};
use fixed_typed_arena::ManuallyDropArena;
use object::Object;
use std::{
    alloc::Layout,
    collections::{HashMap, HashSet},
    fmt::Debug,
    fs::File,
    hash::Hash,
    io::Read,
    path::PathBuf,
    sync::{Mutex, RwLock},
};

mod class;
mod const_pool;
mod field_storage;
pub mod interp;
mod object;
mod parse;
mod verification;

pub use class::*;
use field_storage::FieldStorage;

use crate::object::min_object_size;

// TODO: Garbage collection for objects
// TODO: figure out if I need to keep safety in mind when dropping
/// JVM instance. This cannot be moved because it borrows from itself.
pub struct JVM<'a> {
    string_storage: Mutex<ManuallyDropArena<String, 256>>,
    class_storage: Mutex<ManuallyDropArena<RefType<'a>, 128>>,
    method_storage: Mutex<ManuallyDropArena<Method<'a>, 128>>,
    // Implementation detail of const_pool, maybe remove in the future
    method_descriptor_storage: Mutex<ManuallyDropArena<MethodDescriptor<'a>, 128>>,
    class_path: Vec<PathBuf>,
    // TODO: intern string objects instead
    strings: RwLock<HashSet<&'a str>>,
    classes: RwLock<HashMap<IntStr<'a>, &'a RefType<'a>>>,
    // I hate this
    // I had a static Class in parse.rs, but apparently I can't use a &'a Class<'static> as a &'a Class<'a>
    dummy_class: &'a Class<'a>,
}

bitflags::bitflags! {
    pub struct AccessFlags: u16 {
        const STATIC = 0x0008;
        const FINAL = 0x0010;
        const SUPER = 0x0020;
    }
}

// TODO: impl Copy?
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Typ<'a> {
    Bool,
    Byte,
    Short,
    Char,
    Int,
    Long,
    Float,
    Double,
    Ref(IntStr<'a>),
}

impl<'a> Typ<'a> {
    fn layout(&self) -> Layout {
        use Typ::*;
        match self {
            Bool | Byte => Layout::new::<u8>(),
            Short | Char => Layout::new::<u16>(),
            Int | Float => Layout::new::<u32>(),
            Long | Double => Layout::new::<u64>(),
            Ref(..) => Layout::new::<usize>(),
        }
    }

    fn array_dimensions(&self) -> usize {
        match self {
            Self::Ref(name) => name.0.find(|c| c != '[').unwrap_or(0),
            _ => 0,
        }
    }
}

impl<'a> std::fmt::Display for Typ<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Typ::Bool => write!(f, "boolean"),
            Typ::Byte => write!(f, "byte"),
            Typ::Short => write!(f, "short"),
            Typ::Char => write!(f, "char"),
            Typ::Int => write!(f, "int"),
            Typ::Long => write!(f, "long"),
            Typ::Float => write!(f, "float"),
            Typ::Double => write!(f, "double"),
            Typ::Ref(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq)]
pub struct IntStr<'a>(&'a str);

impl<'a> PartialEq for IntStr<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0 as *const str == other.0 as *const str
    }
}

impl<'a> Hash for IntStr<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.as_ptr().hash(state);
    }
}

impl<'a> std::fmt::Display for IntStr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

impl<'a> JVM<'a> {
    pub fn new(class_path: Vec<PathBuf>) -> Self {
        let class_storage = Mutex::<ManuallyDropArena<_, 128>>::default();
        // SAFETY: Classes inserted into the arena are valid as long as the arena exists,
        // even if the reference to it is invalidated
        let dummy_class = class_storage
            .lock()
            .unwrap()
            .alloc(RefType::Class(Class::dummy_class()));
        let dummy_class = if let RefType::Class(class) = dummy_class {
            class
        } else {
            unreachable!()
        };
        Self {
            string_storage: Default::default(),
            class_storage,
            method_storage: Default::default(),
            method_descriptor_storage: Default::default(),
            class_path,
            strings: Default::default(),
            classes: Default::default(),
            dummy_class,
        }
        .into()
    }

    pub fn intern_str<'b>(&'b self, str: &'b str) -> IntStr<'a> {
        let guard = self.strings.read().unwrap();
        IntStr(if let Some(str) = guard.get(str) {
            *str
        } else {
            drop(guard);
            let mut guard = self.strings.write().unwrap();
            let str = self.string_storage.lock().unwrap().alloc(str.into());
            guard.insert(str);
            str
        })
    }

    /// Returns the class, loading it if it wasn't already loaded. Does not initialize the class!
    pub fn resolve_class<'b>(&'b self, name: IntStr<'a>) -> Result<&'a RefType<'a>> {
        self.resolve_class_impl(name, Vec::new())
    }

    fn resolve_class_impl<'b>(
        &'b self,
        name: IntStr<'a>,
        mut check_circular: Vec<IntStr<'a>>,
    ) -> Result<&'a RefType<'a>> {
        if let Some(class) = self.classes.read().unwrap().get(&name) {
            return Ok(*class);
        }

        if name.0.starts_with('[') {
            let (base, end) = parse::parse_field_descriptor(self, name, 1)?;
            if end != name.0.len() {
                bail!("Invalid array type descriptor: {}", name)
            }

            let super_class = if let Typ::Ref(component) = base {
                let component = self.resolve_class(component)?;
                if let Some(comp_super) = component.super_class() {
                    self.resolve_class(self.intern_str(&format!("[{}", comp_super.name(&self))))?
                } else {
                    self.resolve_class(self.intern_str("java/lang/Object"))?
                }
            } else {
                self.resolve_class(self.intern_str("java/lang/Object"))?
            };

            let array = RefType::Array { base, super_class };
            // SAFETY: Classes inserted into the arena are valid as long as the arena exists,
            // even if the reference to it is invalidated
            let array = self.class_storage.lock().unwrap().alloc(array);
            let mut guard = self.classes.write().unwrap();
            // Check if loaded by another thread in the meantime
            if let Some(array) = guard.get(&name) {
                return Ok(*array);
            }
            guard.insert(name, array);
            return Ok(array);
        }

        let mut bytes = None;
        for path in &self.class_path {
            let mut path = path.clone();
            path.push(name.0);
            path.set_extension("class");

            if path.exists() {
                let mut file = Vec::new();
                File::open(path)
                    .with_context(|| format!("Failed to load class {}", name))?
                    .read_to_end(&mut file)?;
                bytes = Some(file);
                break;
            }
        }
        let bytes = bytes.ok_or_else(|| anyhow!("No class def found for {}", name))?;

        let (class, super_class) = parse::read_class_file(&bytes, &self)
            .with_context(|| format!("Failed to parse class {}", name))?;

        // SAFETY: Classes inserted into the arena are valid as long as the arena exists,
        // even if the reference to it is invalidated
        let class_or_array = self
            .class_storage
            .lock()
            .unwrap()
            .alloc(RefType::Class(class));
        let class = if let RefType::Class(class) = class_or_array {
            class
        } else {
            unreachable!()
        };

        if name != class.name {
            bail!("Class name did not match file name")
        }

        let new_methods = class.methods.clone();

        // Superclasses loading & verification
        if let Some(super_class_name) = super_class {
            if check_circular.contains(&super_class_name) {
                bail!("Circular class inheritance for {}", class.name);
            }
            check_circular.push(super_class_name);
            let super_class = self.resolve_class_impl(super_class_name, check_circular)?;

            class.super_class = Some(super_class);

            let super_class = match super_class {
                RefType::Class(class) => class,
                RefType::Array { .. } => bail!("Can't subclass array!"),
            };

            if super_class.access_flags.contains(AccessFlags::FINAL) {
                bail!("Tried to subclass final class {}", super_class_name)
            }

            // Method resolution
            for (nat, method) in &super_class.methods {
                if method.access_flags.contains(AccessFlags::FINAL)
                    && class.methods.contains_key(nat)
                {
                    bail!("Tried to overwrite final method {}", nat);
                }
                class.methods.entry(*nat).or_insert(*method);
            }
        } else if class.name != self.intern_str("java/lang/Object") {
            bail!("Class has no superclass")
        }

        let mut guard = self.classes.write().unwrap();
        // Check if loaded by another thread in the meantime
        if let Some(class) = guard.get(&name) {
            return Ok(*class);
        }
        guard.insert(name, class_or_array);
        let class = if let Some(RefType::Class(class)) = guard.get(&name) {
            class
        } else {
            unreachable!()
        };

        // While parsing, each method has their class field set to a dummy
        // We can't use class.methods directly because that also includes parent classes
        for method in new_methods.values() {
            method.class.set(class);
        }
        for field in class.fields.values() {
            field.class.set(class);
        }

        Ok(class_or_array)
    }

    fn create_object(&self, typ: &'a RefType) -> Object<'a> {
        let class = match typ {
            RefType::Class(class) => class,
            _ => panic!(),
        };
        let data = FieldStorage::new(class.object_size);
        data.write_usize(0, typ as *const RefType as usize);
        Object {
            data,
            _marker: std::marker::PhantomData,
        }
    }

    fn create_array(&self, typ: &'a RefType, length: usize) -> Object<'a> {
        let component = match typ {
            RefType::Array { base, .. } => *base,
            _ => panic!(),
        };
        let data = FieldStorage::new(min_object_size() + component.layout().size() * length);
        data.write_usize(0, typ as *const RefType as usize);
        Object {
            data,
            _marker: std::marker::PhantomData,
        }
    }
}

impl<'a> Drop for JVM<'a> {
    fn drop(&mut self) {
        // SAFETY: All references have lifetime 'a and no member of JVM has a drop impl that dereferences these
        unsafe {
            ManuallyDropArena::drop(&mut self.string_storage.lock().unwrap());
            ManuallyDropArena::drop(&mut self.class_storage.lock().unwrap());
            ManuallyDropArena::drop(&mut self.method_storage.lock().unwrap());
            ManuallyDropArena::drop(&mut self.method_descriptor_storage.lock().unwrap());
        }
    }
}
