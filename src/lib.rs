#![feature(once_cell)]
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

pub use class::*;
use field_storage::FieldStorage;

// TODO: Garbage collection for objects
// TODO: figure out if I need to keep safety in mind when dropping
/// JVM instance. This cannot be moved because it borrows from itself.
pub struct JVM<'a> {
    string_storage: Arena<String>,
    class_storage: Arena<Class<'a>>,
    typ_storage: Arena<Typ<'a>>,
    method_storage: Arena<Method<'a>>,
    // Implementation detail of const_pool, maybe remove in the future
    method_descriptor_storage: Arena<MethodDescriptor<'a>>,
    class_path: Vec<PathBuf>,
    // TODO: intern string objects instead
    strings: RwLock<HashSet<&'a str>>,
    types: RwLock<HashSet<&'a Typ<'a>>>,
    classes: RwLock<HashMap<IntStr<'a>, &'a Class<'a>>>,
    // I hate this
    // I had a static Class in parse.rs, but apparently I can't use a &'a Class<'static> as a &'a Class<'a>
    dummy_class: &'a Class<'a>,
}

bitflags::bitflags! {
    pub struct AccessFlags: u16 {
        const STATIC = 0x0008;
        const FINAL = 0x0010;
    }
}

// TODO: impl Copy?
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Typ<'a> {
    Bool,
    Byte,
    Short,
    Char,
    Int,
    Long,
    Float,
    Double,
    Class(IntStr<'a>),
    Array { base: &'a Typ<'a>, dimensions: u8 },
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
            Typ::Class(class) => write!(f, "{}", class),
            Typ::Array { base, dimensions } => {
                write!(f, "{}{}", base, "[]".repeat(*dimensions as usize))
            }
        }
    }
}

pub struct Code {
    max_stack: u16,
    max_locals: u16,
    bytes: Vec<u8>,
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
        let class_storage = Default::default();
        // SAFETY: Classes inserted into the arena are valid as long as the arena exists,
        // even if the reference to it is invalidated
        let dummy_class =
            unsafe { &*(&class_storage as *const Arena<Class>) }.alloc(Class::dummy_class());
        Self {
            string_storage: Default::default(),
            class_storage,
            typ_storage: Default::default(),
            method_storage: Default::default(),
            method_descriptor_storage: Default::default(),
            class_path,
            strings: Default::default(),
            types: Default::default(),
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
            let storage = &self.string_storage as *const Arena<String>;
            // SAFETY: Strings inserted into the arena are valid as long as the arena exists,
            // even if the reference to it is invalidated
            let str = unsafe { &*storage }.alloc(str.into());
            guard.insert(str);
            str
        })
    }

    pub fn intern_type(&self, typ: Typ<'a>) -> &'a Typ<'a> {
        let guard = self.types.read().unwrap();
        if let Some(typ) = guard.get(&typ) {
            *typ
        } else {
            drop(guard);
            let mut guard = self.types.write().unwrap();
            let storage = &self.typ_storage as *const Arena<Typ>;
            // SAFETY: Strings inserted into the arena are valid as long as the arena exists,
            // even if the reference to it is invalidated
            let typ = unsafe { &*storage }.alloc(typ.into());
            guard.insert(typ);
            typ
        }
    }

    /// Load & initializes a class if it hasn't already been.
    /// Since this is called as late as possible, both can be done together
    /// (if we switch to Resolution as part of linking for performance reasons, this changes).
    pub fn resolve_class<'b>(&'b self, name: IntStr<'a>) -> Result<&'a Class<'a>> {
        self.resolve_class_impl(name, Vec::new())
    }

    fn resolve_class_impl<'b>(
        &'b self,
        name: IntStr<'a>,
        mut check_circular: Vec<IntStr<'a>>,
    ) -> Result<&'a Class<'a>> {
        if let Some(class) = self.classes.read().unwrap().get(&name) {
            if let Some(thread) = class.initializer.get() {
                if thread != std::thread::current().id() {
                    let _ = class.init_lock.lock().unwrap();
                }
            }
            return Ok(*class);
        }

        let mut bytes = None;
        for path in &self.class_path {
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

        let (class, super_class) = parse::read_class_file(&bytes, &self)?;

        let class_storage = &self.class_storage as *const Arena<Class>;
        // SAFETY: Classes inserted into the arena are valid as long as the arena exists,
        // even if the reference to it is invalidated
        let class = unsafe { &*class_storage }.alloc(class);

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

            if super_class.access_flags.contains(AccessFlags::FINAL) {
                bail!("Tried to subclass final class {}", super_class_name)
            }

            class.super_class = Some(super_class);

            // Method resolution
            for (nat, method) in &super_class.methods {
                class.methods.entry(*nat).or_insert(*method);
            }
        } else if class.name != self.intern_str("java/lang/Object") {
            bail!("Class has no superclass")
        }

        // While parsing, each method has their class field set to a dummy
        // We can't use class.methods directly because that also includes parent classes
        for method in new_methods.values() {
            method.class.set(class);
        }
        for field in class.fields.values() {
            field.class.set(class);
        }

        let mut guard = self.classes.write().unwrap();
        // Check if loaded by another thread in the meantime
        if let Some(class) = guard.get(&name) {
            return Ok(*class);
        }
        guard.insert(name, class);

        // Initialization
        class.initializer.set(Some(std::thread::current().id()));
        let _init_lock_guard = class.init_lock.lock().unwrap();
        drop(guard);

        for field in class.fields.values() {
            if let Some(index) = field.const_value_index {
                unsafe {
                    match field.descriptor {
                        Typ::Bool | Typ::Byte => class.static_storage.write_u8(
                            field.byte_offset,
                            class.const_pool.get_int(index)? as i8 as u8,
                        ),
                        Typ::Short | Typ::Char => class.static_storage.write_u16(
                            field.byte_offset,
                            class.const_pool.get_int(index)? as i16 as u16,
                        ),
                        Typ::Int => class
                            .static_storage
                            .write_u32(field.byte_offset, class.const_pool.get_int(index)? as u32),
                        Typ::Float => class.static_storage.write_u32(
                            field.byte_offset,
                            class.const_pool.get_float(index)?.to_bits(),
                        ),
                        Typ::Double => class.static_storage.write_u64(
                            field.byte_offset,
                            class.const_pool.get_double(index)?.to_bits(),
                        ),
                        Typ::Long => class
                            .static_storage
                            .write_u64(field.byte_offset, class.const_pool.get_long(index)? as u64),
                        Typ::Class(class) => {
                            if class.0 == "java/lang/String" {
                                todo!()
                            } else {
                                bail!("final static non-null object")
                            }
                        }
                        Typ::Array { .. } => bail!("final static non-null array"),
                    }
                }
            }
        }

        if let Some(method) =
            class.method(self.intern_str("<clinit>"), &MethodDescriptor(vec![], None))
        {
            interp::run(self, method, &[])?;
        }
        class.initializer.set(None);

        Ok(class)
    }

    fn create_object(&self, class: &Class) -> Object {
        let layout = class.object_layout;
        let data = FieldStorage::new(layout);
        unsafe { data.write_usize(0, class as *const Class as usize) }
        Box::leak(ObjectData { data }.into())
    }
}
