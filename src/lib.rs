use anyhow::{anyhow, bail, Context, Result};
use bimap::BiMap;
use std::{
    alloc::Layout, collections::HashMap, fs::File, hash::Hash, io::Read, marker::PhantomData,
    path::PathBuf, rc::Rc,
};

mod const_pool;
mod field_storage;
pub mod interp;
mod parse;

pub use const_pool::*;
use field_storage::FieldStorage;

pub struct JVM {
    class_path: PathBuf,
    strings: Interner<String>,
    types: Interner<Typ>,
    classes: Arena<Class>,
    classes_by_name: HashMap<Id<String>, Id<Class>>,
    /// Fields are stored in classes, the id is an index there
    /// As far as I can tell, JVM supports field overloading
    fields: HashMap<FieldRef, (Id<Class>, Id<Field>)>,
    /// Methods are stored in classes, the id is an index there
    methods: HashMap<MethodRef, Rc<Method>>,
}

pub struct Class {
    name: Id<String>,
    super_class: Option<Id<String>>,
    version: (u16, u16),
    const_pool: ConstPool,
    access_flags: AccessFlags,
    interfaces: Vec<Id<String>>,
    methods: Vec<Rc<Method>>,
    fields: Arena<Field>,
    static_storage: FieldStorage,
    object_layout: Layout,
}

pub struct AccessFlags(u16);

impl AccessFlags {
    fn r#static(&self) -> bool {
        (self.0 & 0x0008) != 0
    }

    fn r#final(&self) -> bool {
        (self.0 & 0x0010) != 0
    }
}

// Remove Id<Field> & Id<Method>?
/// This field doesn't necessariy and will need to be resolved
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldRef {
    class: Id<String>,
    name: Id<String>,
    typ: Id<Typ>,
}

/// This Method doesn't necessarily and will need to be resolved
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MethodRef {
    class: Id<String>,
    name: Id<String>,
    typ: MethodDescriptor,
}

pub struct Field {
    name: Id<String>,
    access_flags: AccessFlags,
    descriptor: Id<Typ>,
    // Java has no multiple inheritance for fields, therefore each field can be at a set position
    byte_offset: u32,
}

pub struct Method {
    name: Id<String>,
    access_flags: AccessFlags,
    descriptor: MethodDescriptor,
    code: Option<Code>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MethodDescriptor(Vec<Id<Typ>>, Option<Id<Typ>>);

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
    Array(Id<Typ>),
}

impl Typ {
    fn layout(&self) -> Layout {
        use Typ::*;
        match self {
            Bool | Byte => Layout::new::<u8>(),
            Short | Char => Layout::new::<u16>(),
            Int | Float => Layout::new::<u32>(),
            Long | Double => Layout::new::<u64>(),
            Class(_) | Array(_) => Layout::new::<usize>(),
        }
    }
}

pub struct Code {
    max_stack: u16,
    max_locals: u16,
    bytes: Vec<u8>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Id<T>(u32, PhantomData<T>);

impl<T> Copy for Id<T> {}
impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

// TODO: compine with _by_name
struct Arena<T>(Vec<T>);

impl<T> Arena<T> {
    pub fn get(&self, index: Id<T>) -> &T {
        &self.0[index.0 as usize]
    }
    pub fn get_mut(&mut self, index: Id<T>) -> &mut T {
        &mut self.0[index.0 as usize]
    }
    pub fn insert(&mut self, item: T) -> Id<T> {
        let index = Id(self.0.len() as u32, PhantomData::default());
        self.0.push(item);
        index
    }

    pub fn iter_ids(&self) -> impl Iterator<Item = Id<T>> {
        (0..self.0.len()).map(|i| Id(i as u32, PhantomData::default()))
    }
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

pub struct Interner<T: Eq + Hash>(BiMap<T, Id<T>>);

impl<T: Eq + Hash> Interner<T> {
    pub fn intern(&mut self, item: T) -> Id<T> {
        if let Some(interned) = self.0.get_by_left(&item) {
            *interned
        } else {
            let interned = Id(self.0.len() as u32, PhantomData::default());
            self.0.insert(item, interned);
            interned
        }
    }

    pub fn get(&self, interned: Id<T>) -> &T {
        self.0
            .get_by_right(&interned)
            .expect("Tried to retrieve item from different from interner")
    }
}

impl Interner<String> {
    pub fn intern_str(&mut self, string: &str) -> Id<String> {
        if let Some(id) = self.0.get_by_left(string) {
            *id
        } else {
            self.intern(string.into())
        }
    }
}

impl<T: Eq + Hash> Default for Interner<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl JVM {
    pub fn new(class_path: PathBuf) -> Self {
        let mut jvm = Self {
            class_path,
            strings: Default::default(),
            types: Default::default(),
            classes: Default::default(),
            classes_by_name: Default::default(),
            fields: Default::default(),
            methods: Default::default(),
        };

        // Temporary while there is no library yet
        {
            let name = jvm.strings.intern_str("java/lang/Object");
            let class = Class {
                name,
                super_class: None,
                version: (45, 3),
                access_flags: AccessFlags(0),
                const_pool: ConstPool(Default::default()),
                methods: Default::default(),
                fields: Default::default(),
                interfaces: Default::default(),
                static_storage: FieldStorage::new(Layout::new::<()>()),
                object_layout: Layout::new::<()>(),
            };
            let class_ref = jvm.classes.insert(class);
            jvm.classes_by_name.insert(name, class_ref);
        }

        jvm
    }

    pub fn resolve_class(&mut self, name: Id<String>) -> Result<Id<Class>> {
        if let Some(class_ref) = self.classes_by_name.get(&name) {
            return Ok(*class_ref);
        }

        let mut path = self.class_path.clone();
        path.push(self.strings.get(name));
        path.set_extension("class");

        let mut bytes = Vec::new();
        File::open(path)
            .with_context(|| format!("Trying to load class {}", self.strings.get(name)))?
            .read_to_end(&mut bytes)?;

        let class = parse::read_class_file(&bytes, &mut self.types, &mut self.strings)?;

        if name != class.name {
            bail!("Class name did not math file name")
        }

        let class_ref = self.classes.insert(class);
        self.classes_by_name.insert(name, class_ref);

        // Register fields & methods
        let class = self.classes.get(class_ref);
        for field_ref in class.fields.iter_ids() {
            let field = class.fields.get(field_ref);
            self.fields.insert(
                FieldRef {
                    class: name,
                    name: field.name,
                    typ: field.descriptor,
                },
                (class_ref, field_ref),
            );
        }
        for method in &class.methods {
            self.methods.insert(
                MethodRef {
                    class: name,
                    name: method.name,
                    typ: method.descriptor.clone(),
                },
                method.clone(),
            );
        }

        // Superclasses
        if let Some(super_class) = class.super_class {
            let super_class = self.resolve_class(super_class)?;
            if self.classes.get(super_class).access_flags.r#final() {
                bail!("Tried to subclass final class")
            }
        }

        Ok(class_ref)
    }

    pub fn resolve_field(&mut self, field_ref: FieldRef) -> Result<(Id<Class>, Id<Field>)> {
        let class = self.resolve_class(field_ref.class)?;
        if let Some((class, field)) = self.fields.get(&field_ref) {
            return Ok((*class, *field));
        } else {
            let super_class = self.classes.get(class).super_class.ok_or_else(|| {
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
}

#[cfg(test)]
#[test]
fn test() -> Result<()> {
    let mut jvm = JVM::new("test_classes".into());
    let int = jvm.types.intern(Typ::Int);
    let method = MethodRef {
        class: jvm.strings.intern("InvokeStatic".into()),
        name: jvm.strings.intern("add".into()),
        typ: MethodDescriptor(vec![int], Some(int)),
    };
    use interp::{LocalValue, ReturnValue};
    let args = [LocalValue::Int(1)];
    assert_eq!(interp::run(&mut jvm, method, &args)?, ReturnValue::Int(43));
    Ok(())
}

#[cfg(test)]
#[test]
fn test2() -> Result<()> {
    let mut jvm = JVM::new("test_classes".into());
    let int = jvm.types.intern(Typ::Int);
    let set_method = MethodRef {
        class: jvm.strings.intern("GetStatic".into()),
        name: jvm.strings.intern("set".into()),
        typ: MethodDescriptor(vec![int], None),
    };
    let get_method = MethodRef {
        class: jvm.strings.intern("GetStatic".into()),
        name: jvm.strings.intern("get".into()),
        typ: MethodDescriptor(vec![], Some(int)),
    };
    use interp::{LocalValue, ReturnValue};
    assert_eq!(
        interp::run(&mut jvm, set_method.clone(), &[LocalValue::Int(42)])?,
        ReturnValue::Void
    );
    assert_eq!(
        interp::run(&mut jvm, get_method.clone(), &[])?,
        ReturnValue::Int(42)
    );
    Ok(())
}
