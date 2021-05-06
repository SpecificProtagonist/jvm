use anyhow::{anyhow, bail, Context, Result};
use bimap::BiMap;
use std::{
    collections::HashMap, fs::File, hash::Hash, io::Read, marker::PhantomData, path::PathBuf,
    rc::Rc,
};

mod interp;
mod parse;

pub struct JVM {
    class_path: PathBuf,
    strings: Interner<String>,
    types: Interner<Typ>,
    classes: Arena<Class>,
    classes_by_name: HashMap<Id<String>, Id<Class>>,
    /// Fields are stored in classes, the id is an index there
    /// As far as I can tell, JVM supports field overloading
    fields: HashMap<FieldRef, Id<Field>>,
    /// Methods are stored in classes, the id is an index there
    methods: HashMap<MethodRef, Rc<Method>>,
}

pub struct Class {
    name: Id<String>,
    super_class: Id<String>,
    version: (u16, u16),
    const_pool: ConstPool,
    access_flags: u16,
    interfaces: Vec<Id<String>>,
    fields: Arena<Field>,
    methods: Vec<Rc<Method>>,
}

// Differentiating runtime- and on-disk const pool shouldn't be neccessary
// although it would allow a speedup
struct ConstPool(Vec<ConstPoolItem>);

#[derive(Debug)]
pub enum ConstPoolItem {
    Utf8(Id<String>),
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    Class(u16),
    FieldRef(FieldRef),
    MethodRef(MethodRef),
    InterfaceMethodRef { class: u16, nat: u16 },
    PlaceholderAfterLongOrDoubleEntryOrForEntryZero,
    // These don't end up in the run-time constant pool,
    // but are needed for parsing because there's no
    // guarantee that the referenced items come first
    RawNameAndType { name: u16, descriptor: u16 },
    RawFieldRef { class: u16, nat: u16 },
    RawMethodRef { class: u16, nat: u16 },
    RawString(u16),
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
    access_flags: u16,
    descriptor: Id<Typ>,
}

pub struct Method {
    name: Id<String>,
    access_flags: u16,
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
        Self {
            class_path,
            strings: Default::default(),
            types: Default::default(),
            classes: Default::default(),
            classes_by_name: Default::default(),
            fields: Default::default(),
            methods: Default::default(),
        }
    }

    pub fn resolve_class(&mut self, name: Id<String>) -> Result<Id<Class>> {
        if let Some(class_ref) = self.classes_by_name.get(&name) {
            return Ok(*class_ref);
        }

        let mut path = self.class_path.clone();
        path.push(self.strings.get(name));
        path.set_extension("class");

        let mut bytes = Vec::new();
        File::open(path)?.read_to_end(&mut bytes)?;

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
                field_ref,
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

        Ok(class_ref)
    }
}

impl ConstPool {
    fn get_utf8(&self, index: u16) -> Result<Id<String>> {
        let item = self.0.get(index as usize);
        if let Some(ConstPoolItem::Utf8(string)) = item {
            Ok(*string)
        } else {
            bail!(
                "Constant pool index {}: Expected Utf8, got {:?}",
                index,
                item
            )
        }
    }

    fn get_class(&self, index: u16) -> Result<Id<String>> {
        let item = self.0.get(index as usize);
        if let Some(ConstPoolItem::Class(index)) = item {
            self.get_utf8(*index)
        } else {
            bail!(
                "Constant pool index {}: Expected Class, got {:?}",
                index,
                item
            )
        }
    }

    fn get_method(&self, index: u16) -> Result<&MethodRef> {
        let item = self.0.get(index as usize);
        if let Some(ConstPoolItem::MethodRef(method)) = item {
            Ok(method)
        } else {
            bail!(
                "Constant pool index {}: Expected Method, got {:?}",
                index,
                item
            )
        }
    }

    fn get_raw_nat(&self, index: u16) -> Result<(Id<String>, Id<String>)> {
        if let Some(ConstPoolItem::RawNameAndType { name, descriptor }) = self.0.get(index as usize)
        {
            Ok((self.get_utf8(*name)?, self.get_utf8(*descriptor)?))
        } else {
            bail!("Invalid class constant pool item index")
        }
    }
}

pub mod instructions {
    pub const NOP: u8 = 0;
    pub const ICONST_M1: u8 = 2;
    pub const ICONST_0: u8 = 3;
    pub const ICONST_1: u8 = 4;
    pub const ICONST_2: u8 = 5;
    pub const ICONST_3: u8 = 6;
    pub const ICONST_4: u8 = 7;
    pub const ICONST_5: u8 = 8;
    pub const BIPUSH: u8 = 16;
    pub const ILOAD: u8 = 21;
    pub const ILOAD_0: u8 = 26;
    pub const ILOAD_1: u8 = 27;
    pub const ILOAD_2: u8 = 28;
    pub const ILOAD_3: u8 = 29;
    pub const ISTORE: u8 = 54;
    pub const ISTORE_0: u8 = 59;
    pub const ISTORE_1: u8 = 60;
    pub const ISTORE_2: u8 = 61;
    pub const ISTORE_3: u8 = 62;
    pub const POP: u8 = 87;
    pub const POP2: u8 = 88;
    pub const DUP: u8 = 89;
    pub const DUP_X1: u8 = 90;
    pub const DUP_X2: u8 = 91;
    pub const DUP2: u8 = 92;
    pub const DUP2_X1: u8 = 93;
    pub const DUP2_X2: u8 = 94;
    pub const IADD: u8 = 96;
    pub const ISUB: u8 = 100;
    pub const IMUL: u8 = 104;
    pub const IDIV: u8 = 108;
    pub const IREM: u8 = 112;
    pub const INEG: u8 = 116;
    pub const ISHL: u8 = 120;
    pub const ISHR: u8 = 122;
    pub const IUSHR: u8 = 124;
    pub const IAND: u8 = 126;
    pub const IOR: u8 = 128;
    pub const IXOR: u8 = 130;
    pub const IINC: u8 = 132;
    pub const IFEQ: u8 = 153;
    pub const IFNE: u8 = 154;
    pub const IFLT: u8 = 155;
    pub const IFGE: u8 = 156;
    pub const IFGT: u8 = 157;
    pub const IFLE: u8 = 158;
    pub const IF_ICMPEQ: u8 = 159;
    pub const IF_ICMPNE: u8 = 160;
    pub const IF_ICMPLT: u8 = 161;
    pub const IF_ICMPGE: u8 = 162;
    pub const IF_ICMPGT: u8 = 163;
    pub const IF_ICMPLE: u8 = 164;
    pub const IRETURN: u8 = 172;
    pub const INVOKESTATIC: u8 = 184;
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
