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
    pub const LCONST_0: u8 = 9;
    pub const LCONST_1: u8 = 10;
    pub const FCONST_0: u8 = 11;
    pub const FCONST_1: u8 = 12;
    pub const FCONST_2: u8 = 13;
    pub const DCONST_0: u8 = 14;
    pub const DCONST_1: u8 = 15;
    pub const BIPUSH: u8 = 16;
    pub const SIPUSH: u8 = 17;
    pub const ILOAD: u8 = 21;
    pub const LLOAD: u8 = 22;
    pub const FLOAD: u8 = 23;
    pub const DLOAD: u8 = 24;
    pub const ILOAD_0: u8 = 26;
    pub const ILOAD_1: u8 = 27;
    pub const ILOAD_2: u8 = 28;
    pub const ILOAD_3: u8 = 29;
    pub const LLOAD_0: u8 = 30;
    pub const LLOAD_1: u8 = 31;
    pub const LLOAD_2: u8 = 32;
    pub const LLOAD_3: u8 = 33;
    pub const FLOAD_0: u8 = 34;
    pub const FLOAD_1: u8 = 35;
    pub const FLOAD_2: u8 = 36;
    pub const FLOAD_3: u8 = 37;
    pub const DLOAD_0: u8 = 38;
    pub const DLOAD_1: u8 = 39;
    pub const DLOAD_2: u8 = 40;
    pub const DLOAD_3: u8 = 41;
    pub const ISTORE: u8 = 54;
    pub const LSTORE: u8 = 55;
    pub const FSTORE: u8 = 56;
    pub const DSTORE: u8 = 57;
    pub const ISTORE_0: u8 = 59;
    pub const ISTORE_1: u8 = 60;
    pub const ISTORE_2: u8 = 61;
    pub const ISTORE_3: u8 = 62;
    pub const LSTORE_0: u8 = 63;
    pub const LSTORE_1: u8 = 64;
    pub const LSTORE_2: u8 = 65;
    pub const LSTORE_3: u8 = 66;
    pub const FSTORE_0: u8 = 67;
    pub const FSTORE_1: u8 = 68;
    pub const FSTORE_2: u8 = 69;
    pub const FSTORE_3: u8 = 70;
    pub const DSTORE_0: u8 = 71;
    pub const DSTORE_1: u8 = 72;
    pub const DSTORE_2: u8 = 73;
    pub const DSTORE_3: u8 = 74;
    pub const POP: u8 = 87;
    pub const POP2: u8 = 88;
    pub const DUP: u8 = 89;
    pub const DUP_X1: u8 = 90;
    pub const DUP_X2: u8 = 91;
    pub const DUP2: u8 = 92;
    pub const DUP2_X1: u8 = 93;
    pub const DUP2_X2: u8 = 94;
    pub const SWAP: u8 = 95;
    pub const IADD: u8 = 96;
    pub const LADD: u8 = 97;
    pub const FADD: u8 = 98;
    pub const DADD: u8 = 99;
    pub const ISUB: u8 = 100;
    pub const LSUB: u8 = 101;
    pub const FSUB: u8 = 102;
    pub const DSUB: u8 = 103;
    pub const IMUL: u8 = 104;
    pub const LMUL: u8 = 105;
    pub const FMUL: u8 = 106;
    pub const DMUL: u8 = 107;
    pub const IDIV: u8 = 108;
    pub const LDIV: u8 = 109;
    pub const FDIV: u8 = 110;
    pub const DDIV: u8 = 111;
    pub const IREM: u8 = 112;
    pub const LREM: u8 = 113;
    pub const FREM: u8 = 114;
    pub const DREM: u8 = 115;
    pub const INEG: u8 = 116;
    pub const LNEG: u8 = 117;
    pub const FNEG: u8 = 118;
    pub const DNEG: u8 = 119;
    pub const ISHL: u8 = 120;
    pub const LSHL: u8 = 121;
    pub const ISHR: u8 = 122;
    pub const LSHR: u8 = 123;
    pub const IUSHR: u8 = 124;
    pub const LUSHR: u8 = 125;
    pub const IAND: u8 = 126;
    pub const LAND: u8 = 127;
    pub const IOR: u8 = 128;
    pub const LOR: u8 = 129;
    pub const IXOR: u8 = 130;
    pub const LXOR: u8 = 131;
    pub const IINC: u8 = 132;
    pub const I2L: u8 = 133;
    pub const I2F: u8 = 134;
    pub const I2D: u8 = 135;
    pub const L2I: u8 = 136;
    pub const L2F: u8 = 137;
    pub const L2D: u8 = 138;
    pub const F2I: u8 = 139;
    pub const F2L: u8 = 140;
    pub const F2D: u8 = 141;
    pub const D2I: u8 = 142;
    pub const D2L: u8 = 143;
    pub const D2F: u8 = 144;
    pub const I2B: u8 = 145;
    pub const I2C: u8 = 146;
    pub const I2S: u8 = 147;
    pub const LCMP: u8 = 148;
    pub const FCMPL: u8 = 149;
    pub const FCMPG: u8 = 150;
    pub const DCMPL: u8 = 151;
    pub const DCMPG: u8 = 152;
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
    pub const GOTO: u8 = 167;
    pub const JSR: u8 = 168;
    pub const RET: u8 = 169;
    pub const TABLESWITCH: u8 = 170;
    pub const LOOKUPSWITCH: u8 = 171;
    pub const IRETURN: u8 = 172;
    pub const LRETURN: u8 = 173;
    pub const FRETURN: u8 = 174;
    pub const DRETURN: u8 = 175;
    pub const RETURN: u8 = 177;
    pub const INVOKESTATIC: u8 = 184;
    pub const GOTO_W: u8 = 200;
    pub const JSR_W: u8 = 201;
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
    let bool = jvm.types.intern(Typ::Bool);
    let method = MethodRef {
        class: jvm.strings.intern("ControlFlow".into()),
        name: jvm.strings.intern("test".into()),
        typ: MethodDescriptor(vec![int], Some(bool)),
    };
    use interp::{LocalValue, ReturnValue};
    assert_eq!(
        interp::run(&mut jvm, method.clone(), &[LocalValue::Int(1)])?,
        ReturnValue::Int(1)
    );
    let args = [LocalValue::Int(2)];
    assert_eq!(
        interp::run(&mut jvm, method.clone(), &args)?,
        ReturnValue::Int(0)
    );
    let args = [LocalValue::Int(3)];
    assert_eq!(
        interp::run(&mut jvm, method.clone(), &args)?,
        ReturnValue::Int(0)
    );
    assert_eq!(
        interp::run(&mut jvm, method.clone(), &[LocalValue::Int(4)])?,
        ReturnValue::Int(1)
    );
    Ok(())
}
