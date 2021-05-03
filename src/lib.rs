use anyhow::{bail, Context, Result};
use bimap::BiMap;
use std::{cell::RefCell, collections::HashMap};

mod interp;
mod parse;

#[derive(Default)]
pub struct JVM {
    interned_strings: StringInterner,
    classes: HashMap<InternedString, Classfile>,
}

impl JVM {
    pub fn load_class(&mut self, bytes: &[u8]) -> Result<InternedString> {
        let class = parse::read_class_file(bytes, &mut self.interned_strings)?;
        let name = class.name;
        self.classes.insert(name, class);
        Ok(name)
    }
}

#[derive(Default)]
pub struct StringInterner(BiMap<String, InternedString>);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct InternedString(u32);

impl StringInterner {
    pub fn intern(&mut self, string: String) -> InternedString {
        if let Some(interned) = self.0.get_by_left(&string) {
            *interned
        } else {
            let interned = InternedString(self.0.len() as u32);
            self.0.insert(string, interned);
            interned
        }
    }

    pub fn get(&self, string: &str) -> Option<InternedString> {
        self.0.get_by_left(string).copied()
    }

    pub fn get_str(&self, interned: InternedString) -> &str {
        self.0
            .get_by_right(&interned)
            .expect("Tried to retrieve string from from interner")
    }
}

pub struct Classfile {
    name: InternedString,
    super_class: InternedString,
    version: (u16, u16),
    const_pool: ConstPool,
    access_flags: u16,
    interfaces: Vec<u16>,
    fields: HashMap<InternedString, Field>,
    methods: HashMap<InternedString, Method>,
}

struct ConstPool(Vec<ConstPoolItem>);

impl ConstPool {
    fn get_utf8(&self, index: u16) -> Result<InternedString> {
        if let Some(ConstPoolItem::Utf8(string)) = self.0.get(index as usize) {
            Ok(*string)
        } else {
            bail!("Invalid utf8 constant pool item index")
        }
    }

    fn get_string(&self, index: u16) -> Result<InternedString> {
        if let Some(ConstPoolItem::String(index)) = self.0.get(index as usize) {
            self.get_utf8(*index)
        } else {
            bail!("Invalid string constant pool item index")
        }
    }

    fn get_class(&self, index: u16) -> Result<InternedString> {
        if let Some(ConstPoolItem::Class(index)) = self.0.get(index as usize) {
            self.get_utf8(*index)
        } else {
            bail!("Invalid class constant pool item index")
        }
    }
}

pub enum ConstPoolItem {
    Utf8(InternedString),
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    Class(u16),
    String(u16),
    FieldRef { class: u16, nat: u16 },
    MethodRef { class: u16, nat: u16 },
    InterfaceMethodRef { class: u16, nat: u16 },
    NameAndType { name: u16, descriptor: u16 },
    /* After 51.0:
    MethodHandle {kind: u8, index: u16},
    MethodType(u16),
    Dynamic,
    InvokeDynamic,
    Module,
    Package,
    */
    DumbPlaceholderAfterLongOrDoubleEntryOrForEntryZero,
}

pub struct Field {
    class: InternedString,
    name: InternedString,
    access_flags: u16,
    descriptor: u16,
}

pub struct Method {
    class: InternedString,
    name: InternedString,
    access_flags: u16,
    descriptor: u16,
    code: Option<Code>,
}

pub struct Code {
    max_stack: u16,
    max_locals: u16,
    bytes: Vec<u8>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Value {
    Uninitialized,
    Ref(/*TODO*/),
    ReturnAddress(u16),
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
}

impl Default for Value {
    fn default() -> Self {
        Value::Uninitialized
    }
}

// I'd use TryFrom, but then type interference fails
impl From<Value> for Result<i32> {
    fn from(value: Value) -> Self {
        if let Value::Int(value) = value {
            Ok(value)
        } else {
            bail!("Value not an int")
        }
    }
}

mod instructions {
    pub const ICONST_M1: u8 = 2;
    pub const ICONST_0: u8 = 3;
    pub const ICONST_1: u8 = 4;
    pub const ICONST_2: u8 = 5;
    pub const ICONST_3: u8 = 6;
    pub const ICONST_4: u8 = 7;
    pub const ICONST_5: u8 = 8;
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
    pub const IRETURN: u8 = 172;
}

#[cfg(test)]
#[test]
fn test() -> Result<()> {
    let mut jvm = JVM::default();
    let class = jvm.load_class(include_bytes!("../test_files/Add.class"))?;
    let method = &jvm.classes[&class].methods[&jvm.interned_strings.get("add").unwrap()];
    let args = [Value::Int(1), Value::Int(2)];
    assert_eq!(interp::run(&jvm, method, &args)?, Some(Value::Int(3)));

    Ok(())
}
