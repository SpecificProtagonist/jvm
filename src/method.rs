use std::sync::{
    atomic::{AtomicPtr, Ordering},
    Arc, RwLock,
};

use hashbrown::Equivalent;

use crate::{
    class::{Class, ClassPtr},
    typ::Typ,
    AccessFlags, Jvm,
};

pub(crate) struct Method {
    pub(crate) nat: MethodNaT,
    pub(crate) access_flags: AccessFlags,
    /// Class is behind a AtomicCell to enable circular references
    pub(crate) class: AtomicPtr<Class>,
    pub(crate) code: RwLock<Code>,
}

impl Method {
    pub fn class(&self) -> ClassPtr {
        // SAFETY: This reference will not outlive the JVM
        // and this function will only be called after class has been set
        unsafe { &*self.class.load(Ordering::SeqCst) }
    }
}

/// Contains a method's name and type. Uniquely identifies method within class.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) struct MethodNaT {
    pub name: Arc<str>,
    pub typ: MethodDescriptor,
}

#[derive(Hash)]
pub(crate) struct MethodNaTBorrow<'a> {
    pub name: &'a str,
    pub typ: &'a MethodDescriptor,
}

impl<'a> Equivalent<MethodNaT> for MethodNaTBorrow<'a> {
    fn equivalent(&self, key: &MethodNaT) -> bool {
        (self.name == key.name.as_ref()) & (self.typ == &key.typ)
    }
}

/// A method signature type signature
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MethodDescriptor {
    pub args: Vec<Typ>,
    pub returns: Option<Typ>,
}

impl MethodDescriptor {
    pub(crate) fn arg_slots(&self) -> usize {
        self.args
            .iter()
            .map(|typ| match typ {
                Typ::Long | Typ::Double => 2,
                _ => 1,
            })
            .sum()
    }
}

pub(crate) enum Code {
    None,
    /// When also accessing the constant pool, lock the constant pool first
    Bytecode(Bytecode),
    Native(NativeCode),
}

pub type NativeCode =
    Box<dyn Fn(&Jvm, &[crate::Value]) -> crate::JvmResult<crate::Value> + Send + Sync>;

pub(crate) struct Bytecode {
    pub max_stack: u16,
    pub max_locals: u16,
    pub bytes: Vec<u8>,
    pub stack_map_table: Option<Vec<u8>>,
    pub exception_table: Vec<ExceptionHandler>,
}

pub(crate) struct ExceptionHandler {
    pub start_pc: u16,
    pub end_pc: u16,
    pub handler_pc: u16,
    pub catch_type: u16,
}

pub(crate) type MethodPtr = &'static Method;

impl Eq for MethodPtr {}
impl PartialEq for MethodPtr {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(*self, *other)
    }
}

impl std::hash::Hash for MethodPtr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::hash::Hash::hash(&(*self as *const _ as usize), state)
    }
}

impl std::fmt::Display for Method {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ret) = &self.nat.typ.returns {
            write!(f, "{}", ret)?;
        } else {
            write!(f, "void")?;
        }
        write!(f, " {}.{}(", self.class(), self.nat.name)?;
        let mut first = true;
        for arg in &self.nat.typ.args {
            if !first {
                write!(f, ", ")?;
            }
            first = false;
            write!(f, "{}", arg)?;
        }
        write!(f, ")")?;
        Ok(())
    }
}

impl std::fmt::Debug for Method {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl std::fmt::Display for MethodNaT {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ret) = &self.typ.returns {
            write!(f, "{}", ret)?;
        } else {
            write!(f, "void")?;
        }
        write!(f, " {}(", self.name)?;
        let mut first = true;
        for arg in &self.typ.args {
            if !first {
                write!(f, ", ")?;
            }
            first = false;
            write!(f, "{}", arg)?;
        }
        write!(f, ")")?;
        Ok(())
    }
}
