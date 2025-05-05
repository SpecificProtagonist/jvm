use alloc::{collections::BTreeMap, sync::Arc};
use core::{
    cell::SyncUnsafeCell,
    sync::atomic::{AtomicPtr, Ordering},
};
use std::{collections::HashMap, sync::RwLock};

use hashbrown::Equivalent;

use crate::{
    AccessFlags,
    annotations::Annotation,
    class::{Class, ClassRef},
    interpreter::TWO_STACK_SLOTS_PER_LONG_AND_DOUBLES,
    typ::Type,
};

pub(crate) struct Method {
    pub(crate) nat: MethodNaT,
    pub(crate) access_flags: AccessFlags,
    /// Class is behind a AtomicCell to enable circular references
    pub(crate) class: AtomicPtr<Class>,
    pub(crate) code: RwLock<Code>,
    /// Declared exceptions represented as an index into the constant pool
    pub(crate) exceptions: Vec<u16>,
    pub(crate) signature: Option<Arc<str>>,
    pub(crate) deprecated: bool,
    #[allow(dead_code)]
    pub(crate) runtime_visible_annotations: Vec<Annotation>,
    #[allow(dead_code)]
    pub(crate) runtime_visible_parameter_annotations: Vec<Vec<Annotation>>,
    pub(crate) extra_attributes: HashMap<String, Box<[u8]>>,
}

impl Method {
    pub fn class(&self) -> ClassRef {
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

impl Equivalent<MethodNaT> for MethodNaTBorrow<'_> {
    fn equivalent(&self, key: &MethodNaT) -> bool {
        (self.name == key.name.as_ref()) & (self.typ == &key.typ)
    }
}

/// A method type signature. Does not include declared exceptions.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MethodDescriptor {
    pub args: Vec<Type>,
    pub returns: Option<Type>,
}

impl MethodDescriptor {
    pub(crate) fn arg_slots(&self) -> usize {
        if TWO_STACK_SLOTS_PER_LONG_AND_DOUBLES {
            self.args
                .iter()
                .map(|typ| match typ {
                    Type::Long | Type::Double => 2,
                    _ => 1,
                })
                .sum()
        } else {
            self.args.len()
        }
    }
}

pub(crate) enum Code {
    None,
    /// When also accessing the constant pool, lock the constant pool first
    Bytecode(Bytecode),
    Native(NativeCode),
}

pub type NativeCode = Box<
    dyn Fn(&crate::Jvm, &[crate::Value]) -> crate::JvmResult<Option<crate::Value>> + Send + Sync,
>;

pub(crate) struct Bytecode {
    pub max_stack: u16,
    pub max_locals: u16,
    /// !Freeze to allow replacing instructions with optimized ones during interpretation.
    pub bytes: Vec<SyncUnsafeCell<u8>>,
    pub stack_map_table: Option<Vec<u8>>,
    pub exception_table: Vec<ExceptionHandler>,
    /// Map from pc to source code line number. May be empty.
    #[allow(dead_code)]
    pub line_number_table: BTreeMap<u16, u16>,
    #[allow(dead_code)]
    pub local_variables: Vec<LocalVariable>,
    #[allow(dead_code)]
    pub local_variable_types: Vec<LocalVariableType>,
}

pub(crate) struct ExceptionHandler {
    pub start_pc: u16,
    pub end_pc: u16,
    pub handler_pc: u16,
    pub catch_type: u16,
}

pub(crate) struct LocalVariable {
    #[allow(dead_code)]
    pub start_pc: u16,
    #[allow(dead_code)]
    pub end_pc: u16,
    #[allow(dead_code)]
    pub name_index: u16,
    #[allow(dead_code)]
    pub type_index: u16,
    #[allow(dead_code)]
    pub locals_index: u16,
}

pub(crate) struct LocalVariableType {
    #[allow(dead_code)]
    pub start_pc: u16,
    #[allow(dead_code)]
    pub end_pc: u16,
    #[allow(dead_code)]
    pub name_index: u16,
    #[allow(dead_code)]
    pub signature_index: u16,
    #[allow(dead_code)]
    pub locals_index: u16,
}

pub(crate) type MethodRef = &'static Method;

impl Eq for MethodRef {}
impl PartialEq for MethodRef {
    fn eq(&self, other: &Self) -> bool {
        core::ptr::eq(*self, *other)
    }
}

impl core::hash::Hash for MethodRef {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        core::hash::Hash::hash(&(*self as *const _ as usize), state)
    }
}

impl core::fmt::Display for Method {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
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

impl core::fmt::Debug for Method {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{self}")
    }
}

impl core::fmt::Display for MethodNaT {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
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
