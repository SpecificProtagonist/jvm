use std::sync::Arc;

use crossbeam_utils::atomic::AtomicCell;

use crate::{class::Class, typ::Typ, AccessFlags};

pub(crate) struct Method {
    pub(crate) nat: MethodNaT<'static>,
    pub(crate) access_flags: AccessFlags,
    /// Class is behind a AtomicCell to enable circular references
    pub(crate) class: AtomicCell<&'static Class>,
    pub(crate) code: Option<Code>,
}

impl Method {
    pub fn nat(&self) -> MethodNaT {
        self.nat.clone()
    }

    pub fn class(&self) -> &'static Class {
        self.class.load()
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct MethodNaT<'a> {
    pub name: Arc<str>,
    pub typ: &'a MethodDescriptor,
}

/// Argument and return types
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MethodDescriptor(pub Vec<Typ>, pub Option<Typ>);

impl MethodDescriptor {
    pub(crate) fn arg_slots(&self) -> usize {
        self.0
            .iter()
            .map(|typ| match typ {
                Typ::Long | Typ::Double => 2,
                _ => 1,
            })
            .sum()
    }
}

// TODO: use verification data to construct a more efficient bytecode
pub(crate) struct Code {
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

impl Eq for &'static Method {}
impl PartialEq for &'static Method {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(*self, *other)
    }
}

impl<'a> std::hash::Hash for &'a Method {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::hash::Hash::hash(&(*self as *const _ as usize), state)
    }
}

impl std::fmt::Debug for Method {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.nat)
    }
}

impl<'a> std::fmt::Display for MethodNaT<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ret) = &self.typ.1 {
            write!(f, "{}", ret)?;
        } else {
            write!(f, "void")?;
        }
        write!(f, " {}(", self.name)?;
        let mut first = true;
        for arg in &self.typ.0 {
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