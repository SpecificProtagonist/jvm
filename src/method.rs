use std::sync::Arc;

use crossbeam_utils::atomic::AtomicCell;

use crate::{AccessFlags, Class, Typ};

pub struct Method<'a> {
    pub(crate) nat: MethodNaT<'a>,
    pub(crate) access_flags: AccessFlags,
    /// Class is behind a AtomicCell to enable circular references
    pub(crate) class: AtomicCell<&'a Class<'a>>,
    pub(crate) code: Option<Code>,
}

impl<'a> Method<'a> {
    pub fn nat(&self) -> MethodNaT {
        self.nat.clone()
    }

    pub fn class(&self) -> &'a Class {
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

impl<'a> Eq for &'a Method<'a> {}
impl<'a> PartialEq for &'a Method<'a> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(*self, *other)
    }
}

impl<'a> std::fmt::Debug for Method<'a> {
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
