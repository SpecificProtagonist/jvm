use std::alloc::Layout;

use crate::{heap::JVMPtr, IntStr};

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
    /// Classes are represented unresolved so we can talk about classes without
    /// having all classes resolved already (which wouldn't work as the graph can be cyclic)
    Ref(IntStr<'a>),
}

impl<'a> Typ<'a> {
    pub fn layout(&self) -> Layout {
        use Typ::*;
        match self {
            Bool | Byte => Layout::new::<u8>(),
            Short | Char => Layout::new::<u16>(),
            Int | Float => Layout::new::<u32>(),
            Long | Double => Layout::new::<u64>(),
            Ref(..) => Layout::new::<JVMPtr>(),
        }
    }

    pub fn array_dimensions(&self) -> usize {
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
