use std::{alloc::Layout, sync::Arc};

use crate::heap::JVMPtr;

/// A Java type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Typ {
    Boolean,
    Byte,
    Short,
    Char,
    Int,
    Long,
    Float,
    Double,
    // Classes are represented unresolved so we can talk about classes without
    // having all classes resolved already (which wouldn't work as the graph can be cyclic)
    /// A reference type (class, interface (TODO), enum or array).
    Ref(Arc<str>),
}

impl Typ {
    pub fn layout(&self) -> Layout {
        use Typ::*;
        match self {
            Boolean | Byte => Layout::new::<u8>(),
            Short | Char => Layout::new::<u16>(),
            Int | Float => Layout::new::<u32>(),
            Long | Double => Layout::new::<u64>(),
            Ref(..) => Layout::new::<JVMPtr>(),
        }
    }

    pub fn array_dimensions(&self) -> usize {
        match self {
            Self::Ref(name) => name.chars().filter(|&c| c == '[').count(),
            _ => 0,
        }
    }
}

impl std::fmt::Display for Typ {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Typ::Boolean => write!(f, "boolean"),
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

impl<'a> From<&'a str> for Typ {
    fn from(value: &'a str) -> Self {
        match value {
            "boolean" => Typ::Boolean,
            "byte" => Typ::Byte,
            "short" => Typ::Short,
            "char" => Typ::Char,
            "int" => Typ::Int,
            "long" => Typ::Long,
            "float" => Typ::Float,
            "double" => Typ::Double,
            class => Typ::Ref(class.into()),
        }
    }
}
