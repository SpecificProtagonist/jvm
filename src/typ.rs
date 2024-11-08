use std::sync::{
    atomic::{AtomicU16, AtomicU32, AtomicU64, AtomicU8},
    Arc,
};

use crate::heap::AtomicJvmPtr;

/// A Java type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
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
    /// A reference type (class, interface, enum or array).
    Ref(Arc<str>),
}

impl Type {
    pub fn size(&self) -> usize {
        use Type::*;
        match self {
            Boolean | Byte => size_of::<AtomicU8>(),
            Short | Char => size_of::<AtomicU16>(),
            Int | Float => size_of::<AtomicU32>(),
            Long | Double => size_of::<AtomicU64>(),
            Ref(..) => size_of::<AtomicJvmPtr>(),
        }
    }

    pub fn array_dimensions(&self) -> usize {
        match self {
            Self::Ref(name) => name.chars().filter(|&c| c == '[').count(),
            _ => 0,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Boolean => write!(f, "boolean"),
            Type::Byte => write!(f, "byte"),
            Type::Short => write!(f, "short"),
            Type::Char => write!(f, "char"),
            Type::Int => write!(f, "int"),
            Type::Long => write!(f, "long"),
            Type::Float => write!(f, "float"),
            Type::Double => write!(f, "double"),
            Type::Ref(name) => write!(f, "{}", name),
        }
    }
}

impl<'a> From<&'a str> for Type {
    fn from(value: &'a str) -> Self {
        match value {
            "boolean" => Type::Boolean,
            "byte" => Type::Byte,
            "short" => Type::Short,
            "char" => Type::Char,
            "int" => Type::Int,
            "long" => Type::Long,
            "float" => Type::Float,
            "double" => Type::Double,
            class => Type::Ref(class.into()),
        }
    }
}
