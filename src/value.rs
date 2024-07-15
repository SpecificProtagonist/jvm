use std::fmt::Display;

use crate::object::Object;

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Value {
    Void,
    Ref(Option<Object>),
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Void => write!(f, "void"),
            Self::Ref(None) => write!(f, "null"),
            Self::Ref(Some(obj)) => write!(f, "@{:x}", obj.ptr()),
            Self::Int(v) => write!(f, "{}", v),
            Self::Long(v) => write!(f, "{}", v),
            Self::Float(v) => write!(f, "{}", v),
            Self::Double(v) => write!(f, "{}", v),
        }
    }
}

impl From<Option<Object>> for Value {
    fn from(val: Option<Object>) -> Self {
        Self::Ref(val)
    }
}

impl From<Object> for Value {
    fn from(val: Object) -> Self {
        Self::Ref(Some(val))
    }
}

impl From<i32> for Value {
    fn from(val: i32) -> Self {
        Self::Int(val)
    }
}

impl From<i64> for Value {
    fn from(val: i64) -> Self {
        Self::Long(val)
    }
}

impl From<f32> for Value {
    fn from(val: f32) -> Self {
        Self::Float(val)
    }
}

impl From<f64> for Value {
    fn from(val: f64) -> Self {
        Self::Double(val)
    }
}
