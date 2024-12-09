use std::fmt::Display;

use crate::{heap::Ptrs, object::Object, Type};

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Value {
    Ref(Option<Object>),
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
}

impl Value {
    pub fn assignable_to(self, ptrs: Ptrs, typ: &Type) -> bool {
        match (typ, self) {
            (Type::Ref(declared), Value::Ref(object))
                if object
                    .map(|o| o.class(ptrs).assignable_to(declared))
                    .unwrap_or(true) =>
            {
                true
            }
            (Type::Boolean | Type::Byte | Type::Short | Type::Char | Type::Int, Value::Int(_)) => {
                true
            }
            (Type::Long, Value::Long(_)) => true,
            (Type::Float, Value::Float(_)) => true,
            (Type::Double, Value::Double(_)) => true,
            _ => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ref(None) => write!(f, "null"),
            Self::Ref(Some(obj)) => write!(f, "@{obj:?}"),
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
