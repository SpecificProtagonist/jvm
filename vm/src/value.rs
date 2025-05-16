use core::fmt::Display;

use crate::{Type, heap::Ptrs, object::Object};

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Value {
    Object(Option<Object>),
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
}

impl Value {
    pub fn assignable_to(self, ptrs: Ptrs, typ: &Type) -> bool {
        match (typ, self) {
            (Type::Ref(declared), Value::Object(object))
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
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Object(None) => write!(f, "null"),
            Self::Object(Some(obj)) => write!(f, "@{obj:?}"),
            Self::Int(v) => write!(f, "{v}"),
            Self::Long(v) => write!(f, "{v}"),
            Self::Float(v) => write!(f, "{v}"),
            Self::Double(v) => write!(f, "{v}"),
        }
    }
}

impl From<Option<Object>> for Value {
    fn from(val: Option<Object>) -> Self {
        Self::Object(val)
    }
}

impl From<Object> for Value {
    fn from(val: Object) -> Self {
        Self::Object(Some(val))
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
