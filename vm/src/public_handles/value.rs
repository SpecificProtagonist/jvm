use crate::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Ref(Option<Object>),
    Boolean(bool),
    Byte(i8),
    Char(u16),
    Short(i16),
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
}

impl Value {
    pub(crate) fn new(jvm: &Jvm, value: value::Value, typ: &Type) -> Self {
        match value {
            value::Value::Ref(value) => {
                Self::Ref(value.map(|value| Object::new(jvm.clone(), value)))
            }
            value::Value::Int(value) => match typ {
                Type::Boolean => Self::Boolean(value != 0),
                Type::Byte => Self::Byte(value as i8),
                Type::Short => Self::Short(value as i16),
                Type::Char => Self::Char(value as u16),
                Type::Int => Self::Int(value),
                _ => unreachable!(),
            },
            value::Value::Long(value) => Self::Long(value),
            value::Value::Float(value) => Self::Float(value),
            value::Value::Double(value) => Self::Double(value),
        }
    }

    pub(crate) fn into_inner(self, jvm: &Jvm) -> value::Value {
        match self {
            Self::Ref(value) => value::Value::Ref(value.map(|value| value.as_inner(jvm))),
            Self::Boolean(value) => value::Value::Int(value as i32),
            Self::Byte(value) => value::Value::Int(value as i32),
            Self::Char(value) => value::Value::Int(value as i32),
            Self::Short(value) => value::Value::Int(value as i32),
            Self::Int(value) => value::Value::Int(value),
            Self::Long(value) => value::Value::Long(value),
            Self::Float(value) => value::Value::Float(value),
            Self::Double(value) => value::Value::Double(value),
        }
    }
}

impl core::fmt::Display for Value {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::Ref(None) => write!(f, "null"),
            Self::Ref(Some(obj)) => write!(f, "@{:?}", obj.value),
            Self::Boolean(v) => write!(f, "{}", v),
            Self::Byte(v) => write!(f, "{}", v),
            Self::Char(v) => write!(f, "{}", v),
            Self::Short(v) => write!(f, "{}", v),
            Self::Int(v) => write!(f, "{}", v),
            Self::Long(v) => write!(f, "{}", v),
            Self::Float(v) => write!(f, "{}", v),
            Self::Double(v) => write!(f, "{}", v),
        }
    }
}

macro_rules! partialeq_into_and_try_from_value {
    ($variant:ident, $typ:ty, $err:literal) => {
        impl From<$typ> for Value {
            fn from(value: $typ) -> Self {
                Self::$variant(value)
            }
        }

        impl TryFrom<Value> for $typ {
            type Error = &'static str;

            fn try_from(value: Value) -> Result<Self, Self::Error> {
                if let Value::$variant(value) = value {
                    Ok(value)
                } else {
                    Err($err)
                }
            }
        }

        // Can't do
        // `impl<'a, T: Into<Value>> PartialEq<T> for Value`
        // because that would conflict for T = Value
        impl PartialEq<$typ> for Value {
            fn eq(&self, other: &$typ) -> bool {
                self == &Into::<Value>::into(other.clone())
            }
        }

        impl PartialEq<Value> for $typ {
            fn eq(&self, other: &Value) -> bool {
                other == self
            }
        }
    };
}

partialeq_into_and_try_from_value!(Ref, Option<Object>, "Not a reference");
partialeq_into_and_try_from_value!(Boolean, bool, "Not a boolean");
partialeq_into_and_try_from_value!(Byte, i8, "Not a byte");
partialeq_into_and_try_from_value!(Char, u16, "Not a char");
partialeq_into_and_try_from_value!(Short, i16, "Not a short");
partialeq_into_and_try_from_value!(Int, i32, "Not an int");
partialeq_into_and_try_from_value!(Long, i64, "Not a long");
partialeq_into_and_try_from_value!(Float, f32, "Not a float");
partialeq_into_and_try_from_value!(Double, f64, "Not a double");

impl From<Object> for Value {
    fn from(value: Object) -> Self {
        Self::Ref(Some(value))
    }
}

impl TryFrom<Value> for Object {
    type Error = &'static str;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Value::Ref(Some(value)) = value {
            Ok(value)
        } else {
            Err("Not an object")
        }
    }
}

impl PartialEq<Object> for Value {
    fn eq(&self, other: &Object) -> bool {
        if let Value::Ref(Some(obj)) = self {
            obj == other
        } else {
            false
        }
    }
}

impl PartialEq<Value> for Object {
    fn eq(&self, other: &Value) -> bool {
        other == self
    }
}
