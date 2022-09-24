#![feature(hash_set_entry)]
#![feature(map_first_last)]
#![feature(default_free_fn)]

mod class;
mod class_loader;
mod const_pool;
mod field;
mod field_storage;
mod heap;
mod instructions;
mod interp;
mod jvm;
mod method;
mod object;
mod parse;
mod typ;
mod verification;

use std::{
    fmt::{Debug, Display},
    hash::Hash,
    sync::atomic::Ordering,
};

// @next: ArrayStoreException
pub use class_loader::{ClassLoader, DefaultClassLoader};
pub use field::FieldNaT;
pub use method::{MethodDescriptor, MethodNaT};
pub use typ::Typ;

pub struct Jvm(jvm::Jvm);

impl Jvm {
    pub fn new(base_class_loader: Box<dyn ClassLoader>) -> Self {
        Self(jvm::Jvm::new(base_class_loader))
    }

    /// Bypasses bytecode verification. This is currently necessary
    /// because it is not yet implemented for all opcodes.
    /// # Safety
    /// Undefined behavior if any invalid class gets initialized
    pub unsafe fn disable_verification_by_type_checking(&self) {
        self.0
            .verification_type_checking_disabled
            .store(true, Ordering::SeqCst);
    }

    pub fn enable_verification_by_type_checking(&self) {
        self.0
            .verification_type_checking_disabled
            .store(false, Ordering::SeqCst)
    }

    // TODO: don't use internal name
    /// Returns the class, loading it if it wasn't already loaded.
    /// This also loads its superclass and any interfaces it implements, but not any other class it refers to.
    /// Does not initialize this class, nor load any other class referenced.
    /// The class name is in the internal form, which uses / to define hierarchies.
    pub fn resolve_class<'a>(&'a self, name: &str) -> JVMResult<'a, Class<'a>> {
        self.0
            .resolve_class(name)
            .map(|value| Class {
                jvm: &self.0,
                value,
            })
            .map_err(|value| Object::new(&self.0, value))
    }

    /// Convenience method.
    /// Includes inherited dynamic methods.
    pub fn resolve_method<'a>(
        &'a self,
        class: &str,
        method: &str,
        args: Vec<Typ>,
        returns: Option<Typ>,
    ) -> JVMResult<'a, Method<'a>> {
        self.0
            .resolve_class(class)
            .map_err(|value| Object::new(&self.0, value))?
            .methods
            .get(&MethodNaT {
                name: method.into(),
                typ: &MethodDescriptor(args, returns),
            })
            .ok_or_else(|| jvm::exception(&self.0, "NoSuchMethodError"))
            .map(|value| Method {
                jvm: &self.0,
                value,
            })
            .map_err(|value| Object::new(&self.0, value))
    }
}

macro_rules! guard_impl_trait {
    ($guard_typ:ident, Debug) => {
        impl<'a> Debug for $guard_typ<'a> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                Debug::fmt(&self.value, f)
            }
        }
    };
    ($guard_typ:ident, Display) => {
        impl<'a> Display for $guard_typ<'a> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                Display::fmt(&self.value, f)
            }
        }
    };
    ($guard_typ:ident, Hash) => {
        impl<'a> Hash for $guard_typ<'a> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.value.hash(state);
            }
        }
    };
    ($guard_typ:ident, Eq) => {
        impl<'a> Eq for $guard_typ<'a> {
            fn assert_receiver_is_total_eq(&self) {}
        }
    };

    ($guard_typ:ident, PartialEq) => {
        impl<'a> PartialEq for $guard_typ<'a> {
            fn eq(&self, other: &Self) -> bool {
                self.value == other.value
            }
        }
    };
    ($guard_typ:ident, Copy) => {
        impl<'a> Copy for $guard_typ<'a> {}
    };
    ($guard_typ:ident, Clone) => {
        impl<'a> Clone for $guard_typ<'a> {
            fn clone(&self) -> Self {
                Self {
                    value: self.value.clone(),
                    jvm: self.jvm,
                }
            }
        }
    };
}

macro_rules! define_guard {
    ($(#[$doc:meta])* $guard_typ:ident, $impl_typ:ty, $($t:ident),*) => {
        $(#[$doc])*
        pub struct $guard_typ<'a> {
            value: $impl_typ,
            jvm: &'a jvm::Jvm,
        }

        impl<'a> $guard_typ<'a> {
            fn new(jvm: &'a jvm::Jvm, value: $impl_typ) -> Self {
                Self { value, jvm }
            }

            #[allow(dead_code)]
            fn into_inner(self, jvm: &jvm::Jvm) -> $impl_typ {
                if self.jvm.id == jvm.id {
                    self.value
                } else {
                    panic!("Wrong JVM accessed")
                }
            }
        }

        $(guard_impl_trait!($guard_typ, $t);)*
    };
}

define_guard!(
    /// A Java object, guaranteed to be non-null.
    /// An object may only be used with the jvm by which it was created.
    Object,
    object::Object,
    Debug,
    Copy,
    Clone,
    Eq,
    PartialEq,
    Hash
);

impl<'a> Object<'a> {
    pub fn class(self) -> Class<'a> {
        Class::new(self.jvm, self.value.class())
    }

    pub fn array_len(self) -> Option<i32> {
        self.value.array_len()
    }

    /// Returns either the value at index
    /// # Panics
    /// if the object is not an array or the index is out of bounds
    pub fn array_read(self, index: i32) -> Value<'a> {
        Value::new(self.jvm, self.value.array_read(index))
    }

    /// Sets an array element. If the element type is boolean, byte, short or char, the `JVMValue::Int` is truncated
    /// Panics if object is not an array, the element type mismatches, the index is out of bounds
    /// or the value is an object created by another JVM
    pub fn array_write(self, index: i32, value: Value<'a>) {
        self.value.array_write(index, value.into_inner(self.jvm))
    }
}

define_guard!(
    Class,
    &'static class::Class,
    Debug,
    Copy,
    Clone,
    Eq,
    PartialEq,
    Hash
);

impl<'a> Class<'a> {
    /// Returns the fully qualified internal name (using '/' instead of '.')
    pub fn name(&self) -> &str {
        &self.value.name
    }

    /// Is `Some()` for all classes except `java.lang.Object`
    pub fn super_class(&self) -> Option<Self> {
        self.value
            .super_class
            .map(|value| Class::new(self.jvm, value))
    }

    /// Is `Some()` for classes representing an array and `None` otherwise
    pub fn element_type(&self) -> &Option<Typ> {
        &self.value.element_type
    }

    /// Ensures this class is initialized. Includes linking, verification, preparation & resolution.
    /// This happens automatically when a method belonging to this class is called.
    pub fn ensure_init(&self) -> JVMResult<'a, ()> {
        self.value
            .ensure_init(self.jvm)
            .map_err(|value| Object::new(self.jvm, value))
    }

    /// Creates an instance of a non-array class on the heap
    /// # Panics
    /// Panics if this is an array class
    pub fn create_object(&self) -> Object {
        Object::new(self.jvm, self.jvm.create_object(self.value))
    }

    /// Creates an instance of an array class on the heap
    /// # Panics
    /// Panics if class is not an array class or length < 0
    pub fn create_array(&self, length: i32) -> Object {
        Object::new(self.jvm, self.jvm.create_array(self.value, length))
    }

    pub fn field(&self, name: &str, typ: Typ) -> Option<Field<'a>> {
        self.field_by_nat(FieldNaT {
            name: name.into(),
            typ,
        })
    }

    pub fn field_by_nat(&self, nat: FieldNaT) -> Option<Field<'a>> {
        self.value
            .field_by_nat(nat)
            .map(|value| Field::new(self.jvm, value))
    }

    /// Includes inherited dynamic methods.
    pub fn method(self, name: &str, args: Vec<Typ>, ret: Option<Typ>) -> Option<Method<'a>> {
        self.method_by_nat(&MethodNaT {
            name: name.into(),
            typ: &MethodDescriptor(args, ret),
        })
    }

    pub fn method_by_nat(self, nat: &MethodNaT) -> Option<Method<'a>> {
        self.value
            .methods
            .get(nat)
            .map(|value| Method::new(self.jvm, value))
    }
}

define_guard!(
    Field,
    &'static field::Field,
    Debug,
    Copy,
    Clone,
    Eq,
    PartialEq,
    Hash
);

impl<'a> Field<'a> {
    pub fn class(&self) -> Class {
        Class::new(self.jvm, self.value.class())
    }

    pub fn access_flags(&self) -> AccessFlags {
        self.value.access_flags
    }

    pub fn name_and_type(&self) -> &FieldNaT {
        &self.value.nat
    }

    /// Get the value of a static field.
    /// Note that this might not be initialized yet unless a method has been called on this class.
    /// # Panics
    /// Panics if the field is not static.
    pub fn static_get(&self) -> Value<'a> {
        Value::new(self.jvm, self.value.static_get())
    }

    /// Get the value of an object's field.
    /// # Panics
    /// Panics if the field is static or the object is not an instance of the class corresponding to this field.
    pub fn instance_get(&self, object: Object) -> Value<'a> {
        Value::new(
            self.jvm,
            self.value.instance_get(object.into_inner(self.jvm)),
        )
    }

    /// Set the value of a static field.
    /// # Panics
    /// Panics if the field is not static or the value is not assignable to the field's type
    /// or the value is an object created by a different JVM
    pub fn static_set(&self, value: Value<'a>) {
        self.value.static_set(value.into_inner(self.jvm))
    }

    /// Get the value of an object's field.
    /// # Panics
    /// Panics if the field is static,
    /// the object is not an instance of the class corresponding to this field,
    /// the value is not assignable to the field's type
    /// or the value is an object created by a different JVM
    pub fn instance_set(&self, object: Object, value: Value<'a>) {
        self.value
            .instance_set(object.into_inner(self.jvm), value.into_inner(self.jvm))
    }
}

define_guard!(
    Method,
    &'static method::Method,
    Debug,
    Copy,
    Clone,
    Eq,
    PartialEq,
    Hash
);

impl<'a> Method<'a> {
    pub fn class(&self) -> Class {
        Class::new(self.jvm, self.value.class())
    }

    pub fn name_and_type(&self) -> MethodNaT<'a> {
        self.value.nat()
    }

    // TODO: publicly accessible dynamic method resolution
    /// Calls the method, initializing its class if necessary.
    /// Returns either the function's return value or the exception that was thrown
    /// # Panics
    /// Panics if any of the [`Value`]s is an object created by another JVM
    pub fn invoke(&self, args: &[Value<'a>]) -> JVMResult<'a, Option<Value<'a>>> {
        interp::invoke(
            self.jvm,
            self.value,
            args.iter().map(|arg| arg.into_inner(self.jvm)),
        )
        .map(|value| value.map(|value| Value::new(self.jvm, value)))
        .map_err(|value| Object::new(self.jvm, value))
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Value<'a> {
    Ref(Option<Object<'a>>),
    /// Represents boolean, byte, char, short and int
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
}

impl<'a> Value<'a> {
    fn new(jvm: &'a jvm::Jvm, value: jvm::Value) -> Self {
        match value {
            jvm::Value::Ref(value) => Self::Ref(value.map(|value| Object::new(jvm, value))),
            jvm::Value::Int(value) => Self::Int(value),
            jvm::Value::Long(value) => Self::Long(value),
            jvm::Value::Float(value) => Self::Float(value),
            jvm::Value::Double(value) => Self::Double(value),
        }
    }

    fn into_inner(self, jvm: &jvm::Jvm) -> jvm::Value {
        match self {
            Self::Ref(value) => jvm::Value::Ref(value.map(|value| value.into_inner(jvm))),
            Self::Int(value) => jvm::Value::Int(value),
            Self::Long(value) => jvm::Value::Long(value),
            Self::Float(value) => jvm::Value::Float(value),
            Self::Double(value) => jvm::Value::Double(value),
        }
    }
}

impl<'a> Display for Value<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ref(None) => write!(f, "null"),
            Self::Ref(Some(obj)) => write!(f, "@{:x}", obj.value.ptr()),
            Self::Int(v) => write!(f, "{}", v),
            Self::Long(v) => write!(f, "{}", v),
            Self::Float(v) => write!(f, "{}", v),
            Self::Double(v) => write!(f, "{}", v),
        }
    }
}

macro_rules! into_and_try_from_value {
    ($variant:ident, $typ:ty, $err:literal) => {
        impl<'a> From<$typ> for Value<'a> {
            fn from(value: $typ) -> Self {
                Self::$variant(value)
            }
        }

        impl<'a> TryFrom<Value<'a>> for $typ {
            type Error = &'static str;

            fn try_from(value: Value<'a>) -> Result<Self, Self::Error> {
                if let Value::$variant(value) = value {
                    Ok(value)
                } else {
                    Err($err)
                }
            }
        }
    };
}

into_and_try_from_value!(Ref, Option<Object<'a>>, "Not a reference");
into_and_try_from_value!(Int, i32, "Not a boolean, byte, char, short or int");
into_and_try_from_value!(Long, i64, "Not a long");
into_and_try_from_value!(Float, f32, "Not a float");
into_and_try_from_value!(Double, f64, "Not a double");

/// Err is an object that extends Throwable
pub type JVMResult<'a, T> = std::result::Result<T, Object<'a>>;

bitflags::bitflags! {
    pub struct AccessFlags: u16 {
        const STATIC = 0x0008;
        const FINAL = 0x0010;
        const SUPER = 0x0020;
        const VOLATILE = 0x0040;
        const NATIVE = 0x0100;
        const ABSTRACT = 0x0400;
    }
}
