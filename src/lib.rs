#![feature(hash_set_entry)]
#![doc = include_str!("../README.md")]

mod class;
mod class_loader;
mod const_pool;
mod field;
mod field_storage;
mod heap;
mod instructions;
mod interpreter;
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

pub use class_loader::{ClassLoader, FolderClassLoader};
pub use field::FieldNaT;
pub use method::{MethodDescriptor, MethodNaT};
pub use typ::Typ;

fn default<T: Default>() -> T {
    Default::default()
}

// @next: Test abstract methods

// This file represents most of the public interface.
// It defines wrapper types that ensure types don't outlive the Jvm
// they belong to or get used with a different Jvm.

// Rough overview:
// - a `Jvm` contains classes and objects
// - internally, a lot of static references are used that only live as long as the `Jvm` exists,
//   which Rust can't express. These are never directly made available to the library consumer.
// - instead proxies which also contain a reference to the VM are provided
// - errors returned by fallible operations are Exception objects
// - classes are resolved lazily by `Jvm::resolve_class` and parsed in `parse.rs`
// - before a method is executed, it's class will be verified (once) in verification.rs
// - methods are executed using a simple interpreter loop in `interp.rs`
// - one `FieldStorage` per class/object compactly stores the static/instance fields
// - objects contain a pointer to their class, followed by their fields
// - objects are currently not garbage collected

// This uses copious amounts of unsafe, such as
// - to manage the lifecycle of classes, objects, …
// - when casting type-erased local/stack values to objects
// - when accessing fields (in `fields_storage.rs` and when dereferencing pointers to objects)
// Unfortunately, for most of this the safety boundary is at the crate level.

/// Creates & manages classes and objects, which borrow from the Jvm.
/// Multiple independent JVMs may exist.
/// Mutable shared access follows Java's rules.
///
/// To operate correctly, the JVM requires a class library. The minimal
/// version necessary to implement this is available under the `classes`
/// folder of this repository. These aren't loaded automatically, your
/// class library needs to be provided by the class loader. Missing or
/// invalid classes may cause the thread to hang or crash.
#[derive(Eq)]
pub struct Jvm(jvm::Jvm);

impl Jvm {
    pub fn new(base_class_loader: Box<dyn ClassLoader>) -> Self {
        Self(jvm::Jvm::new(base_class_loader))
    }

    /// Bypasses bytecode verification. This is currently necessary
    /// because veryfication is not yet implemented for all opcodes and
    /// will be removed afterwards.
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

    /// Returns the class, loading it if it wasn't already loaded.
    /// This also loads its superclass and any interfaces it implements.
    /// Does not initialize this class, nor load any other class referenced.
    pub fn resolve_class<'a>(&'a self, name: &str) -> JVMResult<'a, Class<'a>> {
        self.0
            .resolve_class(name)
            .map(|value| Class { jvm: self, value })
            .map_err(|value| Object::new(self, value))
    }

    /// Convenience method.
    /// Includes inherited dynamic methods.
    pub fn resolve_method<'a>(
        &'a self,
        class: &str,
        method: &str,
        args: impl IntoIterator<Item = Typ>,
        returns: Option<Typ>,
    ) -> JVMResult<'a, Method<'a>> {
        self.0
            .resolve_class(class)
            .map_err(|value| Object::new(self, value))?
            .methods
            .get(&MethodNaT {
                name: method.into(),
                typ: &MethodDescriptor {
                    args: args.into_iter().collect(),
                    returns,
                },
            })
            .ok_or_else(|| jvm::exception(&self.0, "NoSuchMethodError"))
            .map(|value| Method { jvm: self, value })
            .map_err(|value| Object::new(self, value))
    }

    /// Creates an array on the heap
    /// # Panics
    /// Panics if length < 0
    pub fn create_array<'a>(&'a self, typ: &Typ, length: i32) -> Object<'a> {
        Object::new(self, self.0.create_array_of(typ, length))
    }
}

impl PartialEq for Jvm {
    fn eq(&self, other: &Self) -> bool {
        self.0.id == other.0.id
    }
}

macro_rules! define_guard {
    ($(#[$doc:meta])* $guard_typ:ident, $impl_typ:ty) => {
        $(#[$doc])*
        pub struct $guard_typ<'a> {
            value: $impl_typ,
            jvm: &'a Jvm,
        }

        impl<'a> $guard_typ<'a> {
            fn new(jvm: &'a Jvm, value: $impl_typ) -> Self {
                Self { value, jvm }
            }

            #[allow(dead_code)]
            fn into_inner(self, jvm: &Jvm) -> $impl_typ {
                if self.jvm.0.id == jvm.0.id {
                    self.value
                } else {
                    panic!("Wrong JVM accessed")
                }
            }

            /// Returns a reference to the [`Jvm`] this belongs to.
            pub fn jvm(self) -> &'a Jvm {
                self.jvm
            }
        }

        impl<'a> Debug for $guard_typ<'a> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                Debug::fmt(&self.value, f)
            }
        }
        impl<'a> Hash for $guard_typ<'a> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.value.hash(state);
            }
        }
        impl<'a> PartialEq for $guard_typ<'a> {
            fn eq(&self, other: &Self) -> bool {
                self.value == other.value
            }
        }
        impl<'a> Eq for $guard_typ<'a> {
            fn assert_receiver_is_total_eq(&self) {}
        }
        impl<'a> Copy for $guard_typ<'a> {}
        impl<'a> Clone for $guard_typ<'a> {
            fn clone(&self) -> Self {
                *self
            }
        }
    };
}

define_guard!(
    /// A Java object, guaranteed to be non-null.
    /// An object may only be used with the jvm by which it was created.
    Object,
    object::Object
);

impl<'a> Object<'a> {
    /// The class this is an instance of.
    pub fn class(self) -> Class<'a> {
        Class::new(self.jvm, self.value.class())
    }

    /// If the object is an array return its length, None otherwise.
    pub fn array_len(self) -> Option<i32> {
        self.value.array_len()
    }

    /// Returns either the value at index.
    /// # Panics
    /// if the object is not an array or the index is out of bounds.
    pub fn array_read(self, index: i32) -> Value<'a> {
        Value::new(
            self.jvm,
            self.value.array_read(index),
            self.class().element_type().as_ref(),
        )
    }

    /// Sets an array element. If the element type is boolean, byte, short or char,
    /// the `JVMValue::Int` is truncated
    /// # Panics
    /// if object is not an array, the element type mismatches, the index is out of bounds
    /// or the value is an object created by another JVM.
    pub fn array_write(self, index: i32, value: Value<'a>) {
        self.value.array_write(index, value.into_inner(self.jvm))
    }
}

define_guard!(
    /// A class, interface (TODO), enum or array class.
    Class,
    &'static class::Class
);

impl<'a> Class<'a> {
    /// Returns the fully qualified internal name
    pub fn name(self) -> &'a str {
        &self.value.name
    }

    /// Returns the super class. Is `Some()` for all classes except `java.lang.Object`.
    pub fn super_class(self) -> Option<Self> {
        self.value
            .super_class
            .map(|value| Class::new(self.jvm, value))
    }

    /// Returns the array class which has this class as its element type.
    pub fn array_class(self) -> Self {
        Class::new(self.jvm, self.jvm.0.resolve_array_of(&self.typ()).unwrap())
    }

    /// Convenience method for getting this class' type
    pub(crate) fn typ(&self) -> Typ {
        self.value.typ()
    }

    /// Is `Some()` for classes representing an array and `None` otherwise.
    pub fn element_type(self) -> Option<Typ> {
        self.value.element_type.clone()
    }

    pub fn access_flags(self) -> AccessFlags {
        self.value.access_flags
    }

    /// Look up a field in this class by its name and type.
    /// Does not search the inheritance hierarchy.
    /// There may be multiple fields of the same name.
    pub fn field(self, name: &str, typ: Typ) -> Option<Field<'a>> {
        self.field_by_nat(FieldNaT {
            name: name.into(),
            typ,
        })
    }

    /// Look up a field in this class by its name and type.
    /// Does not search the inheritance hierarchy.
    /// There may be multiple fields of the same name.
    pub fn field_by_nat(self, nat: FieldNaT) -> Option<Field<'a>> {
        self.value
            .field_by_nat(nat)
            .map(|value| Field::new(self.jvm, value))
    }

    /// Look up a method by its name and type. There may be multiple methods of the same name.
    /// Includes inherited dynamic methods.
    pub fn method(
        self,
        name: &str,
        args: impl IntoIterator<Item = Typ>,
        ret: Option<Typ>,
    ) -> Option<Method<'a>> {
        self.method_by_nat(&MethodNaT {
            name: name.into(),
            typ: &MethodDescriptor {
                args: args.into_iter().collect(),
                returns: ret,
            },
        })
    }

    /// Look up a method by its name and type. There may be multiple methods of the same name.
    /// Includes inherited dynamic methods.
    pub fn method_by_nat(self, nat: &MethodNaT) -> Option<Method<'a>> {
        self.value
            .methods
            .get(nat)
            .map(|value| Method::new(self.jvm, value))
    }

    /// Ensures this class is initialized. Includes linking, verification, preparation & resolution.
    /// This happens automatically when a method belonging to this class is called.
    pub fn ensure_init(self) -> JVMResult<'a, ()> {
        self.value
            .ensure_init(&self.jvm.0)
            .map_err(|value| Object::new(self.jvm, value))
    }

    /// Creates an instance of a non-array class on the heap without invoking its constructor
    /// # Panics
    /// Panics if this is an array class.
    pub fn create_object_uninit(self) -> Object<'a> {
        Object::new(self.jvm, self.jvm.0.create_object(self.value))
    }

    // TODO:
    // /// Create a new instance of this class and call its constructor.
    // /// Returns None if no constructor matching the args is found.
    // /// Returns Some(Err) if the constructor threw an exception.
    // pub fn create_object(self, args: &[Value<'a>]) -> JVMResult<'a, Option<Value<'a>>
}

define_guard!(
    /// A static or instance field.
    Field,
    &'static field::Field
);

impl<'a> Field<'a> {
    /// The class this field is defined by.
    pub fn class(&self) -> Class {
        Class::new(self.jvm, self.value.class())
    }

    /// Access flags, such as whether this field is static.
    pub fn access_flags(&self) -> AccessFlags {
        self.value.access_flags
    }

    /// Name and type of this field. There may be multiple fields of the same name
    /// but differing types on the same class.
    pub fn name_and_type(&self) -> &FieldNaT {
        &self.value.nat
    }

    /// Get the value of a static field.
    /// Note that this might not be initialized yet unless a method has been called on this class.
    /// # Panics
    /// Panics if the field is not static.
    pub fn static_get(&self) -> Value<'a> {
        Value::new(
            self.jvm,
            self.value.static_get(),
            Some(&self.name_and_type().typ),
        )
    }

    /// Get the value of an object's field.
    /// # Panics
    /// Panics if the field is static or the object is not an instance of the class corresponding to this field.
    pub fn instance_get(&self, object: Object) -> Value<'a> {
        Value::new(
            self.jvm,
            self.value.instance_get(object.into_inner(self.jvm)),
            Some(&self.name_and_type().typ),
        )
    }

    /// Set the value of a static field.
    /// # Panics
    /// Panics if the field is not static or the value is not assignable to the field's type
    /// or the value is an object created by a different JVM
    pub fn static_set(&self, value: impl Into<Value<'a>>) {
        self.value.static_set(value.into().into_inner(self.jvm))
    }

    /// Get the value of an object's field.
    /// # Panics
    /// Panics if the field is static,
    /// the object is not an instance of the class corresponding to this field,
    /// the value is not assignable to the field's type
    /// or the value is an object created by a different JVM
    pub fn instance_set(&self, object: Object, value: impl Into<Value<'a>>) {
        self.value.instance_set(
            object.into_inner(self.jvm),
            value.into().into_inner(self.jvm),
        )
    }
}

define_guard!(
    /// A method. Does not necessarily have an implementation.
    Method,
    &'static method::Method
);

impl<'a> Method<'a> {
    /// The class this method is defined by.
    pub fn class(&self) -> Class {
        Class::new(self.jvm, self.value.class())
    }

    /// The name and type signature of the method. There may be multiple methods of the same
    /// name but differing types in the same class.
    pub fn name_and_type(&self) -> MethodNaT<'a> {
        self.value.nat()
    }

    pub fn access_flags(self) -> AccessFlags {
        self.value.access_flags
    }

    // TODO: publicly accessible dynamic method resolution
    /// Calls the method, initializing its class if necessary.
    /// Returns either the function's return value or the exception that was thrown
    /// # Panics
    /// Panics if any of the [`Value`]s is an object created by another JVM
    pub fn invoke(&self, args: &[Value<'a>]) -> JVMResult<'a, Value<'a>> {
        // TODO: Check for argument type mismatch
        interpreter::invoke(
            &self.jvm.0,
            self.value,
            args.iter().map(|arg| arg.into_inner(self.jvm)),
        )
        .map(|value| Value::new(self.jvm, value, self.name_and_type().typ.returns.as_ref()))
        .map_err(|value| Object::new(self.jvm, value))
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Value<'a> {
    /// Lack of a value, only used for return types.
    Void,
    Ref(Option<Object<'a>>),
    Boolean(bool),
    Byte(i8),
    Char(u16),
    Short(i16),
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
}

impl<'a> Value<'a> {
    fn new(jvm: &'a Jvm, value: jvm::Value, typ: Option<&Typ>) -> Self {
        match value {
            jvm::Value::Void => Self::Void,
            jvm::Value::Ref(value) => Self::Ref(value.map(|value| Object::new(jvm, value))),
            jvm::Value::Int(value) => match typ {
                Some(Typ::Boolean) => Self::Boolean(value != 0),
                Some(Typ::Byte) => Self::Byte(value as i8),
                Some(Typ::Short) => Self::Short(value as i16),
                Some(Typ::Char) => Self::Char(value as u16),
                Some(Typ::Int) => Self::Int(value),
                _ => unreachable!(),
            },
            jvm::Value::Long(value) => Self::Long(value),
            jvm::Value::Float(value) => Self::Float(value),
            jvm::Value::Double(value) => Self::Double(value),
        }
    }

    fn into_inner(self, jvm: &Jvm) -> jvm::Value {
        match self {
            Self::Void => jvm::Value::Void,
            Self::Ref(value) => jvm::Value::Ref(value.map(|value| value.into_inner(jvm))),
            Self::Boolean(value) => jvm::Value::Int(value as i32),
            Self::Byte(value) => jvm::Value::Int(value as i32),
            Self::Char(value) => jvm::Value::Int(value as i32),
            Self::Short(value) => jvm::Value::Int(value as i32),
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
            Self::Void => write!(f, "void"),
            Self::Ref(None) => write!(f, "null"),
            Self::Ref(Some(obj)) => write!(f, "@{:x}", obj.value.ptr()),
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

        // Can't do
        // `impl<'a, T: Into<Value<'a>>> PartialEq<T> for Value<'a>`
        // because that would conflict for T = Value
        impl<'a> PartialEq<$typ> for Value<'a> {
            fn eq(&self, other: &$typ) -> bool {
                self == &Into::<Value<'a>>::into(*other)
            }
        }

        impl<'a> PartialEq<Value<'a>> for $typ {
            fn eq(&self, other: &Value) -> bool {
                other == self
            }
        }
    };
}

partialeq_into_and_try_from_value!(Ref, Option<Object<'a>>, "Not a reference");
partialeq_into_and_try_from_value!(Boolean, bool, "Not a boolean");
partialeq_into_and_try_from_value!(Byte, i8, "Not a byte");
partialeq_into_and_try_from_value!(Char, u16, "Not a char");
partialeq_into_and_try_from_value!(Short, i16, "Not a short");
partialeq_into_and_try_from_value!(Int, i32, "Not an int");
partialeq_into_and_try_from_value!(Long, i64, "Not a long");
partialeq_into_and_try_from_value!(Float, f32, "Not a float");
partialeq_into_and_try_from_value!(Double, f64, "Not a double");

impl<'a> From<()> for Value<'a> {
    fn from(_: ()) -> Self {
        Self::Void
    }
}

impl<'a> TryFrom<Value<'a>> for () {
    type Error = &'static str;

    fn try_from(value: Value<'a>) -> Result<Self, Self::Error> {
        if value == Value::Void {
            Ok(())
        } else {
            Err("Hold a value")
        }
    }
}

impl<'a> PartialEq<()> for Value<'a> {
    fn eq(&self, _: &()) -> bool {
        self == &Self::Void
    }
}

impl<'a> PartialEq<Value<'a>> for () {
    fn eq(&self, other: &Value) -> bool {
        other == self
    }
}

/// Err is an object that extends Throwable.
/// Currently no call stack is provided.
pub type JVMResult<'a, T> = std::result::Result<T, Object<'a>>;

bitflags::bitflags! {
    /// Access flags of a class, field or method
    pub struct AccessFlags: u16 {
        /// May be accessed from outside its package.
        const PUBLIC = 0x0001;
        /// May be accessed only from inside its defining class. (TODO)
        const PRIVATE = 0x0002;
        /// May be accessed only from inside its defining class or subclasses. (TODO)
        const PROTECTED = 0x0004;
        /// Declared static
        const STATIC = 0x0008;
        /// Class: May not be subclassed.
        ///
        /// Field: May not be assigned to after initialization.
        ///
        /// Method: May not be overwritten by subclas.
        const FINAL = 0x0010;
        /// Class: Treat superclass methods specially when invoked by the invokespecial instruction.
        const SUPER = 0x0020;
        /// Invocation guarded by monitor (TODO).
        const SYNCHRONIZED = 0x0020;
        /// Reads and writes to this field are consistent across threads.
        const VOLATILE = 0x0040;
        /// Field: Not written or read by a persistent object manager.
        const TRANSIENT = 0x0080;
        /// This method is implemented in native code (TODO).
        const NATIVE = 0x0100;
        /// This class is an interface.
        const INTERACE = 0x0200;
        /// Class: Must not be instantiated.
        ///
        /// Method: Not implemented in this class; may be implemented by subclasses.
        /// If the concrete class of the instance it is called on does not
        const ABSTRACT = 0x0400;
        /// Method is FP-strict. This JVM always uses FP-strict mode.
        const STRICT = 0x0800;
        /// Not extant in source code.
        const SYNTHETIC = 0x1000;
        /// This class is an annotation.
        const ANNOTATION = 0x2000;
        /// Class: Is an enum.
        ///
        /// Field: Member of an enum.
        const ENUM = 0x4000;
    }
}
