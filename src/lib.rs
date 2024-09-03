#![doc = include_str!("../README.md")]

mod class;
mod class_loader;
mod const_pool;
mod field;
mod field_storage;
mod heap;
mod hot_reloading;
mod interpreter;
mod jvm;
mod method;
mod object;
mod opcodes;
mod parse;
mod typ;
mod value;
mod verification;

use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

pub use class_loader::{ClassLoader, FolderClassLoader};
pub use jvm::Jvm;
pub use method::MethodDescriptor;
pub use method::NativeCode;
use method::{Code, MethodNaTBorrow};
pub use typ::Typ;

fn default<T: Default>() -> T {
    Default::default()
}

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

/// Guard types ensure types don't outlive the Jvm
/// they belong to or get used with a different Jvm.
macro_rules! define_guard {
    ($(#[$doc:meta])* $guard_typ:ident, $impl_typ:ty) => {
        $(#[$doc])*
        #[derive(Copy, Clone, Eq)]
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

    /// Get the value of an object's field.
    ///
    /// # Panics
    /// Panics if the field does not exist or is static.
    pub fn get(self, field_name: &str, field_type: Typ) -> Value<'a> {
        let Some(field) = self.class().field(field_name, field_type.clone()) else {
            panic!(
                "Field {field_name} of type {field_type} not found in object of type {}",
                self.class().value
            )
        };
        field.instance_get(self)
    }

    /// Set the value of an object's field.
    ///
    /// # Panics
    /// Panics if the field does not exist or is static or
    /// the value is not assignable to the field's type
    /// or is an object created by a different JVM
    pub fn set(self, field_name: &str, field_type: Typ, value: impl Into<Value<'a>>) {
        let Some(field) = self.class().field(field_name, field_type.clone()) else {
            panic!(
                "Field {field_name} of type {field_type} not found in object of type {}",
                self.class().value
            )
        };
        field.instance_set(self, value.into())
    }

    /// Returns whether this is an array.
    pub fn is_array(self) -> bool {
        self.value.is_array()
    }

    /// Returns the number of items in this array.
    ///
    /// # Panics
    /// Panics if the object is not an array.
    pub fn array_len(self) -> i32 {
        self.value.array_len().expect("not an array")
    }

    /// Get an array element.
    ///
    /// # Panics
    /// Panics if the object is not an array or the `index` is out of bounds.
    pub fn array_read(self, index: i32) -> Value<'a> {
        Value::new(
            self.jvm,
            self.value.array_read(index),
            self.class().element_type().as_ref(),
        )
    }

    /// Sets an array element. If the element type is boolean, byte, short or char,
    /// the `JvmValue::Int` is truncated
    ///
    /// # Panics
    /// Panics if object is not an array, the element type mismatches, `index` is out of bounds
    /// or the value is an object created by another JVM.
    pub fn array_write(self, index: i32, value: Value<'a>) {
        self.value.array_write(index, value.into_inner(self.jvm))
    }

    /// Creates a debug representation of the object. For a full object tree, use `Debug`.
    pub fn format_short(self) -> String {
        self.value.format_short()
    }
}

impl<'a> std::fmt::Pointer for Object<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Pointer::fmt(&self.value, f)
    }
}

define_guard!(
    /// A class, interface, enum or array class.
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
        Class::new(self.jvm, self.jvm.resolve_array_of(&self.typ()).unwrap())
    }

    /// Convenience method for getting this class' type
    pub fn typ(&self) -> Typ {
        self.value.typ()
    }

    pub fn is_interface(&self) -> bool {
        self.value.is_interface()
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
        self.value
            .field(name, typ)
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
        self.method_by_descriptor(
            name,
            &MethodDescriptor {
                args: args.into_iter().collect(),
                returns: ret,
            },
        )
    }

    /// Look up a method by its name and type. There may be multiple methods of the same name.
    /// Includes inherited dynamic methods.
    pub fn method_by_descriptor(self, name: &str, typ: &MethodDescriptor) -> Option<Method<'a>> {
        self.value
            .methods
            .get(&MethodNaTBorrow { name, typ })
            .map(|value| Method::new(self.jvm, value))
    }

    /// Ensures this class is initialized. Includes linking, verification, preparation & resolution.
    /// This happens automatically when a method belonging to this class is called.
    pub fn ensure_init(self) -> JvmResult<'a, ()> {
        self.value
            .ensure_init(self.jvm)
            .map_err(|value| Object::new(self.jvm, value))
    }

    /// Creates an instance of a non-array class on the heap without invoking its constructor
    ///
    /// # Panics
    /// Panics if this is an array class.
    pub fn create_object_uninit(self) -> Object<'a> {
        Object::new(self.jvm, self.jvm.create_object_uninitialized(self.value))
    }
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

    /// Name of this field. There may be multiple fields of the same name
    /// but differing types on the same class.
    pub fn name(&self) -> &str {
        &self.value.nat.name
    }

    /// Type of this field.
    pub fn typ(&self) -> &Typ {
        &self.value.nat.typ
    }

    /// Get the value of a static field.
    /// Note that this might not be initialized yet unless a method has been called on this class.
    ///
    /// # Panics
    /// Panics if the field is not static.
    pub fn static_get(&self) -> Value<'a> {
        Value::new(self.jvm, self.value.static_get(), Some(self.typ()))
    }

    /// Get the value of an object's field.
    ///
    /// # Panics
    /// Panics if the field is static or the object is not an instance of the class corresponding to this field.
    pub fn instance_get(&self, object: Object) -> Value<'a> {
        Value::new(
            self.jvm,
            self.value.instance_get(object.into_inner(self.jvm)),
            Some(self.typ()),
        )
    }

    /// Set the value of a static field.
    ///
    /// # Panics
    /// Panics if the field is not static or the value is not assignable to the field's type
    /// or is an object created by a different JVM
    pub fn static_set(&self, value: impl Into<Value<'a>>) {
        self.value.static_set(value.into().into_inner(self.jvm))
    }

    /// Set the value of an object's field.
    ///
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

    /// The name of this method. There may be multiple methods of the same
    /// name but differing types in the same class.
    pub fn name(&self) -> &str {
        &self.value.nat.name
    }

    /// Type signature of this method.
    pub fn typ(&self) -> &MethodDescriptor {
        &self.value.nat.typ
    }

    pub fn access_flags(self) -> AccessFlags {
        self.value.access_flags
    }

    /// Calls the method, initializing its class if necessary.
    /// Returns either the function's return value or the exception that was thrown
    ///
    /// # Panics
    /// Panics if any of the [`Value`]s is an object created by another JVM
    pub fn invoke(&self, args: &[Value<'a>]) -> JvmResult<'a, Value<'a>> {
        // TODO: Check for argument type mismatch
        interpreter::invoke(
            self.jvm,
            self.value,
            args.iter().map(|arg| arg.into_inner(self.jvm)),
        )
        .map(|value| Value::new(self.jvm, value, self.typ().returns.as_ref()))
        .map_err(|value| Object::new(self.jvm, value))
    }

    /// Register code for a native method. If called multiple times for the same method, the previous registration will be replaced and returned.
    ///
    /// The method needs to be marked as native.
    /// If it is an instance method, the first argument passed to `code` will be the object it is invoked on.
    pub fn register_native_code(
        &self,
        code: NativeCode,
    ) -> Result<Option<NativeCode>, MethodNotNative> {
        if self.value.access_flags.contains(AccessFlags::NATIVE) {
            Ok(
                if let Code::Native(prev) =
                    std::mem::replace(&mut *self.value.code.write(), Code::Native(code))
                {
                    Some(prev)
                } else {
                    None
                },
            )
        } else {
            Err(MethodNotNative)
        }
    }
}

#[derive(Debug)]
pub struct MethodNotNative;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Value<'a> {
    /// Lack of a value, only used for return types.
    Void,
    // Split arrays and normal objects?
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
    fn new(jvm: &'a Jvm, value: value::Value, typ: Option<&Typ>) -> Self {
        match value {
            value::Value::Void => Self::Void,
            value::Value::Ref(value) => Self::Ref(value.map(|value| Object::new(jvm, value))),
            value::Value::Int(value) => match typ {
                Some(Typ::Boolean) => Self::Boolean(value != 0),
                Some(Typ::Byte) => Self::Byte(value as i8),
                Some(Typ::Short) => Self::Short(value as i16),
                Some(Typ::Char) => Self::Char(value as u16),
                Some(Typ::Int) => Self::Int(value),
                _ => unreachable!(),
            },
            value::Value::Long(value) => Self::Long(value),
            value::Value::Float(value) => Self::Float(value),
            value::Value::Double(value) => Self::Double(value),
        }
    }

    fn into_inner(self, jvm: &Jvm) -> value::Value {
        match self {
            Self::Void => value::Value::Void,
            Self::Ref(value) => value::Value::Ref(value.map(|value| value.into_inner(jvm))),
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
pub type JvmResult<'a, T> = std::result::Result<T, Object<'a>>;

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
        // SUPER is removed since Java 8
        /// Invocation guarded by monitor (TODO).
        const SYNCHRONIZED = 0x0020;
        /// Field: Reads and writes are consistent across threads.
        const VOLATILE = 0x0040;
        /// Field: Not written or read by a persistent object manager.
        const TRANSIENT = 0x0080;
        /// Method: Implemented in native code.
        const NATIVE = 0x0100;
        /// Class: Is an interface.
        const INTERFACE = 0x0200;
        /// Class: Must not be instantiated.
        ///
        /// Method: Not implemented in this class; may be implemented by subclasses.
        const ABSTRACT = 0x0400;
        /// Method: FP-strict. This JVM always uses FP-strict mode.
        const STRICT = 0x0800;
        /// Not extant in source code.
        const SYNTHETIC = 0x1000;
        /// Class: Is an annotation.
        const ANNOTATION = 0x2000;
        /// Class: Is an enum.
        ///
        /// Field: Member of an enum.
        const ENUM = 0x4000;
    }
}

#[inline]
fn unreachable_verified() -> ! {
    #[cfg(debug_assertions)]
    unreachable!();
    #[cfg(not(debug_assertions))]
    unsafe {
        std::hint::unreachable_unchecked();
    }
}
