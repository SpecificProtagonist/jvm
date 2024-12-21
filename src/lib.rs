#![feature(ptr_as_ref_unchecked)]
#![feature(sync_unsafe_cell)]
#![doc = include_str!("../README.md")]

mod class;
mod class_loader;
mod const_pool;
mod fake_variadics;
mod field;
mod heap;
mod hot_reloading;
mod identity_hash;
mod interpreter;
mod jvm;
mod method;
mod miri;
mod monitor;
#[path = "monitor_2/monitor.rs"]
mod monitor_2;
mod object;
mod opcodes;
mod parse;
pub mod provided_native_methods;
mod typ;
mod value;
mod verification;

use std::{
    fmt::{Debug, Display},
    hash::Hash,
    marker::PhantomData,
};

pub use class_loader::{ClassLoader, FolderClassLoader};
use fake_variadics::{NativeBindError, NativeFn};
use heap::Ptrs;
pub use jvm::Jvm;
use jvm::{error, JvmId};
pub use method::MethodDescriptor;
use method::MethodNaTBorrow;
pub use method::NativeCode;
pub use typ::Type;

fn default<T: Default>() -> T {
    Default::default()
}

/// Guard types ensure values can't be used without the vm they belong to / be used with the wrong vm.
// TODO: Either split guard object into id and lifetimed object, or use Arc/Weak everywhere.
// If using lifetimes, also add a module that reexports everything with a static lifetime.
macro_rules! define_guard {
    ($(#[$doc:meta])* $guard_typ:ident, $impl_typ:ty) => {
        $(#[$doc])*
        #[derive(Copy, Clone, Eq)]
        pub struct $guard_typ {
            value: usize,
            jvm: JvmId,
        }

        impl $guard_typ {
            fn new(jvm: JvmId, value: &'static $impl_typ) -> Self {
                Self { value: value as *const $impl_typ as usize, jvm }
            }

            #[allow(dead_code)]
            fn into_inner(self, jvm: JvmId) -> &'static $impl_typ {
                if self.jvm == jvm {
                    unsafe {(self.value as *const $impl_typ).as_ref_unchecked()}
                } else {
                    panic!("Wrong JVM accessed")
                }
            }

            #[allow(dead_code)]
            fn as_ref(self) -> &'static $impl_typ {
                unsafe {(self.value as *const $impl_typ).as_ref_unchecked()}
            }
        }

        impl Debug for $guard_typ {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{:p}", self.value as *const $impl_typ)
            }
        }

        impl Hash for $guard_typ {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                // This is safe because pointers are hashed instead of dereferenced
                self.value.hash(state);
            }
        }

        impl PartialEq for $guard_typ {
            fn eq(&self, other: &Self) -> bool {
                // This is safe because pointers are compared instead of dereferenced
                self.value == other.value
            }
        }
    };
}

/// A Java object, guaranteed to be non-null.
/// An object may only be used with the jvm by which it was created.
#[derive(Copy, Clone, Eq)]
pub struct Object {
    value: object::Object,
    jvm: JvmId,
}

impl Hash for Object {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // This is safe because pointers are hashed instead of dereferenced
        self.value.hash(state);
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        // This is safe because pointers are compared instead of dereferenced
        self.value == other.value
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:x}", self.value.0)
    }
}

impl Object {
    fn new(jvm: JvmId, value: object::Object) -> Self {
        Self { value, jvm }
    }

    #[allow(dead_code)]
    fn into_inner(self, jvm: JvmId) -> object::Object {
        if self.jvm == jvm {
            self.value
        } else {
            panic!("Wrong JVM accessed")
        }
    }

    /// The class this is an instance of.
    pub fn class(self, jvm: &Jvm) -> Class {
        Class::new(self.jvm, self.into_inner(jvm.id).class(jvm.heap.ptrs()))
    }

    /// Get the value of an object's field.
    ///
    /// # Panics
    /// Panics if the field does not exist or is static.
    pub fn get(self, jvm: &Jvm, field_name: &str, field_type: Type) -> Value {
        let Some(field) = self.class(jvm).field(jvm, field_name, field_type.clone()) else {
            panic!(
                "Field {field_name} of type {field_type} not found in object of type {}",
                self.class(jvm).into_inner(self.jvm)
            )
        };
        Value::new(
            self.jvm,
            field
                .into_inner(self.jvm)
                .instance_get(jvm.heap.ptrs(), self.value),
            &field.into_inner(self.jvm).nat.typ,
        )
    }

    /// Set the value of an object's field.
    ///
    /// # Panics
    /// Panics if the field does not exist or is static or
    /// the value is not assignable to the field's type
    /// or is an object created by a different JVM
    pub fn set(self, jvm: &Jvm, field_name: &str, field_type: Type, value: impl Into<Value>) {
        let Some(field) = self.class(jvm).field(jvm, field_name, field_type.clone()) else {
            panic!(
                "Field {field_name} of type {field_type} not found in object of type {}",
                self.class(jvm).into_inner(self.jvm)
            )
        };
        field.into_inner(self.jvm).instance_set(
            jvm.heap.ptrs(),
            self.value,
            value.into().into_inner(self.jvm),
        )
    }

    /// Returns whether this is an array.
    pub fn is_array(self, jvm: &Jvm) -> bool {
        self.into_inner(jvm.id).is_array(jvm.heap.ptrs())
    }

    /// Returns the number of items in this array.
    ///
    /// # Panics
    /// Panics if the object is not an array.
    #[track_caller]
    pub fn array_length(self, jvm: &Jvm) -> i32 {
        let obj = self.into_inner(jvm.id);
        if obj.class(jvm.heap.ptrs()).element_type.is_none() {
            panic!("not an array");
        }
        obj.array_length()
    }

    /// Get an array element.
    ///
    /// # Panics
    /// Panics if the object is not an array or the `index` is out of bounds.
    #[track_caller]
    pub fn array_read(self, jvm: &Jvm, index: i32) -> Value {
        Value::new(
            self.jvm,
            self.value.array_read_value(jvm.heap.ptrs(), index),
            self.class(jvm).element_type(jvm).as_ref().unwrap(),
        )
    }

    /// Sets an array element. If the element type is boolean, byte, short or char,
    /// the `JvmValue::Int` is truncated
    ///
    /// # Panics
    /// Panics if object is not an array, the element type mismatches, `index` is out of bounds
    /// or the value is an object created by another JVM.
    #[track_caller]
    pub fn array_write(self, jvm: &Jvm, index: i32, value: Value) {
        self.into_inner(jvm.id)
            .array_write_value(jvm.heap.ptrs(), index, value.into_inner(jvm.id))
    }

    /// Enters this object's monitor, blocking if it is already locked by another thread.
    ///
    /// This is reëntrant: A thread may enter the monitor multiple times, and must [`exit`](Object::exit_monitor)
    /// is as many times to unlock the object.
    pub fn enter_monitor(self, jvm: &Jvm) {
        jvm.parking_lot
            .monitor_enter(self.into_inner(jvm.id).mark_word());
    }

    /// Exists this object's monitor.
    /// After 2³¹-1 more calls to `enter_monitor` on the same thread than to `exit_monitor`, this will fail to release the lock.
    ///
    /// Throws `IllegalMonitorStateException` is the object is not locked by this thread.
    pub fn exit_monitor(self, jvm: &Jvm) -> JvmResult<()> {
        if jvm
            .parking_lot
            .monitor_exit(self.into_inner(jvm.id).mark_word())
            .is_err()
        {
            error(jvm, "IllegalMonitorStateException", "");
        }
        Ok(())
    }

    // /// Wait on this object until notified by [`notify`](Object::notify) or [`notify_all`](Object::notify_all),
    // /// or until spuriously woken up.
    // ///
    // /// This thread must hold the object's lock beforehand (otherwise `IllegalMonitorStateException` will be thrown).
    // /// and will reäquire the lock when woken.
    // pub fn wait(self, jvm: &Jvm) -> JvmResult<()> {
    //     self.into_inner(jvm.id)
    //         .monitor()
    //         .wait()
    //         .map_err(|_| Object::new(jvm.id, error(jvm, "IllegalMonitorStateException", "")))
    // }

    // /// Notifies a thread [`wait`ing](Object::wait) on this object.
    // ///
    // /// This thread must hold the object's lock (otherwise `IllegalMonitorStateException` will be thrown).
    // pub fn notify(self, jvm: &Jvm) -> JvmResult<()> {
    //     self.into_inner(jvm.id)
    //         .monitor()
    //         .notify_one()
    //         .map_err(|_| Object::new(jvm.id, error(jvm, "IllegalMonitorStateException", "")))
    // }

    // /// Notifies all threads [`wait`ing](Object::wait) on this object.
    // ///
    // /// This thread must hold the object's lock (otherwise `IllegalMonitorStateException` will be thrown).
    // pub fn notify_all(self, jvm: &Jvm) -> JvmResult<()> {
    //     self.into_inner(jvm.id)
    //         .monitor()
    //         .notify_all()
    //         .map_err(|_| Object::new(jvm.id, error(jvm, "IllegalMonitorStateException", "")))
    // }

    /// Creates a debug representation of the object. For a full object tree, use `Debug`.
    pub fn format_short(self, jvm: &Jvm) -> String {
        self.into_inner(jvm.id).format_short(jvm.heap.ptrs())
    }

    /// Creates a debug representation of the object. If the object is a string, this prints the string.
    pub fn format(self, jvm: &Jvm) -> impl Debug {
        ObjectFmt(self.into_inner(jvm.id), jvm.heap.ptrs(), PhantomData)
    }

    /// Creates a debug representation of the object. If the object is a string, this prints the string.
    pub fn to_string(self, jvm: &Jvm) -> String {
        format!("{:?}", self.format(jvm))
    }

    pub fn identity_hash(self, jvm: &Jvm) -> i32 {
        jvm.identity_hashes
            .get_identity_hash(self.into_inner(jvm.id))
    }
}

impl std::fmt::Pointer for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Pointer::fmt(&self.value, f)
    }
}

struct ObjectFmt<'a>(object::Object, Ptrs, PhantomData<&'a Jvm>);

impl Debug for ObjectFmt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        object::fmt_object(self.1, self.0, f, true)
    }
}

define_guard!(
    /// A class, interface, enum or array class.
    Class,
    class::Class
);

impl Class {
    /// Fully qualified name.
    pub fn name(self, jvm: &Jvm) -> &str {
        &self.into_inner(jvm.id).name
    }

    /// Returns the super class. Is `Some()` for all classes except `java.lang.Object`.
    pub fn super_class(self, jvm: &Jvm) -> Option<Self> {
        self.into_inner(jvm.id)
            .super_class
            .map(|value| Class::new(self.jvm, value))
    }

    /// Returns the array class which has this class as its element type.
    pub fn array_class(self, jvm: &Jvm) -> Self {
        Class::new(jvm.id, jvm.resolve_array_of(&self.typ(jvm)).unwrap())
    }

    /// Convenience method for getting this class' type.
    pub fn typ(&self, jvm: &Jvm) -> Type {
        self.into_inner(jvm.id).typ()
    }

    pub fn is_interface(&self, jvm: &Jvm) -> bool {
        self.into_inner(jvm.id).is_interface()
    }

    /// Is `Some()` for classes representing an array and `None` otherwise.
    pub fn element_type(self, jvm: &Jvm) -> Option<Type> {
        self.into_inner(jvm.id).element_type.clone()
    }

    pub fn access_flags(self, jvm: &Jvm) -> AccessFlags {
        self.into_inner(jvm.id).access_flags
    }

    /// Name of the source file this class was compiled from. Not an absolute path.
    pub fn source_file_name(self, jvm: &Jvm) -> Option<&str> {
        self.into_inner(jvm.id).source_file_name.as_deref()
    }

    /// Look up a field in this class by its name and type.
    /// Does not search the inheritance hierarchy.
    /// There may be multiple fields of the same name.
    pub fn field(self, jvm: &Jvm, name: &str, typ: Type) -> Option<Field> {
        self.into_inner(jvm.id)
            .field(name, typ)
            .map(|value| Field::new(self.jvm, value))
    }

    /// Look up a method by its name and type.
    /// There may be multiple methods of the same name, as long as they differ in type.
    /// Includes inherited dynamic methods.
    pub fn method(
        self,
        jvm: &Jvm,
        name: &str,
        args: impl IntoIterator<Item = Type>,
        ret: Option<Type>,
    ) -> Option<Method> {
        self.method_by_descriptor(
            jvm,
            name,
            &MethodDescriptor {
                args: args.into_iter().collect(),
                returns: ret,
            },
        )
    }

    /// Look up a method by its name and type.
    /// There may be multiple methods of the same name, as long as they differ in type.
    /// Includes inherited dynamic methods.
    pub fn method_by_descriptor(
        self,
        jvm: &Jvm,
        name: &str,
        typ: &MethodDescriptor,
    ) -> Option<Method> {
        self.into_inner(jvm.id)
            .methods
            .get(&MethodNaTBorrow { name, typ })
            .map(|value| Method::new(self.jvm, value))
    }

    /// Check whether this class is allowed to acces `other`
    pub fn can_access_class(&self, jvm: &Jvm, other: Class) -> bool {
        self.into_inner(jvm.id)
            .can_access_class(other.into_inner(jvm.id))
    }

    /// Ensures this class is initialized. Includes linking, verification, preparation & resolution.
    /// This happens automatically when a method belonging to this class is called.
    pub fn ensure_init(self, jvm: &Jvm) -> JvmResult<()> {
        self.into_inner(jvm.id)
            .ensure_init(jvm)
            .map_err(|value| Object::new(self.jvm, value))
    }

    /// Creates an instance of a non-array class on the heap without invoking its constructor.
    ///
    /// # Panics
    /// Panics if this is an array class or interface.
    #[track_caller]
    pub fn create_object_uninitialized(self, jvm: &Jvm) -> Object {
        Object::new(
            self.jvm,
            jvm.create_object_uninitialized(self.into_inner(jvm.id)),
        )
    }

    /// Creates an object via constructor.
    ///
    /// # Panics
    /// Panics if this is an array class or interface.
    #[track_caller]
    pub fn create_object(
        &self,
        jvm: &Jvm,
        arg_types: impl IntoIterator<Item = Type>,
        mut args: Vec<Value>,
    ) -> JvmResult<Object> {
        let object = Object::new(
            jvm.id,
            jvm.create_object_uninitialized(self.into_inner(jvm.id)),
        );
        args.insert(0, Value::Ref(Some(object)));
        let constructor = self.method(jvm, "<init>", arg_types, None).ok_or_else(|| {
            crate::Object::new(
                jvm.id,
                error(
                    jvm,
                    "NoSuchMethodError",
                    &format!("{} <init> …", self.into_inner(jvm.id)),
                ),
            )
        })?;
        constructor.invoke(jvm, &args)?;
        Ok(object)
    }
}

define_guard!(
    /// A static or instance field.
    Field,
    field::Field
);

impl Field {
    /// The class this field is defined by.
    pub fn class(self, jvm: &Jvm) -> Class {
        Class::new(self.jvm, self.into_inner(jvm.id).class())
    }

    /// Access flags, such as whether this field is static.
    pub fn access_flags(&self, jvm: &Jvm) -> AccessFlags {
        self.into_inner(jvm.id).access_flags
    }

    /// Name of this field. There may be multiple fields of the same name
    /// but differing types on the same class.
    pub fn name(&self, jvm: &Jvm) -> &str {
        &self.into_inner(jvm.id).nat.name
    }

    /// Type of this field.
    pub fn typ(&self, jvm: &Jvm) -> &Type {
        &self.into_inner(jvm.id).nat.typ
    }

    /// Get the value of a static field.
    /// Note that this might not be initialized yet unless a method has been called on this class.
    ///
    /// # Panics
    /// Panics if the field is not static.
    #[track_caller]
    pub fn static_get(&self, jvm: &Jvm) -> Value {
        Value::new(
            self.jvm,
            self.into_inner(jvm.id).static_get(jvm.heap.ptrs()),
            self.typ(jvm),
        )
    }

    /// Get the value of an object's field.
    ///
    /// # Panics
    /// Panics if the field is static or the object is not an instance of the class corresponding to this field.
    #[track_caller]
    pub fn instance_get(&self, jvm: &Jvm, object: Object) -> Value {
        Value::new(
            self.jvm,
            self.into_inner(jvm.id)
                .instance_get(jvm.heap.ptrs(), object.into_inner(self.jvm)),
            self.typ(jvm),
        )
    }

    /// Set the value of a static field.
    ///
    /// # Panics
    /// Panics if the field is not static or the value is not assignable to the field's type
    /// or is an object created by a different JVM
    #[track_caller]
    pub fn static_set(&self, jvm: &Jvm, value: impl Into<Value>) {
        self.into_inner(jvm.id)
            .static_set(jvm.heap.ptrs(), value.into().into_inner(self.jvm))
    }

    /// Set the value of an object's field.
    ///
    /// # Panics
    /// Panics if the field is static,
    /// the object is not an instance of the class corresponding to this field,
    /// the value is not assignable to the field's type
    /// or the value is an object created by a different JVM
    #[track_caller]
    pub fn instance_set(&self, jvm: &Jvm, object: Object, value: impl Into<Value>) {
        self.into_inner(jvm.id).instance_set(
            jvm.heap.ptrs(),
            object.into_inner(self.jvm),
            value.into().into_inner(self.jvm),
        )
    }
}

define_guard!(
    /// A method. Does not necessarily have an implementation.
    Method,
    method::Method
);

impl Method {
    /// The class this method is defined by.
    pub fn class(&self, jvm: &Jvm) -> Class {
        Class::new(self.jvm, self.into_inner(jvm.id).class())
    }

    /// The name of this method. There may be multiple methods of the same
    /// name but differing types in the same class.
    pub fn name(&self, jvm: &Jvm) -> &str {
        &self.into_inner(jvm.id).nat.name
    }

    /// Argument and return types of this method.
    pub fn typ(&self, jvm: &Jvm) -> &MethodDescriptor {
        &self.into_inner(jvm.id).nat.typ
    }

    /// Access flags of this method.
    pub fn access_flags(self, jvm: &Jvm) -> AccessFlags {
        self.into_inner(jvm.id).access_flags
    }

    /// Calls the method, initializing its class if necessary.
    /// Returns either the function's return value or the exception that was thrown.
    ///
    /// # Panics
    /// Panics if any of the [`Value`]s is an object created by another JVM.
    pub fn invoke(&self, jvm: &Jvm, args: &[Value]) -> JvmResult<Option<Value>> {
        interpreter::invoke(
            jvm,
            self.into_inner(jvm.id),
            args.iter().map(|arg| arg.into_inner(self.jvm)),
        )
        .map(|returned| {
            returned
                .map(|value| Value::new(self.jvm, value, self.typ(jvm).returns.as_ref().unwrap()))
        })
        .map_err(|e| Object::new(self.jvm, e))
    }

    /// Register code for a native method. If called multiple times for the same method, the previous registration will be replaced.
    ///
    /// `code` must be a function of the following form:
    /// - optionally, a first argument of type &Jvm
    /// - the remaining arguments consist of either `&[Value]` or up to 16 arguments of a translateable type
    /// - the return type must be `JvmResult<T>` where `T` is a translatable type
    ///
    /// Translatable types:  
    /// `bool`, `i8`, `i16`, `u16`, `i32`, `i64`, `f32`, `f64`, `()` (only for return type), `Option<Object>`, `Object` (only for `this`)
    ///
    /// The method must be marked as native and match the signature of `code`.
    /// If it is an instance method, its first argument will be the object it is invoked on.
    ///
    /// The callback returning a value of wrong type will result in a panic.
    // TODO: Note guarantees about available stack size
    #[allow(private_bounds)]
    pub fn bind_native_code<F: NativeFn<T>, T>(
        &self,
        jvm: &Jvm,
        code: F,
    ) -> Result<(), NativeBindError> {
        fake_variadics::bind_native_code(self.into_inner(jvm.id), code)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
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
    fn new(jvm: JvmId, value: value::Value, typ: &Type) -> Self {
        match value {
            value::Value::Ref(value) => Self::Ref(value.map(|value| Object::new(jvm, value))),
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

    fn into_inner(self, jvm: JvmId) -> value::Value {
        match self {
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

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
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
                self == &Into::<Value>::into(*other)
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
        self == &Into::<Value>::into(*other)
    }
}

impl PartialEq<Value> for Object {
    fn eq(&self, other: &Value) -> bool {
        *other == Value::Ref(Some(*self))
    }
}

/// Err is an object that extends Throwable.
/// Currently no call stack is provided.
pub type JvmResult<T> = std::result::Result<T, Object>;

bitflags::bitflags! {
    /// Access flags of a class, field or method
    pub struct AccessFlags: u16 {
        /// May be accessed from outside its package.
        const PUBLIC = 0x0001;
        /// May be accessed only from inside its defining class. (TODO)
        const PRIVATE = 0x0002;
        /// May be accessed only from inside its defining class or subclasses. (TODO)
        const PROTECTED = 0x0004;
        /// Declared static.
        const STATIC = 0x0008;
        /// Class: May not be subclassed.
        ///
        /// Field: May not be assigned to after initialization.
        ///
        /// Method: May not be overwritten by subclas.
        const FINAL = 0x0010;
        // SUPER is removed since Java 8
        /// Invocation guarded by monitor.
        const SYNCHRONIZED = 0x0020;
        /// Field: Reads and writes are consistent across threads.
        const VOLATILE = 0x0040;
        /// Method: Bridge methods generated by compiler.
        const BRIDGE = 0x0040;
        /// Field: Not written or read by a persistent object manager.
        const TRANSIENT = 0x0080;
        /// Method: Declared with a variable number of arguments at the source code level.
        const VARARGS = 0x0080;
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
#[track_caller]
fn unreachable_verified() -> ! {
    #[cfg(debug_assertions)]
    unreachable!();
    #[cfg(not(debug_assertions))]
    unsafe {
        std::hint::unreachable_unchecked();
    }
}
