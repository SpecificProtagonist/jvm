use core::alloc::Layout;
use core::fmt::{Debug, Display};
use core::hash::Hash;
use core::ptr::NonNull;
use core::sync::atomic::Ordering::*;

use crate::hot_reloading::{ReloadErr, reload_method_bytecodes_for_class};
use crate::method::MethodNaT;
use crate::*;

#[derive(Eq, PartialEq)]
pub struct Jvm {
    inner: NonNull<jvm::Jvm>,
}

impl From<&jvm::Jvm> for Jvm {
    fn from(jvm: &jvm::Jvm) -> Self {
        let uncounted = Self {
            inner: NonNull::from(jvm),
        };
        let handle = uncounted.clone();
        core::mem::forget(uncounted);
        handle
    }
}

unsafe impl Send for Jvm {}
unsafe impl Sync for Jvm {}

impl Clone for Jvm {
    fn clone(&self) -> Self {
        let prev = self.deref().refcount.fetch_add(1, Relaxed);
        if prev > usize::MAX / 2 {
            self.deref().force_alive.store(true, Release);
        }
        Self { inner: self.inner }
    }
}

impl Drop for Jvm {
    fn drop(&mut self) {
        let prev = self.deref().refcount.fetch_sub(1, Release);
        if (prev > 1) || (self.deref().force_alive.load(Relaxed)) {
            // Still references alive
            return;
        }
        // SAFETY: There are no other references anymore
        unsafe {
            self.inner.read();
            alloc::alloc::dealloc(self.inner.as_ptr() as *mut u8, Layout::new::<jvm::Jvm>());
        }
    }
}

impl Jvm {
    pub(crate) fn deref(&self) -> &jvm::Jvm {
        unsafe { self.inner.as_ref() }
    }

    pub(crate) fn deref_mut(&mut self) -> &mut jvm::Jvm {
        unsafe { self.inner.as_mut() }
    }

    pub fn new(
        base_class_loader: Box<dyn ClassLoader>,
        class_library_integration: ClassLibraryIntegration,
        max_heap: usize,
    ) -> Self {
        let jvm = jvm::Jvm::new(base_class_loader, class_library_integration, max_heap);
        Self {
            inner: Box::leak(Box::new(jvm)).into(),
        }
    }

    /// Returns the class, loading it if it wasn't already loaded.
    /// This also loads its superclass and any interfaces it implements.
    /// Does not initialize this class, nor load any other class referenced.
    /// `name` must be fully qualified and may describe an array.
    pub fn resolve_class(&self, name: &str) -> crate::JvmResult<crate::Class> {
        self.deref()
            .resolve_class_internal(name)
            .map(|value| crate::Class::new(self.deref(), value))
            .map_err(|value| crate::Object::new(self.deref(), value))
    }

    /// Creates an object via constructor.
    ///
    /// # Panics
    /// Panics if class is an array class or interface.
    #[track_caller]
    pub fn create_object(
        &self,
        class: &str,
        arg_types: impl IntoIterator<Item = Type>,
        mut args: Vec<crate::Value>,
    ) -> crate::JvmResult<crate::Object> {
        let class_ptr = self.resolve_class(class)?;
        let object = crate::Object::new(
            self.clone(),
            self.deref()
                .create_object_skipping_constructor(class_ptr.value),
        );
        args.insert(0, crate::Value::Ref(Some(object.clone())));
        let constructor = class_ptr.method("<init>", arg_types, None).ok_or_else(|| {
            crate::Object::new(
                self.clone(),
                throw(
                    self.deref(),
                    "NoSuchMethodError",
                    &*format!("{class} <init> …"),
                ),
            )
        })?;
        constructor.invoke(args.into_iter())?;
        Ok(object)
    }

    /// Reload the bytecodes of all methods in the class by querying the class loader.
    /// Fails if there are structural changes to the class (e.g. a field is added of a method signature has changed).
    ///
    /// Reloading a class while it is getting verified or getting initialized (or vice-versa) may deadlock.
    pub fn reload_method_bytecodes_for_class(&self, class: &str) -> Result<(), ReloadErr> {
        reload_method_bytecodes_for_class(self.deref(), class)
    }

    /// Convenience method.
    /// Includes inherited dynamic methods.
    pub fn resolve_method(
        &self,
        class: &str,
        method: &str,
        args: impl IntoIterator<Item = Type>,
        returns: Option<Type>,
    ) -> crate::JvmResult<crate::Method> {
        let nat = MethodNaT {
            name: method.into(),
            typ: MethodDescriptor {
                args: args.into_iter().collect(),
                returns,
            },
        };
        self.deref()
            .resolve_class_internal(class)
            .map_err(|value| crate::Object::new(self.deref(), value))?
            .methods
            .get(&nat)
            .ok_or_else(|| {
                throw(
                    self.deref(),
                    "NoSuchMethodError",
                    &*format!("{class}.{method}:{nat:?}"),
                )
            })
            .map(|value| crate::Method::new(self.deref(), value))
            .map_err(|value| crate::Object::new(self.deref(), value))
    }

    /// Defines extra metadata to be stored on objects of the specified class.
    /// Must be set before the class has been loaded.
    ///
    /// # Panics
    /// Panics if metadata has already been defined for this class,
    /// or the class has already been loaded.
    pub fn define_object_metadata_layout(&mut self, class: &str, layout: Layout) {
        if self.deref().classes.read().unwrap().contains_key(class) {
            panic!("Tried to set metadata for already loaded class {class}");
        }
        if self
            .deref()
            .class_library_integration
            .metatada_layout
            .contains_key(class)
        {
            panic!("Tried to set metadata twice for class {class}");
        }
        self.deref_mut()
            .class_library_integration
            .metatada_layout
            .insert(class.into(), layout);
    }

    /// Creates an array.
    ///
    /// # Panics
    /// Panics if length < 0
    pub fn create_array(&self, component: &Type, length: i32) -> crate::Object {
        crate::Object::new(
            self.deref(),
            self.deref().create_array_of(component, length),
        )
    }

    pub fn create_string(&self, str: &str) -> crate::Object {
        (self.deref().class_library_integration.string_constructor)(self, str)
    }

    /// # Panics
    /// Panics if `class` is not an instance of `java.lang.Class`.
    pub fn get_class_from_class_object(&self, class: crate::Object) -> crate::Class {
        assert_eq!(
            &*class.value.class(self.deref().heap.ptrs()).name,
            "java.lang.Class"
        );
        (self
            .deref()
            .class_library_integration
            .get_class_from_class_object)(self, class)
    }
}

/// Guard types ensure values can't be used without the vm they belong to / be used with the wrong vm.
// TODO: Change this to an implementation similar to `Arc`
// in preparation for garbage collectionn (and a more pleasant api).
//
// When unsafe binder types are available, try them out.
macro_rules! define_guard {
    ($(#[$doc:meta])* $guard_typ:ident, $impl_typ:ty) => {
        $(#[$doc])*
        #[derive(Clone, Eq)]
        pub struct $guard_typ {
            pub(crate) value: &'static $impl_typ,
            pub(crate) jvm: Jvm,
        }

        impl $guard_typ {
            pub(crate) fn new(jvm: impl Into<Jvm>, value: &'static $impl_typ) -> Self {
                Self { value, jvm: jvm.into() }
            }

            #[allow(dead_code)]
            pub(crate) fn as_inner(&self, jvm: &Jvm) -> &'static $impl_typ {
                if &self.jvm == jvm {
                    unsafe {(self.value as *const $impl_typ).as_ref_unchecked()}
                } else {
                    panic!("Wrong JVM accessed")
                }
            }
        }

        impl Debug for $guard_typ {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                write!(f, "{:p}", self.value as *const $impl_typ)
            }
        }

        impl Hash for $guard_typ {
            fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
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
#[derive(Clone, Eq)]
pub struct Object {
    pub(crate) value: object::Object,
    pub(crate) jvm: Jvm,
}

impl Hash for Object {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
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

impl core::fmt::Pointer for Object {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        core::fmt::Pointer::fmt(&self.value, f)
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        object::fmt_object(self.jvm.deref().heap.ptrs(), self.value, f, true)
    }
}

impl Object {
    pub(crate) fn new(jvm: impl Into<Jvm>, value: object::Object) -> Self {
        Self {
            value,
            jvm: jvm.into(),
        }
    }

    #[allow(dead_code)]
    pub(crate) fn as_inner(&self, jvm: &Jvm) -> object::Object {
        if &self.jvm == jvm {
            self.value
        } else {
            panic!("Wrong JVM accessed")
        }
    }

    /// The class this is an instance of.
    pub fn class(&self) -> Class {
        let class = self.value.class(self.jvm.deref().heap.ptrs());
        Class::new(self.jvm.clone(), class)
    }

    /// Get the value of an object's field.
    ///
    /// # Panics
    /// Panics if the field does not exist or is static.
    pub fn get(&self, field_name: &str, field_type: Type) -> Value {
        let Some(field) = self.class().field(field_name, field_type.clone()) else {
            panic!(
                "Field {field_name} of type {field_type} not found in object of type {}",
                self.class().value
            )
        };
        Value::new(
            &self.jvm,
            field
                .value
                .instance_get(self.jvm.deref().heap.ptrs(), self.value),
            &field.value.nat.typ,
        )
    }

    /// Set the value of an object's field.
    ///
    /// # Panics
    /// Panics if the field does not exist or is static or
    /// the value is not assignable to the field's type
    /// or is an object created by a different JVM
    pub fn set(&self, field_name: &str, field_type: Type, value: impl Into<Value>) {
        let Some(field) = self.class().field(field_name, field_type.clone()) else {
            panic!(
                "Field {field_name} of type {field_type} not found in object of type {}",
                self.class().value
            )
        };
        field.as_inner(&self.jvm).instance_set(
            self.jvm.deref().heap.ptrs(),
            self.value,
            value.into().into_inner(&self.jvm),
        )
    }

    /// Returns whether this is an array.
    pub fn is_array(&self) -> bool {
        self.value.is_array(self.jvm.deref().heap.ptrs())
    }

    /// Returns the number of items in this array.
    ///
    /// # Panics
    /// Panics if the object is not an array.
    #[track_caller]
    pub fn array_length(&self) -> i32 {
        if self.class().element_type().is_none() {
            panic!("not an array");
        }
        self.value.array_length()
    }

    /// Get an array element.
    ///
    /// # Panics
    /// Panics if the object is not an array or the `index` is out of bounds.
    #[track_caller]
    pub fn array_read(&self, index: i32) -> Value {
        Value::new(
            &self.jvm,
            self.value
                .array_read_value(self.jvm.deref().heap.ptrs(), index),
            self.class().element_type().as_ref().unwrap(),
        )
    }

    /// Sets an array element. If the element type is boolean, byte, short or char,
    /// the `JvmValue::Int` is truncated
    ///
    /// # Panics
    /// Panics if object is not an array, the element type mismatches, `index` is out of bounds
    /// or the value is an object created by another JVM.
    #[track_caller]
    pub fn array_write(&self, index: i32, value: Value) {
        self.value.array_write_value(
            self.jvm.deref().heap.ptrs(),
            index,
            value.into_inner(&self.jvm),
        )
    }

    /// Enters this object's monitor, blocking if it is already locked by another thread.
    ///
    /// This is reëntrant: A thread may enter the monitor multiple times, and must [`exit`](Object::exit_monitor)
    /// is as many times to unlock the object.
    pub fn enter_monitor(&self) {
        self.jvm
            .deref()
            .parking_lot
            .monitor_enter(self.value.mark_word());
    }

    /// Exists this object's monitor.
    /// After 2³¹-1 more calls to `enter_monitor` on the same thread than to `exit_monitor`, this will fail to release the lock.
    ///
    /// Throws `IllegalMonitorStateException` is the object is not locked by this thread.
    pub fn exit_monitor(&self) -> JvmResult<()> {
        self.jvm
            .deref()
            .parking_lot
            .monitor_exit(self.value.mark_word())
            .map_err(|_| {
                Object::new(
                    self.jvm.clone(),
                    throw(self.jvm.deref(), "IllegalMonitorStateException", None),
                )
            })
    }

    /// Wait on this object until notified by [`notify`](Object::notify) or [`notify_all`](Object::notify_all),
    /// or until spuriously woken up.
    ///
    /// This thread must hold the object's lock beforehand (otherwise `IllegalMonitorStateException` will be thrown)
    /// and will reäquire the lock when woken.
    pub fn wait(&self) -> JvmResult<()> {
        self.jvm
            .deref()
            .parking_lot
            .monitor_wait(self.value.mark_word())
            .map_err(|_| {
                Object::new(
                    self.jvm.clone(),
                    throw(self.jvm.deref(), "IllegalMonitorStateException", None),
                )
            })
    }

    /// Notifies a thread [`wait`ing](Object::wait) on this object.
    ///
    /// This thread must hold the object's lock (otherwise `IllegalMonitorStateException` will be thrown).
    pub fn notify(self) -> JvmResult<()> {
        self.jvm
            .deref()
            .parking_lot
            .monitor_notify(self.value.mark_word())
            .map_err(|_| {
                Object::new(
                    self.jvm.clone(),
                    throw(self.jvm.deref(), "IllegalMonitorStateException", None),
                )
            })
    }

    /// Notifies all threads [`wait`ing](Object::wait) on this object.
    ///
    /// This thread must hold the object's lock (otherwise `IllegalMonitorStateException` will be thrown).
    pub fn notify_all(self) -> JvmResult<()> {
        self.jvm
            .deref()
            .parking_lot
            .monitor_notify_all(self.value.mark_word())
            .map_err(|_| {
                Object::new(
                    self.jvm.clone(),
                    throw(self.jvm.deref(), "IllegalMonitorStateException", None),
                )
            })
    }

    /// Creates a debug representation of the object. For a full object tree, use `Debug`.
    pub fn format_short(self) -> String {
        self.value.format_short(self.jvm.deref().heap.ptrs())
    }

    pub fn identity_hash(self) -> i32 {
        self.jvm
            .deref()
            .identity_hashes
            .get_identity_hash(self.value)
    }
}

define_guard!(
    /// A class, interface, enum or array class.
    Class,
    class::Class
);

impl Class {
    /// Fully qualified name.
    pub fn name(&self) -> &str {
        &self.value.name
    }

    /// Returns the super class. Is `Some()` for all classes except `java.lang.Object`.
    pub fn super_class(&self) -> Option<Self> {
        self.value
            .super_class
            .map(|value| Class::new(self.jvm.clone(), value))
    }

    /// Returns the array class which has this class as its element type.
    pub fn to_array(&self) -> Self {
        Class::new(
            self.jvm.clone(),
            self.jvm.deref().resolve_array_of(&self.typ()).unwrap(),
        )
    }

    /// Convenience method for getting this class' type.
    pub fn typ(&self) -> Type {
        self.value.typ()
    }

    pub fn is_interface(&self) -> bool {
        self.value.is_interface()
    }

    /// Is `Some()` for classes representing an array and `None` otherwise.
    pub fn element_type(&self) -> Option<Type> {
        self.value.element_type.clone()
    }

    pub fn access_flags(&self) -> AccessFlags {
        self.value.access_flags
    }

    /// Name of the source file this class was compiled from. Not an absolute path.
    pub fn source_file_name(&self) -> Option<&str> {
        self.value.source_file_name.as_deref()
    }

    /// Look up a field in this class by its name and type.
    /// Does not search the inheritance hierarchy.
    /// There may be multiple fields of the same name.
    pub fn field(&self, name: &str, typ: Type) -> Option<Field> {
        self.value
            .field(name, typ)
            .map(|value| Field::new(self.jvm.clone(), value))
    }

    /// Look up a method by its name and type.
    /// There may be multiple methods of the same name, as long as they differ in type.
    /// Includes inherited dynamic methods.
    pub fn method(
        &self,
        name: &str,
        args: impl IntoIterator<Item = Type>,
        ret: Option<Type>,
    ) -> Option<Method> {
        self.method_by_descriptor(
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
    pub fn method_by_descriptor(&self, name: &str, typ: &MethodDescriptor) -> Option<Method> {
        self.value
            .methods
            .get(&MethodNaTBorrow { name, typ })
            .map(|value| Method::new(self.jvm.clone(), value))
    }

    /// Check whether this class is allowed to acces `other`
    pub fn can_access_class(&self, other: &Class) -> bool {
        self.value.can_access_class(other.as_inner(&self.jvm))
    }

    /// Ensures this class is initialized. Includes linking, verification, preparation & resolution.
    /// This happens automatically when a method belonging to this class is called.
    pub fn ensure_init(&self) -> JvmResult<()> {
        self.value
            .ensure_init(self.jvm.deref())
            .map_err(|value| Object::new(self.jvm.clone(), value))
    }

    /// Creates an instance of a non-array class on the heap without invoking its constructor.
    ///
    /// Returns `Err` if initializing the class fails.
    ///
    /// # Panics
    /// Panics if this is an array class or interface.
    #[track_caller]
    pub fn create_object_no_constructor(&self) -> JvmResult<Object> {
        self.value
            .ensure_init(self.jvm.deref())
            .map_err(|err| Object::new(self.jvm.clone(), err))?;
        Ok(Object::new(
            self.jvm.clone(),
            self.jvm
                .deref()
                .create_object_skipping_constructor(self.value),
        ))
    }

    /// Creates an object via constructor.
    ///
    /// # Panics
    /// Panics if this is an array class or interface.
    #[track_caller]
    pub fn create_object(
        &self,
        arg_types: impl IntoIterator<Item = Type>,
        mut args: Vec<Value>,
    ) -> JvmResult<Object> {
        self.value
            .ensure_init(self.jvm.deref())
            .map_err(|err| Object::new(self.jvm.clone(), err))?;
        let object = Object::new(
            self.jvm.clone(),
            self.jvm
                .deref()
                .create_object_skipping_constructor(self.value),
        );
        args.insert(0, Value::Ref(Some(object.clone())));
        let constructor = self.method("<init>", arg_types, None).ok_or_else(|| {
            crate::Object::new(
                self.jvm.clone(),
                throw(
                    self.jvm.deref(),
                    "NoSuchMethodError",
                    &*format!("{} <init> …", self.value),
                ),
            )
        })?;
        constructor.invoke(args.into_iter())?;
        Ok(object)
    }

    /// Get a pointer to this class' per-object metadata.
    /// The layout of this metadata is defined by [`Jvm::define_object_metadata_layout`].
    ///
    /// When an object is allocated, its metadata is zero-initialized.
    ///
    /// # Safety
    /// - object must be an instance of this class
    pub unsafe fn get_metadata(&self, object: &Object) -> *const () {
        // SAFETY: Same precondition
        unsafe { class::Class::get_metadata(self.value, object.as_inner(&self.jvm)) }
    }

    /// Get the instance of java.lang.Class that represents this class.
    pub fn class_object(&self) -> Object {
        Object::new(self.jvm.clone(), self.value.class_object(self.jvm.deref()))
    }
}

impl core::fmt::Display for Class {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.name())
    }
}

define_guard!(
    /// A static or instance field.
    Field,
    field::Field
);

impl Field {
    /// The class this field is defined by.
    pub fn class(self) -> Class {
        Class::new(self.jvm.clone(), self.value.class())
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
    pub fn typ(&self) -> &Type {
        &self.value.nat.typ
    }

    /// Get the value of a static field.
    /// Note that this might not be initialized yet unless a method has been called on this class.
    ///
    /// # Panics
    /// Panics if the field is not static.
    #[track_caller]
    pub fn static_get(&self) -> Value {
        Value::new(
            &self.jvm,
            self.value.static_get(self.jvm.deref().heap.ptrs()),
            self.typ(),
        )
    }

    /// Get the value of an object's field.
    ///
    /// # Panics
    /// Panics if the field is static or the object is not an instance of the class corresponding to this field.
    #[track_caller]
    pub fn instance_get(&self, object: &Object) -> Value {
        Value::new(
            &self.jvm,
            self.value
                .instance_get(self.jvm.deref().heap.ptrs(), object.as_inner(&self.jvm)),
            self.typ(),
        )
    }

    /// Set the value of a static field.
    ///
    /// # Panics
    /// Panics if the field is not static or the value is not assignable to the field's type
    /// or is an object created by a different JVM
    #[track_caller]
    pub fn static_set(&self, value: impl Into<Value>) {
        self.value.static_set(
            self.jvm.deref().heap.ptrs(),
            value.into().into_inner(&self.jvm),
        )
    }

    /// Set the value of an object's field.
    ///
    /// # Panics
    /// Panics if the field is static,
    /// the object is not an instance of the class corresponding to this field,
    /// the value is not assignable to the field's type
    /// or the value is an object created by a different JVM
    #[track_caller]
    pub fn instance_set(&self, object: &Object, value: impl Into<Value>) {
        self.value.instance_set(
            self.jvm.deref().heap.ptrs(),
            object.as_inner(&self.jvm),
            value.into().into_inner(&self.jvm),
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
    pub fn class(&self) -> Class {
        Class::new(self.jvm.clone(), self.value.class())
    }

    /// The name of this method. There may be multiple methods of the same
    /// name but differing types in the same class.
    pub fn name(&self) -> &str {
        &self.value.nat.name
    }

    /// Argument and return types of this method.
    pub fn typ(&self) -> &MethodDescriptor {
        &self.value.nat.typ
    }

    /// Access flags of this method.
    pub fn access_flags(&self) -> AccessFlags {
        self.value.access_flags
    }

    /// Calls the method, initializing its class if necessary.
    /// Returns either the function's return value or the exception that was thrown.
    ///
    /// # Panics
    /// Panics if any of the [`Value`]s is an object created by another JVM.
    pub fn invoke(&self, args: impl IntoIterator<Item = Value>) -> JvmResult<Option<Value>> {
        self.jvm
            .deref()
            .stackoverflow_error
            .get_or_init(|| jvm::new_throwable(self.jvm.deref(), "StackOverflowError", None));

        interpreter::invoke(
            self.jvm.deref(),
            self.value,
            args.into_iter().map(|arg| arg.into_inner(&self.jvm)),
        )
        .map(|returned| {
            returned.map(|value| Value::new(&self.jvm, value, self.typ().returns.as_ref().unwrap()))
        })
        .map_err(|e| Object::new(self.jvm.clone(), e))
    }

    /// Register code for a native method.
    ///
    /// The method must be marked as native and match the signature of `code`.
    /// If it is an instance method, its first argument will be the object it is invoked on.
    /// If called multiple times for the same method, the previous registration will be replaced.
    ///
    /// The callback returning a value of wrong type will result in a panic.
    ///
    /// # Example
    /// ```no_run
    /// # use vm::*;
    /// # let method: Method = todo!();
    /// method.bind_native_code(f32::powf)
    /// # .unwrap()
    /// ```
    // TODO: Note guarantees about available stack size
    #[allow(private_bounds)]
    pub fn bind_native_code<F: NativeFn<T>, T>(&self, code: F) -> Result<(), NativeBindError> {
        fake_variadics::bind_native_code(self.value, code)
    }
}

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
    fn new(jvm: &Jvm, value: value::Value, typ: &Type) -> Self {
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

impl Display for Value {
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

/// Err is an object that extends Throwable.
/// Currently no call stack is provided.
pub type JvmResult<T> = Result<T, Object>;
