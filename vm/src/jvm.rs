use alloc::alloc::Layout;
use alloc::sync::Arc;
use core::sync::atomic::{AtomicU32, Ordering::*};
use std::sync::{Mutex, OnceLock, RwLock};

use crate::heap::Heap;
use crate::hot_reloading::{ReloadErr, reload_method_bytecodes_for_class};
use crate::identity_hash::IdentityHashes;
use crate::monitor::{ParkingLot, maybe_loom};
use crate::{AccessFlags, ClassLibraryIntegration, NativeMethod};
use fixed_typed_arena::ManuallyDropArena;
// TODO: posibly exchange for different hasher (benchmark)
use hashbrown::HashMap;

use crate::method::*;
use crate::object::FieldStorage;
use crate::object::Object;
use crate::parse;
use crate::typ::Type;
use crate::value::Value;
use crate::{class::*, default};

/// Err is an object that extends Throwable
pub(crate) type JvmResult<T> = Result<T, Object>;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub(crate) struct JvmId(u32);

/// Provides class files.
pub trait ClassLoader: Send + Sync {
    /// Get the bytes of the classfile representing the class.
    // TODO: Return result for better error message
    fn load(&self, name: &str) -> Option<Vec<u8>>;
}

/// Creates & manages classes and objects, which borrow from the Jvm.
/// Multiple independent `Jvm`s may exist.
/// Mutable shared access follows Java's rules.
///
/// To operate correctly, the JVM requires a class library. The minimal
/// version necessary to implement this is available under the `classes`
/// folder of this repository. These aren't loaded automatically, your
/// class library needs to be provided by the class loader. Those classes missing
/// or being invalid classes may cause the thread to hang or crash.
pub struct Jvm {
    /// Uniquely identifies the Jvm to prevent mixing of objects between Jvms.
    pub(crate) id: JvmId,
    /// Currently there's a single class loader per Jvm.
    pub bootstrap_class_loader: Box<dyn ClassLoader>,
    pub(crate) heap: Heap,
    // Classes (including arrays), methods, method descriptors and interned strings live as long as the JVM.
    // TODO: alloc on heap
    pub(crate) method_storage: Mutex<ManuallyDropArena<Method, 128>>,
    pub(crate) classes: RwLock<HashMap<Arc<str>, ClassRef>>,
    pub(crate) identity_hashes: IdentityHashes,
    pub(crate) parking_lot: ParkingLot,
    pub(crate) unbound_native_methods: Mutex<HashMap<String, UnboundNativeMethods>>,
    pub(crate) stackoverflow_error: OnceLock<Object>,
    pub(crate) class_library_integration: ClassLibraryIntegration,
}

impl Jvm {
    /// Construct a new JVM.
    ///
    /// NOTE: The Jvm will preallocate `max_heap` bytes, relying on memory overcommit.
    /// On 64bit systems, `max_heap` is limited to 2³⁵ bytes (32Gb).
    pub fn new(
        base_class_loader: Box<dyn ClassLoader>,
        mut class_library_integration: ClassLibraryIntegration,
        max_heap: usize,
    ) -> Self {
        static NEXT_ID: AtomicU32 = AtomicU32::new(0);
        let id = JvmId(NEXT_ID.fetch_add(1, SeqCst));
        if id.0 == u32::MAX {
            panic!(
                "You've created 4294967296 jvm instances. You've had your fun but that's enough for today."
            )
        }
        let mut native_methods = HashMap::<String, UnboundNativeMethods>::new();
        for method in class_library_integration.native_methods.drain(..) {
            let UnboundNativeMethods::Available(methods) = native_methods
                .entry(method.class.clone())
                .or_insert(UnboundNativeMethods::Available(Vec::new()))
            else {
                unreachable!()
            };
            methods.push(method);
        }
        Self {
            id,
            bootstrap_class_loader: base_class_loader,
            heap: Heap::new(max_heap),
            method_storage: default(),
            classes: default(),
            identity_hashes: default(),
            parking_lot: default(),
            unbound_native_methods: Mutex::new(native_methods),
            stackoverflow_error: OnceLock::new(),
            class_library_integration,
        }
    }

    /// Returns the class, loading it if it wasn't already loaded.
    /// This also loads its superclass and any interfaces it implements.
    /// Does not initialize this class, nor load any other class referenced.
    /// `name` must be fully qualified and may describe an array.
    pub fn resolve_class(&self, name: &str) -> crate::JvmResult<crate::Class> {
        self.resolve_class_internal(name)
            .map(|value| crate::Class::new(self.id, value))
            .map_err(|value| crate::Object::new(self.id, value))
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
        self.resolve_class_internal(class)
            .map_err(|value| crate::Object::new(self.id, value))?
            .methods
            .get(&nat)
            .ok_or_else(|| {
                throw(
                    self,
                    "NoSuchMethodError",
                    &*format!("{class}.{method}:{nat:?}"),
                )
            })
            .map(|value| crate::Method::new(self.id, value))
            .map_err(|value| crate::Object::new(self.id, value))
    }

    /// Reload the bytecodes of all methods in the class by querying the class loader.
    /// Fails if there are structural changes to the class (e.g. a field is added of a method signature has changed).
    ///
    /// Reloading a class while it is getting verified or getting initialized (or vice-versa) may deadlock.
    pub fn reload_method_bytecodes_for_class(&self, class: &str) -> Result<(), ReloadErr> {
        reload_method_bytecodes_for_class(self, class)
    }

    /// Defines extra metadata to be stored on objects of the specified class.
    /// Must be set before the class has been loaded.
    ///
    /// # Panics
    /// Panics if metadata has already been defined for this class,
    /// or the class has already been loaded.
    pub fn define_object_metadata_layout(&mut self, class: &str, layout: Layout) {
        if self.classes.read().unwrap().contains_key(class) {
            panic!("Tried to set metadata for already loaded class {class}");
        }
        if self
            .class_library_integration
            .metatada_layout
            .contains_key(class)
        {
            panic!("Tried to set metadata twice for class {class}");
        }
        self.class_library_integration
            .metatada_layout
            .insert(class.into(), layout);
    }

    /// Returns the class representing arrays of the given component.
    /// If the component is a class, this also loads it, its superclass (recursively) and any interfaces it implements.
    /// Does not initialize this class nor load any other class referenced.
    pub(crate) fn resolve_array_of(&self, component: &Type) -> JvmResult<ClassRef> {
        let name: Arc<str> = format!("{component}[]").into();
        if let Some(class) = self.classes.read().unwrap().get(&name) {
            return Ok(*class);
        }

        let top = self.resolve_class_internal("java.lang.Object")?;

        // Arrays of primitives inherit from objects,
        // other arrays inherit from arrays of the superclass of their component type
        let super_class = if let Type::Ref(component) = &component {
            let component = self.resolve_class_internal(component)?;
            if let Some(comp_super) = component.super_class {
                self.resolve_array_of(&comp_super.typ())?
            } else {
                self.resolve_class_internal("java.lang.Object")?
            }
        } else {
            top
        };

        let array = Class {
            name: name.clone(),
            package: "java.lang".into(),
            super_class: Some(super_class),
            element_type: Some(component.clone()),
            const_pool: default(),
            access_flags: AccessFlags::PUBLIC | AccessFlags::FINAL | AccessFlags::SYNTHETIC,
            source_file_name: None,
            interfaces: vec![
                self.resolve_class_internal("java.lang.Cloneable")?,
                self.resolve_class_internal("java.io.Serializable")?,
            ],
            fields: default(),
            methods: super_class.methods.clone(),
            static_storage: FieldStorage::EMPTY,
            static_monitor: maybe_loom::AtomicU32::new(0),
            object_size: 0,
            metadata_offset: 0,
            init: ClassState::Unverified.into(),
            init_waiter: default(),
            class_object: default(),
        };
        let array = self.heap.alloc_class(array);
        let mut guard = self.classes.write().unwrap();
        // Check if loaded by another thread in the meantime
        if let Some(array) = guard.get(&name) {
            return Ok(*array);
        }
        guard.insert(name, array);
        Ok(array)
    }

    /// Returns the class, loading it if it wasn't already loaded.
    /// This also loads its superclass and any interfaces it implements.
    /// Does not initialize this class nor load any other class referenced.
    /// `name` must be fully qualified and may describe an array.
    pub(crate) fn resolve_class_internal(&self, name: &str) -> JvmResult<ClassRef> {
        self.resolve_class_impl(name, &mut Vec::new())
    }

    /// This is only to be called by `resolve_class` and errors out on loops in the class hierarchy
    fn resolve_class_impl(
        &self,
        name: &str,
        check_circular: &mut Vec<String>,
    ) -> JvmResult<ClassRef> {
        if let Some(class) = self.classes.read().unwrap().get(name) {
            return Ok(*class);
        }

        // Do we want to load an array (normal name)?
        if let Some(component) = name.strip_suffix("[]") {
            return self.resolve_array_of(&component.into());
        }

        let package = name.rsplit_once('.').map(|(p, _)| p).unwrap_or_default();

        if check_circular.iter().any(|c| c == name) {
            return Err(throw(self, "ClassCircularityError", name));
        }
        check_circular.push(name.to_owned());

        // We want to load a normal class
        let bytes = self.bootstrap_class_loader.load(name).ok_or_else(|| {
            if name == "java.lang.NoClassDefFoundError" {
                panic!("Required base classes missing");
            }
            throw(self, "NoClassDefFoundError", name)
        })?;

        let mut class_desc = parse::read_class_file(&bytes, self)?;

        let mut methods = HashMap::default();
        for (nat, method) in class_desc.methods.drain() {
            let method = &*self.method_storage.lock().unwrap().alloc(method);
            methods.insert(nat, method);
        }

        if name != class_desc.name.as_ref() {
            return Err(throw(
                self,
                "NoClassDefFoundError",
                &*format!("{name} != {}", class_desc.name.as_ref()),
            ));
        }

        let new_methods = methods.clone();

        // Superclasses resolution
        let super_class = if let Some(super_class_name) = class_desc.super_class {
            let super_class = self.resolve_class_impl(&super_class_name, check_circular)?;

            if super_class.element_type.is_some()
                | super_class
                    .access_flags
                    .intersects(AccessFlags::FINAL | AccessFlags::INTERFACE)
            {
                return Err(throw(self, "IncompatibleClassChangeError", name));
            }

            // Method resolution
            for (nat, method) in &super_class.methods {
                methods.entry(nat.clone()).or_insert(*method);
            }
            for (nat, method) in &super_class.methods {
                if !method.access_flags.contains(AccessFlags::PRIVATE) {
                    methods.entry(nat.clone()).or_insert(*method);
                }
            }
            Some(super_class)
        } else {
            None
        };

        // Interface resolution
        let mut interfaces = Vec::new();
        for interface in class_desc.interfaces {
            let interface = self.resolve_class_impl(&interface, check_circular)?;
            if !interface.is_interface() {
                return Err(throw(self, "IncompatibleClassChangeError", name));
            }
            interfaces.push(interface);

            // Method resolution
            for (nat, method) in &interface.methods {
                if !method
                    .access_flags
                    .intersects(AccessFlags::PRIVATE | AccessFlags::STATIC)
                {
                    methods.entry(nat.clone()).or_insert(*method);
                }
            }
        }

        // Interfaces must directly inherit from object (5.3.5)
        if class_desc.access_flags.contains(AccessFlags::INTERFACE)
            & (super_class.map(|c| &*c.name) != Some("java.lang.Object"))
        {
            return Err(throw(self, "IncompatibleClassChangeError", name));
        }

        let metadata_layout = self
            .class_library_integration
            .metatada_layout
            .get(name)
            .copied()
            .unwrap_or(Layout::new::<()>());

        let class_layout = Class::set_layouts(super_class, class_desc.fields, metadata_layout);
        let static_storage = if class_layout.static_size == 0 {
            FieldStorage::EMPTY
        } else {
            FieldStorage(self.heap.alloc_object(class_layout.static_size).get())
        };

        let ref_type = self.heap.alloc_class(Class {
            name: class_desc.name,
            package: package.into(),
            super_class,
            element_type: None,
            const_pool: RwLock::new(class_desc.const_pool),
            access_flags: class_desc.access_flags,
            source_file_name: class_desc.source_file_name,
            interfaces,
            fields: class_layout.fields,
            methods,
            static_storage,
            static_monitor: maybe_loom::AtomicU32::new(0),
            object_size: class_layout.object_size,
            metadata_offset: class_layout.metadata_offset,
            init: ClassState::Unverified.into(),
            init_waiter: default(),
            class_object: default(),
        });

        let mut guard = self.classes.write().unwrap();
        // Check if loaded by another thread in the meantime
        if let Some(class) = guard.get(name) {
            return Ok(*class);
        }
        guard.insert(name.into(), ref_type);
        let class = guard.get(name).unwrap();

        // While parsing, each method has their class field set to a dummy
        // We can't use class.methods directly because that also includes parent classes
        for method in new_methods.values() {
            method
                .class
                .store(*class as *const Class as *mut Class, SeqCst);
        }
        for field in class.fields.values() {
            field
                .class
                .store(*class as *const Class as *mut Class, SeqCst);
        }

        check_circular.pop();
        Ok(ref_type)
    }

    /// Creates an instance of a non-array class on the heap.
    /// Class must be initialized.
    ///
    /// # Panics
    /// Panics if class is an array class or interface.
    #[track_caller]
    pub(crate) fn create_object_skipping_constructor(&self, class: ClassRef) -> Object {
        if class.element_type.is_some() | class.is_interface() {
            panic!("{} is an array or interface type", class.name)
        }
        Object::new_uninitialized(&self.heap, class)
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
            self.id,
            self.create_object_skipping_constructor(class_ptr.as_ref()),
        );
        args.insert(0, crate::Value::Ref(Some(object)));
        let constructor = class_ptr
            .method(self, "<init>", arg_types, None)
            .ok_or_else(|| {
                crate::Object::new(
                    self.id,
                    throw(self, "NoSuchMethodError", &*format!("{class} <init> …")),
                )
            })?;
        constructor.invoke(self, &args)?;
        Ok(object)
    }

    /// Creates an array.
    ///
    /// # Panics
    /// Panics if length < 0
    #[track_caller]
    pub fn create_array(&self, component: &Type, length: i32) -> crate::Object {
        crate::Object::new(self.id, self.create_array_of(component, length))
    }

    /// Creates an array.
    ///
    /// # Panics
    /// Panics if length < 0 or the component is a class that fails to load.
    #[track_caller]
    pub(crate) fn create_array_of(&self, component: &Type, length: i32) -> Object {
        if length < 0 {
            panic!("array length < 0")
        }
        let class = self
            .resolve_array_of(component)
            .expect("Failed to resolve class");
        Object::new_array(&self.heap, class, length)
    }
}

impl Eq for Jvm {
    fn assert_receiver_is_total_eq(&self) {}
}

impl PartialEq for Jvm {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Drop for Jvm {
    fn drop(&mut self) {
        // SAFETY: All references have lifetime 'a and no member of JVM has a drop impl that dereferences them
        unsafe {
            ManuallyDropArena::drop(&mut self.method_storage.lock().unwrap());
        }
    }
}

/// For exceptions and errors thrown by the JVM (instead of by athrow).
/// `name` is unqualified.
#[track_caller]
pub(crate) fn throw<'a>(jvm: &Jvm, name: &str, msg: impl Into<Option<&'a str>>) -> Object {
    new_throwable(jvm, name, msg.into())
}

pub(crate) fn new_throwable(jvm: &Jvm, name: &str, msg: Option<&str>) -> Object {
    let message = if let Some(msg) = msg {
        format!("{name}: {msg}")
    } else {
        name.to_owned()
    };
    let object = match jvm.resolve_class_internal(&format!("java.lang.{name}")) {
        Ok(class) => {
            if let Err(thrown) = class.ensure_init(jvm) {
                thrown
            } else {
                jvm.create_object_skipping_constructor(class)
            }
        }
        Err(thrown) => thrown,
    };
    let field = object
        .class(jvm.heap.ptrs())
        .field("message", Type::Ref("java.lang.String".into()))
        .expect("java.lang.Throwable missing field java.lang.String message");
    let string =
        (jvm.class_library_integration.string_constructor)(jvm, &message).into_inner(jvm.id);
    field.instance_set(jvm.heap.ptrs(), object, Value::Ref(Some(string)));
    object
}

#[allow(clippy::extra_unused_type_parameters)]
fn _assert_jvm_is_send_sync<T: Send + Sync>() {
    if false {
        _assert_jvm_is_send_sync::<Jvm>()
    }
}

#[derive(Debug)]
pub(crate) enum UnboundNativeMethods {
    Available(Vec<NativeMethod>),
    ClassAlreadyInitialized,
}
