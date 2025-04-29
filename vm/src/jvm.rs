use alloc::alloc::Layout;
use alloc::sync::Arc;
use core::sync::atomic::Ordering::*;
use core::sync::atomic::{AtomicBool, AtomicUsize};
use std::sync::{Mutex, OnceLock, RwLock};

use crate::heap::Heap;
use crate::identity_hash::IdentityHashes;
use crate::monitor::{ParkingLot, maybe_loom};
use crate::string_interning::StringInterner;
use crate::{AccessFlags, ClassLibraryIntegration, ClassLoader, NativeMethod};
use fixed_typed_arena::ManuallyDropArena;
// TODO: possibly exchange for different hasher (benchmark)
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

/// Creates & manages classes and objects, which borrow from the Jvm.
/// Multiple independent `Jvm`s may exist.
/// Mutable shared access follows Java's rules.
///
/// To operate correctly, the JVM requires a class library. The minimal
/// version necessary to implement this is available under the `classes`
/// folder of this repository. These aren't loaded automatically, your
/// class library needs to be provided by the class loader. Those classes missing
/// or being invalid classes may cause the thread to hang or crash.
pub(crate) struct Jvm {
    /// Currently there's a single class loader per Jvm.
    pub(crate) bootstrap_class_loader: Box<dyn ClassLoader>,
    pub(crate) heap: Heap,
    // Classes (including arrays), methods, method descriptors and interned strings live as long as the JVM.
    // TODO: alloc on heap
    pub(crate) method_storage: Mutex<ManuallyDropArena<Method, 128>>,
    pub(crate) classes: RwLock<HashMap<Arc<str>, ClassRef>>,
    pub(crate) identity_hashes: IdentityHashes,
    // TODO: Change to HashSet to avoid two copies of the string
    pub(crate) interned_strings: RwLock<StringInterner>,
    pub(crate) parking_lot: ParkingLot,
    pub(crate) unbound_native_methods: Mutex<HashMap<String, UnboundNativeMethods>>,
    pub(crate) stackoverflow_error: OnceLock<Object>,
    pub(crate) class_library_integration: ClassLibraryIntegration,
    /// Keeps the Jvm alive when > 0
    pub(crate) refcount: AtomicUsize,
    pub(crate) force_alive: AtomicBool,
    /// Offset into the data of a `java.lang.Class` object at which a pointer to
    /// the class it represents is stored.
    /// Should only be read once `java.lang.Class` is resolved.
    pub(crate) class_class_ptr_offset: AtomicUsize,
}

impl Jvm {
    /// Construct a new JVM.
    ///
    /// NOTE: The Jvm will preallocate `max_heap` bytes, relying on memory overcommit.
    /// On 64bit systems, `max_heap` is limited to 2³⁵ bytes (32Gb).
    pub(crate) fn new(
        base_class_loader: Box<dyn ClassLoader>,
        mut class_library_integration: ClassLibraryIntegration,
        max_heap: usize,
    ) -> Self {
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
            bootstrap_class_loader: base_class_loader,
            heap: Heap::new(max_heap),
            method_storage: default(),
            classes: default(),
            identity_hashes: default(),
            interned_strings: default(),
            parking_lot: default(),
            unbound_native_methods: Mutex::new(native_methods),
            stackoverflow_error: OnceLock::new(),
            class_library_integration,
            refcount: AtomicUsize::new(1),
            force_alive: AtomicBool::new(false),
            class_class_ptr_offset: AtomicUsize::default(),
        }
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

        let class_layout = Class::set_layouts(
            self,
            &class_desc.name,
            super_class,
            class_desc.fields,
            metadata_layout,
        );
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

    pub(crate) fn create_string(&self, str: &str) -> crate::Object {
        (self.class_library_integration.string_constructor)(&self.into(), str)
    }

    /// # Panics
    /// Panics if object is not a string.
    pub(crate) fn intern_string(&self, string: Object) -> Object {
        self.interned_strings
            .write()
            .unwrap()
            .intern_string(self, string)
    }

    pub(crate) fn create_interned_string(&self, string: &str) -> Object {
        self.interned_strings
            .write()
            .unwrap()
            .create_interned_string(self, string)
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
    let handle = crate::Jvm::from(jvm);
    let string =
        (jvm.class_library_integration.string_constructor)(&handle, &message).as_inner(&handle);
    field.instance_set(jvm.heap.ptrs(), object, Value::Ref(Some(string)));
    object
}

#[allow(clippy::extra_unused_type_parameters)]
const fn _assert_send_sync<T: Send + Sync>() {}
const _: () = _assert_send_sync::<Jvm>();

#[derive(Debug)]
pub(crate) enum UnboundNativeMethods {
    Available(Vec<NativeMethod>),
    ClassAlreadyInitialized,
}
