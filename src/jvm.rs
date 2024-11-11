use crate::heap::Heap;
use crate::hot_reloading::{reload_method_bytecodes_for_class, ReloadErr};
use crate::{class_loader::ClassLoader, AccessFlags};
use fixed_typed_arena::ManuallyDropArena;
use std::alloc::Layout;
use std::sync::atomic::AtomicU32;
use std::sync::{atomic::Ordering, Arc};
use std::sync::{Mutex, RwLock};
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
pub(crate) type JvmResult<T> = std::result::Result<T, Object>;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub(crate) struct JvmId(u32);

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
    /// Uniquely identifies the Jvm to prevent mixing of objects between Jvms
    pub(crate) id: JvmId,
    /// Currently there's a single class loader per Jvm.
    pub bootstrap_class_loader: Box<dyn ClassLoader>,
    pub(crate) heap: Heap,
    // Classes (including arrays), methods, method descriptors and interned strings live as long as the JVM
    // TODO: alloc on heap
    pub(crate) method_storage: Mutex<ManuallyDropArena<Method, 128>>,
    pub(crate) classes: RwLock<HashMap<Arc<str>, ClassRef>>,
}

impl Jvm {
    /// Construct a new JVM.
    pub fn new(base_class_loader: Box<dyn ClassLoader>) -> Self {
        static NEXT_ID: AtomicU32 = AtomicU32::new(0);
        let id = JvmId(NEXT_ID.fetch_add(1, Ordering::SeqCst));
        if id.0 == u32::MAX {
            panic!("You've created 4294967296 jvm instances. You've had your fun but that's enough for today.")
        }
        let jvm = Self {
            id,
            bootstrap_class_loader: base_class_loader,
            heap: default(),
            method_storage: default(),
            classes: default(),
        };
        if let Err(err) = jvm
            .resolve_class("java.lang.StackOverflowError")
            .and_then(|c| c.ensure_init(&jvm))
        {
            panic!("{err:?}");
        }
        jvm
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
                error(
                    self,
                    "NoSuchMethodError",
                    &format!("{class}.{method}:{nat:?}"),
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

    /// Returns the class representing arrays of the given component.
    /// If the component is a class, this also loads it, its superclass (recursively) and any interfaces it implements.
    /// Does not initialize this class nor load any other class referenced.
    pub(crate) fn resolve_array_of(&self, component: &Type) -> JvmResult<ClassRef> {
        let name: Arc<str> = format!("{component}[]").into();
        if let Some(class) = self.classes.read().unwrap().get(&name) {
            return Ok(*class);
        }

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
            self.resolve_class_internal("java.lang.Object")?
        };

        let array = Class {
            name: name.clone(),
            super_class: Some(super_class),
            element_type: Some(component.clone()),
            const_pool: default(),
            access_flags: AccessFlags::PUBLIC | AccessFlags::FINAL | AccessFlags::SYNTHETIC,
            interfaces: vec![
                self.resolve_class_internal("java.lang.Cloneable")?,
                self.resolve_class_internal("java.io.Serializable")?,
            ],
            fields: default(),
            methods: super_class.methods.clone(),
            static_storage: FieldStorage::EMPTY,
            object_size: 0,
            init: ClassState::Unverified.into(),
            init_waiter: default(),
        };
        let array = unsafe { self.heap.alloc_typed(array) };
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
        self.resolve_class_impl(name, Vec::new())
    }

    /// This is only to be called by `resolve_class` and errors out on loops in the class hierarchy
    fn resolve_class_impl(
        &self,
        name: &str,
        mut check_circular: Vec<String>,
    ) -> JvmResult<ClassRef> {
        if let Some(class) = self.classes.read().unwrap().get(name) {
            return Ok(*class);
        }

        // Do we want to load an array (normal name)?
        if let Some(component) = name.strip_suffix("[]") {
            return self.resolve_array_of(&component.into());
        }

        if check_circular.iter().any(|c| c == name) {
            return Err(error(self, "ClassCircularityError", name));
        }
        check_circular.push(name.to_owned());

        // We want to load a normal class
        let bytes = self
            .bootstrap_class_loader
            .load(name)
            .ok_or_else(|| error(self, "NoClassDefFoundError", name))?;

        let mut class_desc = parse::read_class_file(&bytes, self)?;

        let mut methods = HashMap::default();
        for (nat, method) in class_desc.methods.drain() {
            let method = &*self.method_storage.lock().unwrap().alloc(method);
            methods.insert(nat, method);
        }

        if name != class_desc.name.as_ref() {
            return Err(error(
                self,
                "NoClassDefFoundError",
                &format!("{name} != {}", class_desc.name.as_ref()),
            ));
        }

        let new_methods = methods.clone();

        // Interface resolution
        let mut interfaces = Vec::new();
        for interface in class_desc.interfaces {
            let interface = self.resolve_class_impl(&interface, check_circular.clone())?;
            if !interface.is_interface() {
                return Err(error(self, "IncompatibleClassChangeError", name));
            }
            interfaces.push(interface);
        }

        // Superclasses resolution
        let super_class = if let Some(super_class_name) = class_desc.super_class {
            let super_class = self.resolve_class_impl(&super_class_name, check_circular)?;

            if super_class.element_type.is_some()
                | super_class
                    .access_flags
                    .intersects(AccessFlags::FINAL | AccessFlags::INTERFACE)
            {
                return Err(error(self, "IncompatibleClassChangeError", name));
            }

            // Method resolution
            for (nat, method) in &super_class.methods {
                match methods.entry(nat.clone()) {
                    hashbrown::hash_map::Entry::Occupied(_) => {
                        if method.access_flags.contains(AccessFlags::FINAL) {
                            return Err(error(self, "IncompatibleClassChangeError", name));
                        }
                    }
                    hashbrown::hash_map::Entry::Vacant(entry) => {
                        entry.insert(*method);
                    }
                }
            }
            Some(super_class)
        } else {
            None
        };

        // Interfaces must directly inherit from object (5.3.5)
        if class_desc.access_flags.contains(AccessFlags::INTERFACE)
            & (super_class.map(|c| &*c.name) != Some("java.lang.Object"))
        {
            return Err(error(self, "IncompatibleClassChangeError", name));
        }

        let (fields, static_size, object_size) = Class::set_layouts(super_class, class_desc.fields);
        let static_storage = if static_size == 0 {
            FieldStorage::EMPTY
        } else {
            FieldStorage(
                self.heap
                    .alloc(Layout::from_size_align(static_size, 8).unwrap()),
            )
        };

        let ref_type = unsafe {
            self.heap.alloc_typed(Class {
                name: class_desc.name,
                super_class,
                element_type: None,
                const_pool: RwLock::new(class_desc.const_pool),
                access_flags: class_desc.access_flags,
                interfaces,
                fields,
                methods,
                static_storage,
                object_size,
                init: ClassState::Unverified.into(),
                init_waiter: default(),
            })
        };

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
                .store(*class as *const Class as *mut Class, Ordering::SeqCst);
        }
        for field in class.fields.values() {
            field
                .class
                .store(*class as *const Class as *mut Class, Ordering::SeqCst);
        }

        Ok(ref_type)
    }

    /// Creates an instance of a non-array class on the heap.
    ///
    /// # Panics
    /// Panics if class is an array class or interface.
    #[track_caller]
    pub(crate) fn create_object_uninitialized(&self, class: ClassRef) -> Object {
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
            self.create_object_uninitialized(class_ptr.as_ref()),
        );
        args.insert(0, crate::Value::Ref(Some(object)));
        let constructor = class_ptr
            .method(self, "<init>", arg_types, None)
            .ok_or_else(|| {
                crate::Object::new(
                    self.id,
                    error(self, "NoSuchMethodError", &format!("{class} <init> …")),
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

/// For errors thrown by the JVM (instead of by athrow).
/// `name` is unqualified.
pub(crate) fn error(jvm: &Jvm, name: &str, message: &str) -> Object {
    let message = if message.is_empty() {
        name.to_owned()
    } else {
        format!("{name}: {message}")
    };
    let object = throwable(jvm, name);
    let field = object
        .class()
        .field("message", Type::Ref("java.lang.String".into()))
        .expect("java.lang.Throwable missing field java.lang.String message");
    field.instance_set(object, Value::Ref(Some(str_to_java(jvm, &message))));
    object
}

/// For exceptions and errors thrown by the JVM (instead of by athrow).
/// `name` is unqualified.
pub(crate) fn throwable(jvm: &Jvm, name: &str) -> Object {
    match jvm.resolve_class_internal(&format!("java.lang.{name}")) {
        Ok(class) => {
            if let Err(thrown) = class.ensure_init(jvm) {
                thrown
            } else {
                jvm.create_object_uninitialized(class)
            }
        }
        Err(thrown) => thrown,
    }
}

pub(crate) fn str_to_java(jvm: &Jvm, utf8: &str) -> Object {
    let chars: Vec<u16> = utf8.encode_utf16().collect();
    let chars_object = jvm.create_array_of(&Type::Char, chars.len() as i32);
    for (i, char) in chars.iter().enumerate() {
        chars_object.array_write_value(i as i32, Value::Int(*char as i32));
    }
    let string_class = jvm
        .resolve_class_internal("java.lang.String")
        .expect("java.lang.String missing");
    let object = jvm.create_object_uninitialized(string_class);
    let field = string_class
        .field("chars", Type::Ref("char[]".into()))
        .expect("java.lang.String missing field char[] chars");
    field.instance_set(object, Value::Ref(Some(chars_object)));
    object
}

#[allow(clippy::extra_unused_type_parameters)]
fn _assert_jvm_is_send_sync<T: Send + Sync>() {
    if false {
        _assert_jvm_is_send_sync::<Jvm>()
    }
}
