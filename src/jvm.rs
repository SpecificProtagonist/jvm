use crate::heap::Heap;
use crate::{class_loader::ClassLoader, AccessFlags};
use fixed_typed_arena::ManuallyDropArena;
use parking_lot::{Mutex, RwLock};
use std::{
    // TODO: posibly exchange for different hasher (benchmark)
    collections::HashMap,
    fmt::Display,
    sync::{
        atomic::{AtomicBool, AtomicU64, Ordering},
        Arc,
    },
};

use crate::field_storage::FieldStorage;
use crate::heap;
use crate::method::*;
use crate::object;
use crate::object::Object;
use crate::parse;
use crate::typ::Typ;
use crate::{class::*, default};

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Value {
    Void,
    Ref(Option<Object>),
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
}

// TODO: what to do if trying to throw an exception retursively throws infinite exceptions
// (e.g. java.lang.Object is missing/corrupt)? Currently stackoverflow
/// Err is an object that extends Throwable
pub(crate) type JVMResult<T> = std::result::Result<T, Object>;

static NEXT_ID: AtomicU64 = AtomicU64::new(0);

pub(crate) struct Jvm {
    /// Uniquely identifies the Jvm to prevent mixing of objects between Jvms
    pub(crate) id: u64,
    /// Currently there's a single class loader per Jvm.
    pub bootstrap_class_loader: Box<dyn ClassLoader>,
    /// Bypasses verification by type checking. This is unsafe!
    pub(crate) verification_type_checking_disabled: AtomicBool,
    pub(crate) heap: Heap,
    // Classes (including arrays), methods, method descriptors and interned strings live as long as the JVM
    // TODO: alloc on heap
    pub(crate) method_storage: Mutex<ManuallyDropArena<Method, 128>>,
    /// Implementation detail of const_pool, maybe remove in the future
    pub(crate) method_descriptor_storage: Mutex<ManuallyDropArena<MethodDescriptor, 128>>,
    classes: RwLock<HashMap<Arc<str>, ClassPtr>>,
}

impl Jvm {
    /// Construct a new JVM. When searching for class definitions, the folders specified in
    /// `class_path` are searched in order.
    pub fn new(base_class_loader: Box<dyn ClassLoader>) -> Self {
        let heap = Heap::default();
        Self {
            id: NEXT_ID.fetch_add(1, Ordering::SeqCst),
            bootstrap_class_loader: base_class_loader,
            verification_type_checking_disabled: false.into(),
            heap,
            method_storage: default(),
            method_descriptor_storage: default(),
            classes: default(),
        }
    }

    /// Returns the class representing arrays of the given component.
    /// If the component is a class, this also loads it, its superclass (recursively) and any interfaces it implements.
    /// Does not initialize this class nor load any other class referenced.
    pub fn resolve_array_of(&self, component: &Typ) -> JVMResult<ClassPtr> {
        // Arrays of primitives inherit from objects,
        // other arrays inherit from arrays of the superclass of their component type
        let super_class = if let Typ::Ref(component) = &component {
            let component = self.resolve_class(component)?;
            if let Some(comp_super) = component.super_class {
                self.resolve_array_of(&comp_super.typ())?
            } else {
                self.resolve_class("java.lang.Object")?
            }
        } else {
            self.resolve_class("java.lang.Object")?
        };

        let name: Arc<str> = format!("{component}[]").into();

        let array = Class {
            name: name.clone(),
            super_class: Some(super_class),
            element_type: Some(component.clone()),
            const_pool: default(),
            // TODO: correct access flags
            access_flags: AccessFlags::empty(),
            interfaces: vec![],
            // TODO: implement interfaces
            /*vec![
                self.resolve_class("Clonable")?,
                self.resolve_class("java.io.Serializable")?,
            ]*/
            fields: default(),
            methods: super_class.methods.clone(),
            static_storage: FieldStorage::new(&self.heap, 0),
            object_size: 0,
            init: ClassInitState::Uninit.into(),
            init_waiter: default(),
        };
        let array = unsafe { self.heap.alloc_typed(array) };
        let mut guard = self.classes.write();
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
    /// `name` must be fully qualified and may be a binary class name (https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.2.1) or describe an array.
    pub fn resolve_class(&self, name: &str) -> JVMResult<ClassPtr> {
        self.resolve_class_impl(name, Vec::new())
    }

    /// This is only to be called by `resolve_class` and errors out on loops in the class hierarchy
    fn resolve_class_impl(
        &self,
        name: &str,
        mut check_circular: Vec<Arc<str>>,
    ) -> JVMResult<ClassPtr> {
        if let Some(class) = self.classes.read().get(name) {
            return Ok(*class);
        }
        // Do we want to load an array (binary class name)?
        if name.starts_with('[') {
            let (component, end) = parse::parse_typ_descriptor(self, name, 1)?;
            if end != name.len() {
                return Err(exception(self, "ClassFormatError"));
            }
            return self.resolve_array_of(&component);
        }

        // Do we want to load an array (normal name)?
        if let Some(component) = name.strip_suffix("[]") {
            return self.resolve_array_of(&component.into());
        }

        // Normalize binary class names
        let name = name.replace('/', ".");

        // We want to load a normal class
        let bytes = self
            .bootstrap_class_loader
            .load(&name)
            .ok_or_else(|| exception(self, "NoClassDefFoundError"))?;

        let mut class_desc = parse::read_class_file(&bytes, self)?;

        if name != class_desc.name.as_ref() {
            return Err(exception(self, "NoClassDefFoundError"));
        }

        let new_methods = class_desc.methods.clone();

        // Superclasses loading & verification
        let super_class = if let Some(super_class_name) = class_desc.super_class {
            if check_circular.contains(&super_class_name) {
                return Err(exception(self, "ClassCircularityError"));
            }
            check_circular.push(super_class_name.clone());
            let super_class = self.resolve_class_impl(&super_class_name, check_circular)?;

            if super_class.element_type.is_some()
                | super_class
                    .access_flags
                    .intersects(AccessFlags::FINAL | AccessFlags::INTERACE)
            {
                return Err(exception(self, "IncompatibleClassChangeError"));
            }

            // Method resolution
            for (nat, method) in &super_class.methods {
                class_desc.methods.entry(nat.clone()).or_insert(*method);
            }
            Some(super_class)
        } else {
            None
        };

        // Interfaces must directly inherit from object (5.3.5)
        if class_desc.access_flags.contains(AccessFlags::INTERACE)
            && match &super_class {
                None => true,
                Some(s) => *s.name != *"java.lang.Object",
            }
        {
            return Err(exception(self, "IncompatibleClassChangeError"));
        }

        let (fields, static_size, object_size) = Class::set_layouts(super_class, class_desc.fields);
        let static_storage = FieldStorage::new(&self.heap, static_size);

        let interfaces = class_desc.interfaces.into_iter().map(|_| todo!()).collect();

        let ref_type = unsafe {
            self.heap.alloc_typed(Class {
                name: class_desc.name,
                super_class,
                element_type: None,
                const_pool: RwLock::new(class_desc.const_pool),
                access_flags: class_desc.access_flags,
                interfaces,
                fields,
                methods: class_desc.methods,
                static_storage,
                object_size,
                init: ClassInitState::Uninit.into(),
                init_waiter: default(),
            })
        };

        let mut guard = self.classes.write();
        // Check if loaded by another thread in the meantime
        if let Some(class) = guard.get(name.as_str()) {
            return Ok(*class);
        }
        guard.insert(name.as_str().into(), ref_type);
        let class = guard.get(name.as_str()).unwrap();

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
    /// # Panics
    /// Panics if class is an array class
    #[track_caller]
    pub fn create_object(&self, class: ClassPtr) -> Object {
        if class.element_type.is_some() {
            panic!("{} is an array type", class.name)
        }
        let ptr = FieldStorage::new(&self.heap, class.object_size);
        // SAFETY: object_size takes object head into account
        unsafe { ptr.write_ptr(0, heap::ptr_encode(class as *const Class as *mut u8), true) };
        Object { ptr }
    }

    /// Creates an array on the heap.
    /// # Panics
    /// Panics if length < 0 or class is not an array class.
    #[track_caller]
    pub fn create_array(&self, class: ClassPtr, length: i32) -> Object {
        self.create_array_impl(
            class,
            class
                .element_type
                .as_ref()
                .expect("Class is not an array class"),
            length,
        )
    }

    /// Creates an bdcvbmn, n vdcn bmf,.vn dc.,m-nön-nnnon the heap.
    /// # Panics
    /// Panics if length < 0 or the component is a class that fails to load.
    #[track_caller]
    pub fn create_array_of(&self, component: &Typ, length: i32) -> Object {
        let class = self
            .resolve_array_of(component)
            .expect("Failed to resolve class");
        self.create_array_impl(class, component, length)
    }

    #[track_caller]
    fn create_array_impl(&self, class: ClassPtr, component: &Typ, length: i32) -> Object {
        if length < 0 {
            panic!("array length < 0")
        }
        let data = FieldStorage::new(
            &self.heap,
            object::header_size() + component.layout().size() * length as usize,
        );
        // SAFETY: object_size takes object head into account
        unsafe { data.write_ptr(0, heap::ptr_encode(class as *const Class as *mut u8), true) };
        Object { ptr: data }
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
            ManuallyDropArena::drop(&mut self.method_storage.lock());
            ManuallyDropArena::drop(&mut self.method_descriptor_storage.lock());
        }
    }
}

/// For exceptions and errors thrown by the JVM (instead of by athrow).
/// `name` is unqualified.
pub(crate) fn exception(jvm: &Jvm, name: &str) -> Object {
    match jvm.resolve_class(&format!("java.lang.{name}")) {
        Ok(class) => {
            if let Err(thrown) = class.ensure_init(jvm) {
                thrown
            } else {
                jvm.create_object(class)
            }
        }
        Err(thrown) => thrown,
    }
}

#[allow(clippy::extra_unused_type_parameters)]
fn _assert_jvm_is_send_sync<T: Send + Sync>() {
    if false {
        _assert_jvm_is_send_sync::<Jvm>()
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Void => write!(f, "void"),
            Self::Ref(None) => write!(f, "null"),
            Self::Ref(Some(obj)) => write!(f, "@{:x}", obj.ptr()),
            Self::Int(v) => write!(f, "{}", v),
            Self::Long(v) => write!(f, "{}", v),
            Self::Float(v) => write!(f, "{}", v),
            Self::Double(v) => write!(f, "{}", v),
        }
    }
}

impl From<Option<Object>> for Value {
    fn from(val: Option<Object>) -> Self {
        Self::Ref(val)
    }
}

impl From<Object> for Value {
    fn from(val: Object) -> Self {
        Self::Ref(Some(val))
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
