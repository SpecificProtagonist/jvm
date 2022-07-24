#![feature(hash_set_entry)]
#![feature(map_first_last)]

use class_loader::ClassLoader;
use fixed_typed_arena::ManuallyDropArena;
use heap::Heap;
use parking_lot::{Mutex, RwLock};
use std::{
    borrow::Cow,
    // TODO: posibly exchange for different hasher (benchmark)
    collections::HashMap,
    fmt::Display,
    hash::Hash,
    sync::atomic::{AtomicBool, Ordering},
};

mod class;
mod class_loader;
mod const_pool;
mod field;
mod field_storage;
mod heap;
mod instructions;
mod interp;
mod method;
mod object;
mod parse;
mod string_interning;
mod typ;
mod verification;

pub use class::*;
pub use class_loader::*;
pub use field::*;
use field_storage::FieldStorage;
pub use method::*;
pub use object::Object;
pub use string_interning::*;
pub use typ::Typ;

// TODO: what to do if trying to throw an exception retursively throws infinite exceptions
// (e.g. java/lang/Object is missing/corrupt)? Currently stackoverflow
/// Err is an object that extends Throwable
pub type JVMResult<'a, T> = std::result::Result<T, Object<'a>>;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum JVMValue<'a> {
    Ref(Option<Object<'a>>),
    /// Represents boolean, byte, char, short and int
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
}

impl<'a> Display for JVMValue<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ref(None) => write!(f, "null"),
            Self::Ref(Some(obj)) => write!(f, "@{:x}", obj.ptr()),
            Self::Int(v) => write!(f, "{}", v),
            Self::Long(v) => write!(f, "{}", v),
            Self::Float(v) => write!(f, "{}", v),
            Self::Double(v) => write!(f, "{}", v),
        }
    }
}

// TODO: branded lifetimes/what to do to prevent passing refs to objects/classes/… between JVMs with matching lifetimes?

pub struct JVM<'a> {
    pub bootstrap_class_loader: Box<dyn ClassLoader>,
    /// Bypasses verification by type checking. This is unsafe!
    verification_type_checking_disabled: AtomicBool,
    heap: Heap<'a>,
    // Classes (including arrays), methods, method descriptors and interned strings live as long as the JVM
    string_storage: Mutex<ManuallyDropArena<String, 256>>,
    method_storage: Mutex<ManuallyDropArena<Method<'a>, 128>>,
    /// Implementation detail of const_pool, maybe remove in the future
    method_descriptor_storage: Mutex<ManuallyDropArena<MethodDescriptor<'a>, 128>>,
    /// Interned strings along with the corrosponding object,
    /// if String.intern has been called or CONST_String was in const pool (null otherwise)
    // TMP: OPTION
    strings: RwLock<HashMap<&'a str, Option<Object<'a>>>>,
    classes: RwLock<HashMap<IntStr<'a>, &'a Class<'a>>>,
    /// Placeholder for Method.class/Field.class. Somehow removing this would be nice.
    /// I had a static Class in parse.rs, but apparently I can't use a &'a Class<'static> as a &'a Class<'a>
    dummy_class: &'a Class<'a>,
}

impl<'a> JVM<'a> {
    /// Construct a new JVM. When searching for class definitions, the folders specified in
    /// `class_path` are searched in order.
    pub fn new(base_class_loader: Box<dyn ClassLoader>) -> Self {
        let heap = Heap::default();
        let dummy_class = heap.alloc_typed(Class::dummy_class(&heap));
        Self {
            bootstrap_class_loader: base_class_loader,
            verification_type_checking_disabled: false.into(),
            heap,
            string_storage: Default::default(),
            method_storage: Default::default(),
            method_descriptor_storage: Default::default(),
            strings: Default::default(),
            classes: Default::default(),
            dummy_class,
        }
    }

    /// Bypasses verification by type checking.
    /// # Safety
    /// Undefined behavior if any invalid class gets loaded
    pub unsafe fn disable_verification_by_type_checking(&self) {
        self.verification_type_checking_disabled
            .store(true, Ordering::SeqCst);
    }

    pub fn enable_verification_by_type_checking(&self) {
        self.verification_type_checking_disabled
            .store(false, Ordering::SeqCst)
    }

    pub fn intern_str<'b>(&'b self, str: impl Into<Cow<'b, str>>) -> IntStr<'a> {
        let str: Cow<_> = str.into();
        let guard = self.strings.read();
        IntStr(if let Some((str, _)) = guard.get_key_value(str.as_ref()) {
            str
        } else {
            drop(guard);
            let mut guard = self.strings.write();
            let entry = guard.entry(self.string_storage.lock().alloc(str.into_owned()));
            // Read back key in case another thread inserted the same string
            // between read guard release and write guard aquire
            let key = *entry.key();
            entry.or_default();
            key
        })
    }

    /// Returns the class, loading it if it wasn't already loaded.
    /// This also loads its superclass and any interfaces it implements, but not any other class it refers to
    /// Does not initialize this class, nor load any other class referenced.
    pub fn resolve_class<'b>(
        &'b self,
        name: impl Into<MaybeInteredString<'a, 'b>>,
    ) -> JVMResult<'a, &'a Class<'a>> {
        self.resolve_class_impl(name.into().get(self), Vec::new())
    }

    /// This is only to be called by `resolve_class` and errors out on loops in the class hierarchy
    fn resolve_class_impl<'b>(
        &'b self,
        name: IntStr<'a>,
        mut check_circular: Vec<IntStr<'a>>,
    ) -> JVMResult<'a, &'a Class<'a>> {
        if let Some(class) = self.classes.read().get(&name) {
            return Ok(*class);
        }

        // Do we want to load an array?
        if name.0.starts_with('[') {
            let (base, end) = parse::parse_field_descriptor(self, name, 1)?;
            if end != name.0.len() {
                return Err(exception(self, "ClassFormatError"));
            }

            // Arrays of primitives inherit from objects,
            // other arrays inherit from arrays of the superclass of their component type
            let super_class = if let Typ::Ref(component) = base {
                let component = self.resolve_class(component)?;
                if let Some(comp_super) = component.super_class {
                    self.resolve_class(format!("[{}", comp_super.name))?
                } else {
                    self.resolve_class("java/lang/Object")?
                }
            } else {
                self.resolve_class("java/lang/Object")?
            };

            //let array = RefType::Array { base, super_class };
            let array = Class {
                name,
                super_class: Some(super_class),
                element_type: Some(base),
                const_pool: Default::default(),
                // TODO: correct access flags
                access_flags: AccessFlags::empty(),
                interfaces: vec![],
                // TODO: implement interfaces
                /*vec![
                    self.resolve_class("Clonable")?,
                    self.resolve_class("java/io/Serializable")?,
                ]*/
                fields: Default::default(),
                methods: super_class.methods.clone(),
                static_storage: FieldStorage::new(&self.heap, 0),
                object_size: 0,
                init: ClassInitState::Uninit.into(),
                init_waiter: Default::default(),
            };
            let array = self.heap.alloc_typed(array);
            let mut guard = self.classes.write();
            // Check if loaded by another thread in the meantime
            if let Some(array) = guard.get(&name) {
                return Ok(*array);
            }
            guard.insert(name, array);
            return Ok(array);
        }

        // We want to load a normal class
        let bytes = self
            .bootstrap_class_loader
            .load(name)
            .ok_or_else(|| exception(self, "NoClassDefFoundError"))?;

        let mut class_desc = parse::read_class_file(&bytes, self)?;

        if name != class_desc.name {
            return Err(exception(self, "NoClassDefFoundError"));
        }

        let new_methods = class_desc.methods.clone();

        // Superclasses loading & verification
        let super_class = if let Some(super_class_name) = class_desc.super_class {
            if check_circular.contains(&super_class_name) {
                return Err(exception(self, "ClassCircularityError"));
            }
            check_circular.push(super_class_name);
            let super_class = self.resolve_class_impl(super_class_name, check_circular)?;

            if super_class.element_type.is_some()
                | super_class.access_flags.contains(AccessFlags::FINAL)
            {
                return Err(exception(self, "IncompatibleClassChangeError"));
            }

            // Method resolution
            for (nat, method) in &super_class.methods {
                class_desc.methods.entry(*nat).or_insert(*method);
            }
            Some(super_class)
        } else {
            None
        };

        let interfaces = class_desc.interfaces.into_iter().map(|_| todo!()).collect();

        let ref_type = self.heap.alloc_typed(Class {
            name: class_desc.name,
            super_class,
            element_type: None,
            const_pool: RwLock::new(class_desc.const_pool),
            access_flags: class_desc.access_flags,
            interfaces,
            fields: class_desc.fields,
            methods: class_desc.methods,
            static_storage: class_desc.static_storage,
            object_size: class_desc.object_size,
            init: ClassInitState::Uninit.into(),
            init_waiter: Default::default(),
        });

        let mut guard = self.classes.write();
        // Check if loaded by another thread in the meantime
        if let Some(class) = guard.get(&name) {
            return Ok(*class);
        }
        guard.insert(name, ref_type);
        let class = guard.get(&name).unwrap();

        // While parsing, each method has their class field set to a dummy
        // We can't use class.methods directly because that also includes parent classes
        for method in new_methods.values() {
            method.class.store(class);
        }
        for field in class.fields.values() {
            field.class.store(class);
        }

        Ok(ref_type)
    }

    /// Convenience function; this can also be achieved with resolve_class().method()
    pub fn resolve_method<'b>(
        &'b self,
        class: impl Into<MaybeInteredString<'a, 'b>>,
        method: impl Into<MaybeInteredString<'a, 'b>>,
        args: Vec<Typ<'a>>,
        returns: Option<Typ<'a>>,
    ) -> JVMResult<'a, &'a Method<'a>> {
        self.resolve_class(class)?
            .methods
            .get(&MethodNaT {
                name: method.into().get(self),
                typ: &MethodDescriptor(args, returns),
            })
            .ok_or_else(|| exception(self, "NoSuchMethodError"))
            .map(|m| *m)
    }

    // TODO: publicly accessible dynamic method resolution
    /// Calls a method, initializing its class if necessary.
    /// Returns either the function's return value or the exception that was thrown
    pub fn invoke(
        &self,
        method: &'a Method<'a>,
        args: &[JVMValue<'a>],
    ) -> JVMResult<'a, Option<JVMValue<'a>>> {
        interp::invoke(self, method, args)
    }

    // Move this over to class?
    /// Creates an instance of a non-array class on the heap
    /// # Panics
    /// Panics if class is an array class
    pub fn create_object(&self, class: &'a Class) -> Object<'a> {
        if class.element_type.is_some() {
            panic!("{} is an array type", class.name)
        }
        let data = FieldStorage::new(&self.heap, class.object_size);
        // SAFETY: object_size takes object head into account
        unsafe { data.write_ptr(0, heap::ptr_encode(class as *const Class as *mut u8), true) };
        Object {
            ptr: data,
            _marker: std::marker::PhantomData,
        }
    }

    /// Creates an instance of an array class on the heap
    /// # Panics
    /// Panics if class is not an array class or length < 0
    pub fn create_array(&self, class: &'a Class, length: i32) -> Object<'a> {
        let component = class.element_type.expect("non-array class");
        if length < 0 {
            panic!("array length < 0")
        }
        let data = FieldStorage::new(
            &self.heap,
            object::header_size() + component.layout().size() * length as usize,
        );
        // SAFETY: object_size takes object head into account
        unsafe { data.write_ptr(0, heap::ptr_encode(class as *const Class as *mut u8), true) };
        Object {
            ptr: data,
            _marker: std::marker::PhantomData,
        }
    }
}

impl<'a> Drop for JVM<'a> {
    fn drop(&mut self) {
        // SAFETY: All references have lifetime 'a and no member of JVM has a drop impl that dereferences them
        unsafe {
            ManuallyDropArena::drop(&mut self.string_storage.lock());
            ManuallyDropArena::drop(&mut self.method_storage.lock());
            ManuallyDropArena::drop(&mut self.method_descriptor_storage.lock());
        }
    }
}

/// For exceptions and errors thrown by the JVM (instead of by athrow).
/// `name` doesn't include `java/lang` prefix
fn exception<'b, 'a: 'b>(jvm: &'b JVM<'a>, name: &'static str) -> Object<'a> {
    match jvm.resolve_class(format!("java/lang/{name}")) {
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

fn _assert_jvm_send_sync<T: Send + Sync>() {
    if false {
        _assert_jvm_send_sync::<JVM>()
    }
}
