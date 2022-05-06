#![feature(hash_set_entry)]
#![feature(map_first_last)]

use anyhow::{anyhow, bail, Context, Result};
use fixed_typed_arena::ManuallyDropArena;
use heap::Heap;
use object::Object;
use parking_lot::{Mutex, RwLock};
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    fs::File,
    hash::Hash,
    io::Read,
    path::PathBuf,
};

mod class;
mod const_pool;
mod field_storage;
mod heap;
mod instructions;
pub mod interp;
mod object;
mod parse;
mod string_interning;
mod typ;
mod verification;

pub use class::*;
use field_storage::FieldStorage;
pub use string_interning::*;
pub use typ::Typ;

// TODO: Garbage collection for objects

pub struct JVM<'a> {
    /// TODO: provide ClassLoader trait instead
    class_path: Vec<PathBuf>,
    heap: Heap<'a>,
    // Classes (including arrays), methods, method descriptors and interned strings live as long as the JVM
    string_storage: Mutex<ManuallyDropArena<String, 256>>,
    method_storage: Mutex<ManuallyDropArena<Method<'a>, 128>>,
    /// Implementation detail of const_pool, maybe remove in the future
    method_descriptor_storage: Mutex<ManuallyDropArena<MethodDescriptor<'a>, 128>>,
    // TODO: intern string objects instead
    strings: RwLock<HashSet<&'a str>>,
    classes: RwLock<HashMap<IntStr<'a>, &'a Class<'a>>>,
    /// Placeholder for Method.class/Field.class. Somehow removing this would be nice.
    /// I had a static Class in parse.rs, but apparently I can't use a &'a Class<'static> as a &'a Class<'a>
    dummy_class: &'a Class<'a>,
}

impl<'a> JVM<'a> {
    /// Construct a new JVM. When searching for class definitions, the folders specified in
    /// `class_path` are searched in order.
    pub fn new(class_path: Vec<PathBuf>) -> Self {
        //let class_storage = Mutex::<ManuallyDropArena<_, 128>>::default();
        let heap = Heap::default();
        let dummy_class = heap.alloc_typed(Class::dummy_class(&heap));
        Self {
            class_path,
            heap,
            string_storage: Default::default(),
            //class_storage,
            method_storage: Default::default(),
            method_descriptor_storage: Default::default(),
            strings: Default::default(),
            classes: Default::default(),
            dummy_class,
        }
    }

    pub fn intern_str<'b>(&'b self, str: impl Into<Cow<'b, str>>) -> IntStr<'a> {
        let str: Cow<_> = str.into();
        let guard = self.strings.read();
        IntStr(if let Some(str) = guard.get(str.as_ref()) {
            *str
        } else {
            drop(guard);
            let mut guard = self.strings.write();
            guard.get_or_insert(self.string_storage.lock().alloc(str.into_owned()))
        })
    }

    /// Returns the class, loading it if it wasn't already loaded.
    /// This also loads its superclass and any interfaces it implements, but not any other class it refers to
    /// Does not initialize this class, nor load any other class referenced.
    pub fn resolve_class<'b>(
        &'b self,
        name: impl Into<MaybeInteredString<'a, 'b>>,
    ) -> Result<&'a Class<'a>> {
        self.resolve_class_impl(name.into().get(self), Vec::new())
    }

    /// This is only to be called by `resolve_class` and errors out on loops in the class hierarchy
    fn resolve_class_impl<'b>(
        &'b self,
        name: IntStr<'a>,
        mut check_circular: Vec<IntStr<'a>>,
    ) -> Result<&'a Class<'a>> {
        if let Some(class) = self.classes.read().get(&name) {
            return Ok(*class);
        }

        // Do we want to load an array?
        if name.0.starts_with('[') {
            let (base, end) = parse::parse_field_descriptor(self, name, 1)?;
            if end != name.0.len() {
                bail!("Invalid array type descriptor: {}", name)
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
        // TODO: move loading the bytes into a Classloader trait
        let mut bytes = None;
        for path in &self.class_path {
            let mut path = path.clone();
            path.push(name.0);
            path.set_extension("class");

            if path.exists() {
                let mut file = Vec::new();
                File::open(path)
                    .with_context(|| format!("Failed to load class {}", name))?
                    .read_to_end(&mut file)?;
                bytes = Some(file);
                break;
            }
        }
        let bytes = bytes.ok_or_else(|| anyhow!("No class def found for {}", name))?;

        let mut class_desc = parse::read_class_file(&bytes, self)
            .with_context(|| format!("Failed to parse class {}", name))?;

        if name != class_desc.name {
            bail!("Class name did not match requested name")
        }

        let new_methods = class_desc.methods.clone();

        // Superclasses loading & verification
        let super_class = if let Some(super_class_name) = class_desc.super_class {
            if check_circular.contains(&super_class_name) {
                bail!("Circular class inheritance for {}", super_class_name);
            }
            check_circular.push(super_class_name);
            let super_class = self.resolve_class_impl(super_class_name, check_circular)?;

            if super_class.element_type.is_some() {
                bail!("Can't subclass array!")
            }

            if super_class.access_flags.contains(AccessFlags::FINAL) {
                bail!("Tried to subclass final class {}", super_class.name)
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
    ) -> Result<&'a Method<'a>> {
        self.resolve_class(class)?
            .methods
            .get(&MethodNaT {
                name: method.into().get(self),
                typ: &MethodDescriptor(args, returns),
            })
            .ok_or_else(|| anyhow!("Method not found"))
            .map(|m| *m)
    }

    /// Creates an instance of a non-array class on the heap
    fn create_object(&self, class: &'a Class) -> Object<'a> {
        if class.element_type.is_some() {
            panic!("Called create_object with an array")
        }
        let data = FieldStorage::new(&self.heap, class.object_size);
        data.write_ptr(0, heap::ptr_encode(class as *const Class as *mut u8));
        Object {
            ptr: data,
            _marker: std::marker::PhantomData,
        }
    }

    /// Creates an instance of an array class on the heap
    fn create_array(&self, class: &'a Class, length: usize) -> Object<'a> {
        let component = class
            .element_type
            .expect("Called create_array with a non-array class");
        let data = FieldStorage::new(
            &self.heap,
            object::header_size() + component.layout().size() * length,
        );
        data.write_ptr(0, heap::ptr_encode(class as *const Class as *mut u8));
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

bitflags::bitflags! {
    pub struct AccessFlags: u16 {
        const STATIC = 0x0008;
        const FINAL = 0x0010;
        const SUPER = 0x0020;
        const NATIVE = 0x0100;
        const ABSTRACT = 0x0400;
    }
}

fn _assert_jvm_send_sync<T: Send + Sync>() {
    if false {
        _assert_jvm_send_sync::<JVM>()
    }
}
