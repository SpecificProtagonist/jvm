use core::alloc::Layout;
use core::ptr::NonNull;
use core::sync::atomic::Ordering::*;

use crate::hot_reloading::{ReloadErr, reload_method_bytecodes_for_class};
use crate::method::MethodNaT;
use crate::*;

/// Reference counted (cheaply cloned) handle to a Java Virtual Machine.
/// Kept alive while handles to its Classes/Objects/etc exist.
///
/// Multiple independent `Jvm`s may exist.
///
/// Mutable shared access follows Java's rules.
///
/// To operate correctly, the JVM requires a class library. The minimal
/// version necessary to implement this is available under the `classes`
/// folder of this repository. These aren't loaded automatically, your
/// class library needs to be provided by the class loader. Those classes missing
/// or being invalid classes may cause the thread to hang or crash.
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
        let offset = self.deref().class_class_ptr_offset.load(Acquire);
        let ptr: usize =
            class
                .as_inner(self)
                .fields()
                .read(self.deref().heap.ptrs(), offset, false);
        Class::new(self.deref(), unsafe {
            (ptr as *const class::Class).as_ref_unchecked()
        })
    }

    /// # Panics
    /// Panics if the slice is longer than the maximum length of a Java array (`i32::MAX`).
    pub fn create_array_from_primitive_slice<T: Primitive>(&self, data: &[T]) -> Object {
        assert!(data.len() <= i32::MAX as usize);
        let array = self.create_array(&T::TYPE, data.len() as i32);
        for (index, &value) in data.iter().enumerate() {
            array
                .value
                .array_write(self.deref(), index as i32, value)
                .unwrap();
        }
        array
    }
}
