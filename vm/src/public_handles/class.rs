use alloc::sync::Arc;

use crate::*;

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

    /// Java class signature. See JVMS §4.7.9.1 for details.
    pub fn signature(&self) -> Option<Arc<str>> {
        self.value.signature.clone()
    }

    /// Whether this class is marked as deprecated (does not affect semantics).
    pub fn deprecated(&self) -> bool {
        self.value.deprecated
    }

    /// Attributes not recognized by this Jvm.
    pub fn extra_attributes(&self) -> &HashMap<String, Box<[u8]>> {
        &self.value.extra_attributes
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
        args.insert(0, Value::Object(Some(object.clone())));
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
