use alloc::sync::Arc;

use crate::*;

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

    /// Name of this field. There may be multiple fields of the same name
    /// but differing types on the same class.
    pub fn name(&self) -> &str {
        &self.value.nat.name
    }

    /// Type of this field.
    pub fn typ(&self) -> &Type {
        &self.value.nat.typ
    }

    /// Access flags, such as whether this field is static.
    pub fn access_flags(&self) -> AccessFlags {
        self.value.access_flags
    }

    /// Java field signature. See JVMS §4.7.9.1 for details.
    pub fn signature(&self) -> Option<Arc<str>> {
        self.value.signature.clone()
    }

    /// Whether this field is marked as deprecated (does not affect semantics).
    pub fn deprecated(&self) -> bool {
        self.value.deprecated
    }

    /// Attributes not recognized by this Jvm.
    pub fn extra_attributes(&self) -> &HashMap<String, Box<[u8]>> {
        &self.value.extra_attributes
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
    /// Panics if
    /// - the field is not static
    /// - the field is final
    /// - the value is not assignable to the field's type
    /// - or is an object created by a different JVM
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
    /// Panics if
    /// - the field is static
    /// - the field is final
    /// - the object is not an instance of the class corresponding to this field
    /// - the value is not assignable to the field's type
    /// - or the value is an object created by a different JVM
    #[track_caller]
    pub fn instance_set(&self, object: &Object, value: impl Into<Value>) {
        self.value.instance_set(
            self.jvm.deref().heap.ptrs(),
            object.as_inner(&self.jvm),
            value.into().into_inner(&self.jvm),
        )
    }
}
