use std::sync::{
    atomic::{AtomicPtr, Ordering},
    Arc,
};

use crate::{
    class::Class, field_storage::FieldStorage, heap::NULL_PTR, jvm::Value, object::Object,
    typ::Typ, AccessFlags,
};

pub(crate) struct Field {
    /// Fields within a class are uniquely identified by name and type – multiple fields of the same name may exist
    pub(crate) nat: FieldNaT,
    /// Class is behind a AtomicCell to enable circular references
    pub(crate) class: AtomicPtr<Class>,
    pub(crate) access_flags: AccessFlags,
    /// Offset into the FieldData of the class (static field) / instance (non-static) where this is stored
    /// Java has no multiple inheritance for fields, therefore each field can be at a set position
    pub(crate) byte_offset: usize,
    /// Only set for static fields
    pub(crate) const_value_index: Option<u16>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FieldNaT {
    pub name: Arc<str>,
    pub typ: Typ,
}

impl Field {
    pub(crate) fn class(&self) -> &'static Class {
        // SAFETY: This reference will not outlive the JVM
        // and this function will only be called after class has been set
        unsafe { &*self.class.load(Ordering::SeqCst) }
    }

    /// Get the value of a static field.
    /// Note that this might not be initialized yet unless a method has been called on this class.
    /// # Panics
    /// Panics if the field is not static.
    pub fn static_get(&self) -> Value {
        if !self.access_flags.contains(AccessFlags::STATIC) {
            panic!("field {} is not static", self.nat.name)
        }
        self.read(&self.class().static_storage)
    }

    /// Get the value of an object's field.
    /// # Panics
    /// Panics if the field is static or the object is not an instance of the class corresponding to this field.
    pub fn instance_get(&self, object: Object) -> Value {
        if self.access_flags.contains(AccessFlags::STATIC) {
            panic!("field {} is static", self.nat.name)
        }
        if !object.class().assignable_to(&self.class().name) {
            panic!(
                "object of class {} not instance of {}",
                object.class().name,
                self.class().name
            )
        }
        self.read(&object.ptr)
    }

    fn read(&self, storage: &FieldStorage) -> Value {
        let volatile = self.access_flags.contains(AccessFlags::VOLATILE);
        // SAFETY: bounds, alignment & datatype unchanged after construction
        unsafe {
            match self.nat.typ {
                Typ::Boolean | Typ::Byte => {
                    Value::Int(storage.read_i8(self.byte_offset, volatile) as i32)
                }
                Typ::Short | Typ::Char => {
                    Value::Int(storage.read_i16(self.byte_offset, volatile) as i32)
                }
                Typ::Int => storage.read_i32(self.byte_offset, volatile).into(),
                Typ::Float => storage.read_f32(self.byte_offset, volatile).into(),
                Typ::Long => storage.read_i64(self.byte_offset, volatile).into(),
                Typ::Double => storage.read_f64(self.byte_offset, volatile).into(),
                Typ::Ref(..) => {
                    Object::from_ptr(storage.read_ptr(self.byte_offset, volatile)).into()
                }
            }
        }
    }

    /// Set the value of a static field.
    /// # Panics
    /// Panics if the field is not static or the value is not assignable to the field's type
    pub fn static_set(&self, value: Value) {
        if !self.access_flags.contains(AccessFlags::STATIC) {
            panic!("field {} is not static", self.nat.name)
        }
        self.write(&self.class().static_storage, value)
    }

    /// Get the value of an object's field.
    /// # Panics
    /// Panics if the field is static,
    /// the object is not an instance of the class corresponding to this field
    /// or the value is not assignable to the field's type
    pub fn instance_set(&self, object: Object, value: Value) {
        if self.access_flags.contains(AccessFlags::STATIC) {
            panic!("field {} is static", self.nat.name)
        }
        if !object.class().assignable_to(&self.class().name) {
            panic!(
                "object of class {} not instance of {}",
                object.class().name,
                self.class().name
            )
        }
        self.write(&object.ptr, value)
    }

    fn write(&self, storage: &FieldStorage, value: Value) {
        let volatile = self.access_flags.contains(AccessFlags::VOLATILE);

        // This is to avoid the need to take a ref to JVM for resolution
        fn assignable(class: &Class, to: &str) -> bool {
            to == class.name.as_ref() || class.super_class.map_or(false, |s| assignable(s, to))
        }

        // SAFETY: bounds, alignment & datatype unchanged after construction
        unsafe {
            match (&self.nat.typ, value) {
                (Typ::Boolean | Typ::Byte, Value::Int(value)) => {
                    storage.write_i8(self.byte_offset, value as i8, volatile)
                }
                (Typ::Short | Typ::Char, Value::Int(value)) => {
                    storage.write_i16(self.byte_offset, value as i16, volatile)
                }
                (Typ::Int, Value::Int(value)) => {
                    storage.write_i32(self.byte_offset, value, volatile)
                }
                (Typ::Float, Value::Float(value)) => {
                    storage.write_f32(self.byte_offset, value, volatile)
                }
                (Typ::Long, Value::Long(value)) => {
                    storage.write_i64(self.byte_offset, value, volatile)
                }
                (Typ::Double, Value::Double(value)) => {
                    storage.write_f64(self.byte_offset, value, volatile)
                }
                (Typ::Ref(name), Value::Ref(obj))
                    if obj.map(|o| assignable(o.class(), name)).unwrap_or(true) =>
                {
                    storage.write_ptr(
                        self.byte_offset,
                        obj.map(|o| o.ptr().into()).unwrap_or(NULL_PTR),
                        volatile,
                    )
                }
                _ => panic!(
                    "type mismatch: Field has type {}, value is {:?}",
                    self.nat.typ, value
                ),
            }
        }
    }
}

impl<'a> Eq for &'a Field {}
impl<'a> PartialEq for &'a Field {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(*self, *other)
    }
}

impl<'a> std::hash::Hash for &'a Field {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::hash::Hash::hash(&(*self as *const _ as usize), state)
    }
}

impl std::fmt::Debug for FieldNaT {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.typ, self.name)
    }
}

impl std::fmt::Debug for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.nat.fmt(f)
    }
}
