use alloc::sync::Arc;
use core::sync::atomic::{AtomicPtr, Ordering};
use std::collections::HashMap;

use hashbrown::Equivalent;

use crate::{
    AccessFlags,
    annotations::Annotation,
    class::{Class, ClassRef},
    heap::Ptrs,
    object::{FieldStorage, Object},
    typ::Type,
    value::Value,
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
    /// Index into the const pool.
    /// Only set for static fields.
    pub(crate) const_value_index: Option<u16>,
    pub(crate) signature: Option<Arc<str>>,
    pub(crate) deprecated: bool,
    #[allow(dead_code)]
    pub(crate) runtime_visible_annotations: Vec<Annotation>,
    pub(crate) extra_attributes: HashMap<String, Box<[u8]>>,
}

/// Contains a field's name and type.
#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) struct FieldNaT {
    pub name: Arc<str>,
    pub typ: Type,
}

#[derive(Hash)]
pub(crate) struct FieldNaTBorrow<'a> {
    pub name: &'a str,
    pub typ: Type,
}

impl Equivalent<FieldNaT> for FieldNaTBorrow<'_> {
    fn equivalent(&self, key: &FieldNaT) -> bool {
        (self.name == key.name.as_ref()) & (self.typ == key.typ)
    }
}

impl Field {
    pub(crate) fn class(&self) -> ClassRef {
        // SAFETY: This reference will not outlive the JVM
        // and this function will only be called after class has been set
        unsafe { &*self.class.load(Ordering::SeqCst) }
    }

    /// Get the value of a static field.
    /// Note that this might not be initialized yet unless a method has been called on this class.
    ///
    /// # Panics
    /// Panics if the field is not static.
    #[track_caller]
    pub(crate) fn static_get(&self, ptrs: Ptrs) -> Value {
        if !self.access_flags.contains(AccessFlags::STATIC) {
            panic!("field {} is not static", self.nat.name)
        }
        self.read(ptrs, self.class().static_storage)
    }

    /// Get the value of an object's field.
    ///
    /// # Panics
    /// Panics if the field is static or the object is not an instance of the class corresponding to this field.
    #[track_caller]
    pub(crate) fn instance_get(&self, ptrs: Ptrs, object: Object) -> Value {
        if self.access_flags.contains(AccessFlags::STATIC) {
            panic!("field {} is static", self.nat.name)
        }
        if !object.class(ptrs).assignable_to(&self.class().name) {
            panic!(
                "object of class {} not instance of {}",
                object.class(ptrs).name,
                self.class().name
            )
        }
        self.read(ptrs, object.fields())
    }

    fn read(&self, ptrs: Ptrs, storage: FieldStorage) -> Value {
        let volatile = self.access_flags.contains(AccessFlags::VOLATILE);
        match self.nat.typ {
            Type::Boolean | Type::Byte => {
                Value::Int(storage.read::<i8>(ptrs, self.byte_offset, volatile) as i32)
            }
            Type::Short | Type::Char => {
                Value::Int(storage.read::<i16>(ptrs, self.byte_offset, volatile) as i32)
            }
            Type::Int => storage.read::<i32>(ptrs, self.byte_offset, volatile).into(),
            Type::Float => storage.read::<f32>(ptrs, self.byte_offset, volatile).into(),
            Type::Long => storage.read::<i64>(ptrs, self.byte_offset, volatile).into(),
            Type::Double => storage.read::<f64>(ptrs, self.byte_offset, volatile).into(),
            Type::Ref(..) => {
                Object::from_addr(storage.read::<usize>(ptrs, self.byte_offset, volatile)).into()
            }
        }
    }

    /// Set the value of a static field.
    ///
    /// # Panics
    /// Panics if the field is not static, the field is final, or the value is not assignable to the field's type
    #[track_caller]
    pub(crate) fn static_set(&self, ptrs: Ptrs, value: Value) {
        if !self.access_flags.contains(AccessFlags::STATIC)
            | self.access_flags.contains(AccessFlags::FINAL)
        {
            panic!("field {} is final or not static", self.nat.name)
        }
        self.write(ptrs, self.class().static_storage, value)
    }

    /// Get the value of an object's field.
    ///
    /// # Panics
    /// Panics if the field is static, the field is final,
    /// the object is not an instance of the class corresponding to this field
    /// or the value is not assignable to the field's type
    #[track_caller]
    pub(crate) fn instance_set(&self, ptrs: Ptrs, object: Object, value: Value) {
        if self.access_flags.contains(AccessFlags::STATIC)
            | self.access_flags.contains(AccessFlags::FINAL)
        {
            panic!("field {} is final or static", self.nat.name)
        }
        if !object.class(ptrs).assignable_to(&self.class().name) {
            panic!(
                "object of class {} not instance of {}",
                object.class(ptrs).name,
                self.class().name
            )
        }
        self.write(ptrs, object.fields(), value)
    }

    fn write(&self, ptrs: Ptrs, storage: FieldStorage, value: Value) {
        let volatile = self.access_flags.contains(AccessFlags::VOLATILE);

        // This is to avoid the need to take a ref to JVM for resolution
        fn assignable(class: &Class, to: &str) -> bool {
            to == class.name.as_ref() || class.super_class.is_some_and(|s| assignable(s, to))
        }

        match (&self.nat.typ, value) {
            (Type::Boolean | Type::Byte, Value::Int(value)) => {
                storage.write::<i8>(ptrs, self.byte_offset, value as i8, volatile)
            }
            (Type::Short | Type::Char, Value::Int(value)) => {
                storage.write::<i16>(ptrs, self.byte_offset, value as i16, volatile)
            }
            (Type::Int, Value::Int(value)) => {
                storage.write::<i32>(ptrs, self.byte_offset, value, volatile)
            }
            (Type::Float, Value::Float(value)) => {
                storage.write::<f32>(ptrs, self.byte_offset, value, volatile)
            }
            (Type::Long, Value::Long(value)) => {
                storage.write::<i64>(ptrs, self.byte_offset, value, volatile)
            }
            (Type::Double, Value::Double(value)) => {
                storage.write::<f64>(ptrs, self.byte_offset, value, volatile)
            }
            (Type::Ref(name), Value::Object(obj))
                if obj.map(|o| assignable(o.class(ptrs), name)).unwrap_or(true) =>
            {
                storage.write::<usize>(
                    ptrs,
                    self.byte_offset,
                    obj.map(|o| o.0.get()).unwrap_or(0),
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

impl Eq for &'_ Field {}
impl PartialEq for &'_ Field {
    fn eq(&self, other: &Self) -> bool {
        core::ptr::eq(*self, *other)
    }
}

impl core::hash::Hash for &'_ Field {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        core::hash::Hash::hash(&(*self as *const _ as usize), state)
    }
}

impl core::fmt::Debug for FieldNaT {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{} {}", self.typ, self.name)
    }
}

impl core::fmt::Debug for Field {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.nat.fmt(f)
    }
}
