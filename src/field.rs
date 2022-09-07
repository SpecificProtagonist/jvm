use std::sync::Arc;

use crossbeam_utils::atomic::AtomicCell;

use crate::{
    field_storage::FieldStorage, heap::NULL_PTR, object::Object, AccessFlags, Class, JVMValue, Typ,
};

pub struct Field<'a> {
    /// Fields within a class are uniquely identified by name and type – multiple fields of the same name may exist
    pub(crate) nat: FieldNaT,
    /// Class is behind a AtomicCell to enable circular references
    pub(crate) class: AtomicCell<&'a Class<'a>>,
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

impl<'a> Field<'a> {
    pub fn nat(&self) -> &FieldNaT {
        &self.nat
    }

    pub fn access_flags(&self) -> AccessFlags {
        self.access_flags
    }

    pub fn class(&self) -> &'a Class<'a> {
        self.class.load()
    }

    /// Get the value of a static field.
    /// Note that this might not be initialized yet unless a method has been called on this class.
    /// # Panics
    /// Panics if the field is not static.
    pub fn static_get(&self) -> JVMValue<'a> {
        if !self.access_flags.contains(AccessFlags::STATIC) {
            panic!("field {} is not static", self.nat.name)
        }
        self.read(&self.class().static_storage)
    }

    /// Get the value of an object's field.
    /// # Panics
    /// Panics if the field is static or the object is not an instance of the class corresponding to this field.
    pub fn instance_get(&self, object: Object<'a>) -> JVMValue<'a> {
        if self.access_flags.contains(AccessFlags::STATIC) {
            panic!("field {} is static", self.nat.name)
        }
        if !object.class().assignable_to(&self.class().name) {
            panic!(
                "object of class {} not instance of {}",
                object.class().name(),
                self.class().name()
            )
        }
        self.read(&object.ptr)
    }

    fn read(&self, storage: &FieldStorage) -> JVMValue<'a> {
        let volatile = self.access_flags.contains(AccessFlags::VOLATILE);
        // SAFETY: bounds, alignment & datatype unchanged after construction
        unsafe {
            match self.nat.typ {
                Typ::Bool | Typ::Byte => {
                    JVMValue::Int(storage.read_i8(self.byte_offset, volatile) as i32)
                }
                Typ::Short | Typ::Char => {
                    JVMValue::Int(storage.read_i16(self.byte_offset, volatile) as i32)
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
    pub fn static_set(&self, object: Object<'a>, value: JVMValue<'a>) {
        if !self.access_flags.contains(AccessFlags::STATIC) {
            panic!("field {} is not static", self.nat.name)
        }
        self.write(&object.ptr, value)
    }

    /// Get the value of an object's field.
    /// # Panics
    /// Panics if the field is static,
    /// the object is not an instance of the class corresponding to this field
    /// or the value is not assignable to the field's type
    pub fn instance_set(&self, object: Object<'a>, value: JVMValue<'a>) {
        if self.access_flags.contains(AccessFlags::STATIC) {
            panic!("field {} is static", self.nat.name)
        }
        if !object.class().assignable_to(&self.class().name) {
            panic!(
                "object of class {} not instance of {}",
                object.class().name(),
                self.class().name()
            )
        }
        self.write(&object.ptr, value)
    }

    fn write(&self, storage: &FieldStorage, value: JVMValue<'a>) {
        let volatile = self.access_flags.contains(AccessFlags::VOLATILE);

        // This is to avoid the need to take a ref to JVM for resolution
        fn assignable<'a>(class: &'a Class<'a>, to: &str) -> bool {
            to == class.name.as_ref() || class.super_class.map_or(false, |s| assignable(s, to))
        }

        // SAFETY: bounds, alignment & datatype unchanged after construction
        unsafe {
            match (&self.nat.typ, value) {
                (Typ::Bool | Typ::Byte, JVMValue::Int(value)) => {
                    storage.write_i8(self.byte_offset, value as i8, volatile)
                }
                (Typ::Short | Typ::Char, JVMValue::Int(value)) => {
                    storage.write_i16(self.byte_offset, value as i16, volatile)
                }
                (Typ::Int, JVMValue::Int(value)) => {
                    storage.write_i32(self.byte_offset, value, volatile)
                }
                (Typ::Float, JVMValue::Float(value)) => {
                    storage.write_f32(self.byte_offset, value, volatile)
                }
                (Typ::Long, JVMValue::Long(value)) => {
                    storage.write_i64(self.byte_offset, value, volatile)
                }
                (Typ::Double, JVMValue::Double(value)) => {
                    storage.write_f64(self.byte_offset, value, volatile)
                }
                (Typ::Ref(name), JVMValue::Ref(obj))
                    if obj.map(|o| assignable(o.class(), &name)).unwrap_or(true) =>
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

impl<'a, 'b> Eq for &'b Field<'a> {}
impl<'a, 'b> PartialEq for &'b Field<'a> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(*self, *other)
    }
}

impl std::fmt::Debug for FieldNaT {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.typ, self.name)
    }
}

impl<'a> std::fmt::Debug for Field<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.nat.fmt(f)
    }
}
