use core::ops::Bound;
use core::ops::RangeBounds;

use crate::*;

/// A Java object, guaranteed to be non-null.
/// An object may only be used with the jvm by which it was created.
#[derive(Clone, Eq)]
pub struct Object {
    pub(crate) value: object::Object,
    pub(crate) jvm: Jvm,
}

impl Hash for Object {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        // This is safe because pointers are hashed instead of dereferenced
        self.value.hash(state);
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        // This is safe because pointers are compared instead of dereferenced
        self.value == other.value
    }
}

impl core::fmt::Pointer for Object {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        core::fmt::Pointer::fmt(&self.value, f)
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        object::fmt_object(self.jvm.deref().heap.ptrs(), self.value, f, true)
    }
}

impl Object {
    pub(crate) fn new(jvm: impl Into<Jvm>, value: object::Object) -> Self {
        Self {
            value,
            jvm: jvm.into(),
        }
    }

    #[allow(dead_code)]
    pub(crate) fn as_inner(&self, jvm: &Jvm) -> object::Object {
        if &self.jvm == jvm {
            self.value
        } else {
            panic!("Wrong JVM accessed")
        }
    }

    /// The class this is an instance of.
    pub fn class(&self) -> Class {
        let class = self.value.class(self.jvm.deref().heap.ptrs());
        Class::new(self.jvm.clone(), class)
    }

    /// Get the value of an object's field.
    ///
    /// # Panics
    /// Panics if the field does not exist or is static.
    pub fn get(&self, field_name: &str, field_type: Type) -> Value {
        let Some(field) = self.class().field(field_name, field_type.clone()) else {
            panic!(
                "Field {field_name} of type {field_type} not found in object of type {}",
                self.class().value
            )
        };
        Value::new(
            &self.jvm,
            field
                .value
                .instance_get(self.jvm.deref().heap.ptrs(), self.value),
            &field.value.nat.typ,
        )
    }

    /// Set the value of an object's field.
    ///
    /// # Panics
    /// Panics if the field does not exist or is static or
    /// the value is not assignable to the field's type
    /// or is an object created by a different JVM
    pub fn set(&self, field_name: &str, field_type: Type, value: impl Into<Value>) {
        let Some(field) = self.class().field(field_name, field_type.clone()) else {
            panic!(
                "Field {field_name} of type {field_type} not found in object of type {}",
                self.class().value
            )
        };
        field.as_inner(&self.jvm).instance_set(
            self.jvm.deref().heap.ptrs(),
            self.value,
            value.into().into_inner(&self.jvm),
        )
    }

    /// Returns whether this is an array.
    pub fn is_array(&self) -> bool {
        self.value.is_array(self.jvm.deref().heap.ptrs())
    }

    /// Returns the number of items in this array.
    ///
    /// # Panics
    /// Panics if the object is not an array.
    #[track_caller]
    pub fn array_length(&self) -> i32 {
        if self.class().element_type().is_none() {
            panic!("not an array");
        }
        self.value.array_length()
    }

    /// Get an array element.
    ///
    /// # Panics
    /// Panics if the object is not an array or the `index` is out of bounds.
    #[track_caller]
    pub fn array_read(&self, index: i32) -> Value {
        Value::new(
            &self.jvm,
            self.value
                .array_read_value(self.jvm.deref().heap.ptrs(), index),
            self.class().element_type().as_ref().unwrap(),
        )
    }

    /// Sets an array element. If the element type is boolean, byte, short or char,
    /// the `JvmValue::Int` is truncated
    ///
    /// # Panics
    /// Panics if object is not an array, the element type mismatches, `index` is out of bounds
    /// or the value is an object created by another JVM.
    #[track_caller]
    pub fn array_write(&self, index: i32, value: Value) {
        self.value.array_write_value(
            self.jvm.deref().heap.ptrs(),
            index,
            value.into_inner(&self.jvm),
        )
    }

    /// Enters this object's monitor, blocking if it is already locked by another thread.
    ///
    /// This is reëntrant: A thread may enter the monitor multiple times, and must [`exit`](Object::exit_monitor)
    /// is as many times to unlock the object.
    pub fn enter_monitor(&self) {
        self.jvm
            .deref()
            .parking_lot
            .monitor_enter(self.value.mark_word());
    }

    /// Exists this object's monitor.
    /// After 2³¹-1 more calls to `enter_monitor` on the same thread than to `exit_monitor`, this will fail to release the lock.
    ///
    /// Throws `IllegalMonitorStateException` is the object is not locked by this thread.
    pub fn exit_monitor(&self) -> JvmResult<()> {
        self.jvm
            .deref()
            .parking_lot
            .monitor_exit(self.value.mark_word())
            .map_err(|_| {
                Object::new(
                    self.jvm.clone(),
                    throw(self.jvm.deref(), "IllegalMonitorStateException", None),
                )
            })
    }

    /// Wait on this object until notified by [`notify`](Object::notify) or [`notify_all`](Object::notify_all),
    /// or until spuriously woken up.
    ///
    /// This thread must hold the object's lock beforehand (otherwise `IllegalMonitorStateException` will be thrown)
    /// and will reäquire the lock when woken.
    pub fn wait(&self) -> JvmResult<()> {
        self.jvm
            .deref()
            .parking_lot
            .monitor_wait(self.value.mark_word())
            .map_err(|_| {
                Object::new(
                    self.jvm.clone(),
                    throw(self.jvm.deref(), "IllegalMonitorStateException", None),
                )
            })
    }

    /// Notifies a thread [`wait`ing](Object::wait) on this object.
    ///
    /// This thread must hold the object's lock (otherwise `IllegalMonitorStateException` will be thrown).
    pub fn notify(self) -> JvmResult<()> {
        self.jvm
            .deref()
            .parking_lot
            .monitor_notify(self.value.mark_word())
            .map_err(|_| {
                Object::new(
                    self.jvm.clone(),
                    throw(self.jvm.deref(), "IllegalMonitorStateException", None),
                )
            })
    }

    /// Notifies all threads [`wait`ing](Object::wait) on this object.
    ///
    /// This thread must hold the object's lock (otherwise `IllegalMonitorStateException` will be thrown).
    pub fn notify_all(self) -> JvmResult<()> {
        self.jvm
            .deref()
            .parking_lot
            .monitor_notify_all(self.value.mark_word())
            .map_err(|_| {
                Object::new(
                    self.jvm.clone(),
                    throw(self.jvm.deref(), "IllegalMonitorStateException", None),
                )
            })
    }

    /// Creates a debug representation of the object. For a full object tree, use `Debug`.
    pub fn format_short(self) -> String {
        self.value.format_short(self.jvm.deref().heap.ptrs())
    }

    pub fn identity_hash(self) -> i32 {
        self.jvm
            .deref()
            .identity_hashes
            .get_identity_hash(self.value)
    }

    /// Tries to read part of this array.
    ///
    /// Returns a `ClassCastException` if this object isn't
    /// an array of the appropriate type.
    /// Returns an `ArrayIndexOutOfBoundsException` is the range
    /// is out of bounds.
    pub fn array_read_range<T: Datum>(
        &self,
        range: impl RangeBounds<i32>,
    ) -> JvmResult<impl Iterator<Item = T>> {
        Ok({
            // Type safety
            let class_cast = || {
                Err(Object::new(
                    self.jvm.deref(),
                    throw(self.jvm.deref(), "ClassCastException", None),
                ))
            };
            let Some(elem_type) = self.class().element_type() else {
                return class_cast();
            };
            if let Some(typ) = T::STATIC_TYPE
                && typ != elem_type
            {
                return class_cast();
            }

            // Bounds
            let start = match range.start_bound() {
                Bound::Included(&start) => start,
                Bound::Excluded(&start) => start + 1,
                Bound::Unbounded => 0,
            };
            let end = match range.end_bound() {
                Bound::Included(&end) => end,
                Bound::Excluded(&end) => end - 1,
                Bound::Unbounded => self.array_length(),
            };

            if (start < 0) | (end >= self.array_length()) {
                return Err(Object::new(
                    self.jvm.deref(),
                    throw(self.jvm.deref(), "ArrayIndexOutOfBoundsException", None),
                ));
            }

            // Actually do the op
            (0..self.value.array_length()).map(|index| {
                T::read(
                    &self.jvm,
                    self.value.array_read(self.jvm.deref(), index).unwrap(),
                )
            })
        })
    }

    /// Tries to override part of this array.
    ///
    /// Returns a `ClassCastException` if this object isn't
    /// an array of the appropriate type.
    /// Returns an `ArrayIndexOutOfBoundsException` is the range
    /// is out of bounds.
    ///
    /// # Panics
    /// Panics if the iterator returns fewer values than needed
    /// for the specified range.
    pub fn array_write_range<'a, T: 'a + Datum>(
        &self,
        range: impl RangeBounds<i32>,
        iter: impl IntoIterator<Item = &'a T>,
    ) -> JvmResult<()> {
        // Type safety, part 1
        let class_cast = || {
            Err(Object::new(
                self.jvm.deref(),
                throw(self.jvm.deref(), "ClassCastException", None),
            ))
        };
        let Some(elem_type) = self.class().element_type() else {
            return class_cast();
        };
        if let Some(typ) = T::STATIC_TYPE
            && typ != elem_type
        {
            return class_cast();
        }

        // Bounds
        let start = match range.start_bound() {
            Bound::Included(&start) => start,
            Bound::Excluded(&start) => start + 1,
            Bound::Unbounded => 0,
        };
        let end = match range.end_bound() {
            Bound::Included(&end) => end,
            Bound::Excluded(&end) => end - 1,
            Bound::Unbounded => self.array_length(),
        };

        if (start < 0) | (end > self.array_length()) {
            return Err(Object::new(
                self.jvm.deref(),
                throw(self.jvm.deref(), "ArrayIndexOutOfBoundsException", None),
            ));
        }

        // Write
        let mut iter = iter.into_iter();
        for index in start..end {
            let value = iter.next().expect("Iterator returned None");
            if !T::write_valid(value, &elem_type) {
                return class_cast();
            }
            self.value
                .array_write(self.jvm.deref(), index, T::write(&self.jvm, value))
                .unwrap();
        }
        Ok(())
    }
}
