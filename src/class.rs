use hashbrown::HashMap;
use std::{
    sync::{Arc, Condvar, Mutex, RwLock},
    thread::ThreadId,
};

use crate::{
    const_pool::ConstPool,
    field::{Field, FieldNaT, FieldNaTBorrow},
    heap::JvmPtr,
    jvm::{error, JvmResult},
    method::{MethodDescriptor, MethodNaT, MethodNaTBorrow, MethodRef},
    monitor::Monitor,
    object::{self, FieldStorage, Object},
    typ::Type,
    verification::verify,
    AccessFlags, Jvm,
};

/// Represents an interface, regular class, array class or enum.
/// Does not store its defining loader as the Jvm currently may only have one.
pub(crate) struct Class {
    /// Fully qualified name, e.g. "java.lang.Object[]"
    pub(crate) name: Arc<str>,
    pub(crate) super_class: Option<ClassRef>,
    /// For array classes, this is the type of the array elements; None for normal classes
    pub(crate) element_type: Option<Type>,
    /// Write access is required for resolution and hot reloading.
    /// Otherwise only read access is required.
    pub(crate) const_pool: RwLock<ConstPool>,
    pub(crate) access_flags: AccessFlags,
    pub(crate) interfaces: Vec<ClassRef>,
    /// Does not contain inherited fields.
    /// Note: JVM supports field overloading.
    pub(crate) fields: HashMap<FieldNaT, Field>,
    /// At the moment, this includes any inherited methods. Change this?
    pub(crate) methods: HashMap<MethodNaT, MethodRef>,
    pub(crate) static_storage: FieldStorage,
    /// Combined size of instance fields in normal class, 0 if array
    pub(crate) object_size: usize,
    pub(crate) init: Mutex<ClassState>,
    pub(crate) init_waiter: Condvar,
}

#[derive(Debug)]
pub(crate) enum ClassState {
    Unverified,
    Verified,
    InProgress(ThreadId),
    /// Initialization succeeded.
    Initialized,
    /// Initialization failed; holds the exception thrown.
    Error(Object),
}

pub(crate) type ClassRef = &'static Class;

impl Eq for ClassRef {}
impl PartialEq for ClassRef {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(*self, *other)
    }
}

impl std::hash::Hash for ClassRef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::hash::Hash::hash(&(*self as *const Class as usize), state)
    }
}

impl Class {
    pub(crate) fn set_layouts(
        super_class: Option<&Class>,
        fields: Vec<Field>,
    ) -> (
        HashMap<FieldNaT, Field>,
        /*static size*/ usize,
        /*instance size*/ usize,
    ) {
        let mut static_fields = Vec::new();
        let mut object_fields = Vec::new();
        for field in fields {
            if field.access_flags.contains(AccessFlags::STATIC) {
                static_fields.push(field);
            } else {
                object_fields.push(field);
            }
        }

        let mut fields = HashMap::default();

        let mut set_layouts = |storage_size: &mut usize, mut add_fields: Vec<Field>| {
            add_fields.sort_by_key(|field| field.nat.typ.size());
            'outer: while !add_fields.is_empty() {
                // Try to find the biggest field we can append without padding
                let log_align = storage_size.trailing_zeros();
                for i in 0..add_fields.len() {
                    if add_fields[i].nat.typ.size().trailing_zeros() <= log_align {
                        let mut field = add_fields.remove(i);
                        field.byte_offset = *storage_size;
                        *storage_size += field.nat.typ.size();
                        fields.insert(field.nat.clone(), field);
                        continue 'outer;
                    }
                }

                // Alright, we need padding, so continue with the smallest field
                let mut field = add_fields.pop().unwrap();
                while storage_size.trailing_zeros() < field.nat.typ.size().trailing_zeros() {
                    *storage_size += 1;
                }
                field.byte_offset = *storage_size;
                *storage_size += field.nat.typ.size();
                fields.insert(field.nat.clone(), field);
            }
        };

        let mut static_size = size_of::<Monitor>();
        set_layouts(&mut static_size, static_fields);

        let mut object_size = match super_class {
            Some(super_class) => super_class.object_size,
            None => object::OFFSET_FIELDS,
        };
        set_layouts(&mut object_size, object_fields);

        (fields, static_size, object_size)
    }

    pub(crate) fn is_interface(&self) -> bool {
        self.access_flags.contains(AccessFlags::INTERFACE)
    }

    /// Convenience method for getting this class' type
    pub(crate) fn typ(&self) -> Type {
        Type::Ref(self.name.clone())
    }

    pub(crate) fn method(
        &self,
        name: &str,
        args: impl IntoIterator<Item = Type>,
        ret: Option<Type>,
    ) -> Option<MethodRef> {
        self.methods
            .get(&MethodNaTBorrow {
                name,
                typ: &MethodDescriptor {
                    args: args.into_iter().collect(),
                    returns: ret,
                },
            })
            .copied()
    }

    pub(crate) fn iter_fields(&'static self) -> impl Iterator<Item = &'static Field> {
        struct FieldIterator {
            fields: hashbrown::hash_map::Values<'static, FieldNaT, Field>,
            class: &'static Class,
        }
        impl Iterator for FieldIterator {
            type Item = &'static Field;

            fn next(&mut self) -> Option<Self::Item> {
                if let Some(field) = self.fields.next() {
                    Some(field)
                } else if let Some(super_class) = self.class.super_class {
                    self.fields = super_class.fields.values();
                    self.class = super_class;
                    self.next()
                } else {
                    None
                }
            }
        }
        FieldIterator {
            fields: self.fields.values(),
            class: self,
        }
    }

    pub(crate) fn field(&self, name: &str, typ: Type) -> Option<&Field> {
        self.field_by_nat(FieldNaTBorrow { name, typ })
    }

    pub(crate) fn field_by_nat(&self, nat: FieldNaTBorrow) -> Option<&Field> {
        self.fields
            .get(&nat)
            .or_else(|| self.super_class.and_then(|c| c.field_by_nat(nat)))
    }

    pub(crate) fn monitor(&self) -> &'static Monitor {
        unsafe { (self.static_storage.0 as *const Monitor).as_ref_unchecked() }
    }

    // TODO: provide fields() and methods()
    // figure out how to present distinguished between inherited and not

    pub fn true_subclass_of(&self, other: &str) -> bool {
        self.super_class
            .map(|sc| sc.assignable_to(other))
            .unwrap_or(false)
    }

    pub fn assignable_to(&self, other: &str) -> bool {
        (self.name.as_ref() == other)
            || self.true_subclass_of(other)
            || self.interfaces.iter().any(|i| other == &*i.name)
    }

    /// Ensures this class is initialized. Includes linking, verification, preparation & resolution.
    /// This happens automatically when a method belonging to this class is called.
    // May only be called by the library consumer or instuctions new, getstatic, putstatic or invokestatic or by being the initial class
    pub fn ensure_init(&'static self, jvm: &Jvm) -> JvmResult<()> {
        loop {
            let mut guard = self.init.lock().unwrap();
            match *guard {
                ClassState::InProgress(thread) => {
                    if thread == std::thread::current().id() {
                        return Ok(());
                    } else {
                        drop(self.init_waiter.wait(guard).unwrap());
                    }
                }
                ClassState::Unverified => {
                    *guard = ClassState::InProgress(std::thread::current().id());
                    drop(guard);
                    self.verify(jvm)?;
                    self.init_waiter.notify_all();
                }
                ClassState::Verified => {
                    *guard = ClassState::InProgress(std::thread::current().id());
                    drop(guard);

                    if let Some(super_class) = self.super_class {
                        super_class.ensure_init(jvm)?;
                    }

                    for interface in &self.interfaces {
                        interface.ensure_init(jvm)?;
                    }

                    let result = self.init(jvm);
                    *self.init.lock().unwrap() = match result {
                        Ok(()) => ClassState::Initialized,
                        Err(err) => ClassState::Error(err),
                    };
                    self.init_waiter.notify_all();
                    return result;
                }
                ClassState::Initialized => return Ok(()),
                ClassState::Error(err) => return Err(err),
            }
        }
    }

    pub(crate) fn verify(&'static self, jvm: &Jvm) -> JvmResult<()> {
        let mut guard = self.init.lock().unwrap();
        loop {
            match *guard {
                ClassState::Unverified => break,
                ClassState::InProgress(thread) => {
                    if thread == std::thread::current().id() {
                        break;
                    } else {
                        guard = self.init_waiter.wait(guard).unwrap();
                    }
                }
                ClassState::Verified | ClassState::Initialized => return Ok(()),
                ClassState::Error(err) => return Err(err),
            }
        }
        let result = self.verify_impl(jvm);
        *guard = match result {
            Ok(()) => ClassState::Verified,
            Err(err) => ClassState::Error(err),
        };
        result
    }

    pub(crate) fn verify_impl(&'static self, jvm: &Jvm) -> JvmResult<()> {
        let mut const_pool = self.const_pool.write().unwrap();
        // Preparation
        for field in self.fields.values() {
            if let Some(index) = field.const_value_index {
                match &field.nat.typ {
                    Type::Boolean | Type::Byte => self.static_storage.write::<i8>(
                        field.byte_offset,
                        const_pool.get_int(jvm, index)? as i8,
                        true,
                    ),
                    Type::Short | Type::Char => self.static_storage.write::<i16>(
                        field.byte_offset,
                        const_pool.get_int(jvm, index)? as i16,
                        true,
                    ),
                    Type::Int => self.static_storage.write::<i32>(
                        field.byte_offset,
                        const_pool.get_int(jvm, index)?,
                        true,
                    ),
                    Type::Float => self.static_storage.write::<f32>(
                        field.byte_offset,
                        const_pool.get_float(jvm, index)?,
                        true,
                    ),
                    Type::Double => self.static_storage.write::<f64>(
                        field.byte_offset,
                        const_pool.get_double(jvm, index)?,
                        true,
                    ),
                    Type::Long => self.static_storage.write::<i64>(
                        field.byte_offset,
                        const_pool.get_long(jvm, index)?,
                        true,
                    ),
                    Type::Ref(name) => {
                        if name.as_ref() == "java.lang.String" {
                            self.static_storage.write::<JvmPtr>(
                                field.byte_offset,
                                const_pool.get_string(jvm, index)?.ptr().into(),
                                true,
                            )
                        } else {
                            return Err(error(
                                jvm,
                                "ClassFormatError",
                                &format!("{}: preparation failed", self.name),
                            ));
                        }
                    }
                }
            }
        }

        // Resolution
        const_pool.resolve(jvm)?;
        drop(const_pool);

        // Verification
        verify(jvm, self)?;

        Ok(())
    }

    /// Requires class to be verified, may only be called from self.ensure_init
    fn init(&'static self, jvm: &Jvm) -> JvmResult<()> {
        // Initialization
        if let Some(initializer) = self.method("<clinit>", vec![], None) {
            crate::interpreter::invoke_initializer(jvm, initializer)?;
        }

        Ok(())
    }
}

impl std::fmt::Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl std::fmt::Debug for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}
