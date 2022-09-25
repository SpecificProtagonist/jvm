use parking_lot::{Condvar, Mutex, RwLock};
use std::{collections::HashMap, sync::Arc, thread::ThreadId};

use crate::{
    const_pool::ConstPool,
    field::{Field, FieldNaT},
    field_storage::FieldStorage,
    jvm::exception,
    jvm::JVMResult,
    jvm::Jvm,
    method::Method,
    method::MethodDescriptor,
    method::MethodNaT,
    object::Object,
    typ::Typ,
    verification::verify,
    AccessFlags,
};

/// Represents a regular class, an array class or (in the future) an enum or an interface
pub(crate) struct Class {
    // TODO: use normal name, not internal name
    /// Fully qualified name
    pub(crate) name: Arc<str>,
    pub(crate) super_class: Option<&'static Class>,
    /// For array classes, this is the type of the array elements; None for normal classes
    pub(crate) element_type: Option<Typ>,
    /// When the class is initialized, write access is required for resolution.
    /// Afterward (such as when executing a function in this class), only read access is required.
    pub(crate) const_pool: RwLock<ConstPool>,
    pub(crate) access_flags: AccessFlags,
    #[allow(unused)]
    pub(crate) interfaces: Vec<&'static Class>,
    /// As far as I can tell, JVM supports field overloading
    pub(crate) fields: HashMap<FieldNaT, Field>,
    /// At the moment, this includes any inherited methods. Change this?
    pub(crate) methods: HashMap<MethodNaT<'static>, &'static Method>,
    pub(crate) static_storage: FieldStorage,
    /// Combined size of instance fields in normal class, 0 if array
    pub(crate) object_size: usize,
    pub(crate) init: Mutex<ClassInitState>,
    pub(crate) init_waiter: Condvar,
}

pub(crate) enum ClassInitState {
    Uninit,
    InProgress(ThreadId),
    Done,
    Error(Object),
}

impl<'a> Eq for &'a Class {}
impl<'a> PartialEq for &'a Class {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(*self, *other)
    }
}

impl<'a> std::hash::Hash for &'a Class {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::hash::Hash::hash(&(*self as *const Class as usize), state)
    }
}

impl Class {
    pub(crate) fn field(&self, name: &str, typ: Typ) -> Option<&Field> {
        self.field_by_nat(FieldNaT {
            name: name.into(),
            typ,
        })
    }

    pub(crate) fn field_by_nat(&self, nat: FieldNaT) -> Option<&Field> {
        self.fields
            .get(&nat)
            .or_else(|| self.super_class.and_then(|c| c.field_by_nat(nat)))
    }

    // TODO: provide fields() and methods()
    // figure out how to present distinguished between inherited and not

    pub fn true_subclass_of(&self, other: &str) -> bool {
        self.super_class
            .map(|sc| sc.assignable_to(other))
            .unwrap_or(false)
    }

    pub fn assignable_to(&self, other: &str) -> bool {
        (self.name.as_ref() == other) || self.true_subclass_of(other)
    }

    /// Ensures this class is initialized. Includes linking, verification, preparation & resolution.
    /// This happens automatically when a method belonging to this class is called.
    // May only be called by the library consumer or instuctions new, getstatic, putstatic or invokestatic or by being the initial class
    pub fn ensure_init(&self, jvm: &Jvm) -> JVMResult<()> {
        let mut guard = self.init.lock();
        match *guard {
            ClassInitState::Done => Ok(()),
            ClassInitState::Error(err) => Err(err),
            ClassInitState::InProgress(thread) => {
                if thread == std::thread::current().id() {
                    Ok(())
                } else {
                    loop {
                        self.init_waiter.wait(&mut guard);
                        match *guard {
                            ClassInitState::Done => return Ok(()),
                            ClassInitState::Error(err) => return Err(err),
                            ClassInitState::InProgress(_) => continue,
                            ClassInitState::Uninit => unreachable!(),
                        }
                    }
                }
            }
            ClassInitState::Uninit => {
                *guard = ClassInitState::InProgress(std::thread::current().id());
                drop(guard);

                if let Some(super_class) = self.super_class {
                    super_class.ensure_init(jvm)?;
                }

                let result = self.init(jvm);
                *self.init.lock() = match result {
                    Ok(()) => ClassInitState::Done,
                    Err(err) => ClassInitState::Error(err),
                };
                self.init_waiter.notify_all();
                result
            }
        }
    }

    /// This may only be called from self.ensure_init
    fn init(&self, jvm: &Jvm) -> JVMResult<()> {
        let mut const_pool = self.const_pool.write();
        // Preparation
        for field in self.fields.values() {
            if let Some(index) = field.const_value_index {
                match &field.nat.typ {
                    Typ::Bool | Typ::Byte => unsafe {
                        self.static_storage.write_i8(
                            field.byte_offset as usize,
                            const_pool.get_int(jvm, index)? as i8,
                            true,
                        )
                    },
                    Typ::Short | Typ::Char => unsafe {
                        self.static_storage.write_i16(
                            field.byte_offset as usize,
                            const_pool.get_int(jvm, index)? as i16,
                            true,
                        )
                    },
                    Typ::Int => unsafe {
                        self.static_storage.write_i32(
                            field.byte_offset as usize,
                            const_pool.get_int(jvm, index)?,
                            true,
                        )
                    },
                    Typ::Float => unsafe {
                        self.static_storage.write_f32(
                            field.byte_offset as usize,
                            const_pool.get_float(jvm, index)?,
                            true,
                        )
                    },
                    Typ::Double => unsafe {
                        self.static_storage.write_f64(
                            field.byte_offset as usize,
                            const_pool.get_double(jvm, index)?,
                            true,
                        )
                    },
                    Typ::Long => unsafe {
                        self.static_storage.write_i64(
                            field.byte_offset as usize,
                            const_pool.get_long(jvm, index)?,
                            true,
                        )
                    },
                    Typ::Ref(name) => {
                        if name.as_ref() == "java/lang/String" {
                            unsafe {
                                self.static_storage.write_ptr(
                                    field.byte_offset as usize,
                                    const_pool.get_string(jvm, index)?.ptr().into(),
                                    true,
                                )
                            }
                        } else {
                            return Err(exception(jvm, "ClassFormatError"));
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

        // Initialization
        if let Some(initializer) = self.methods.get(&MethodNaT {
            name: "<clinit>".into(),
            typ: &MethodDescriptor(vec![], None),
        }) {
            crate::interp::invoke_initializer(jvm, initializer)?;
        }

        Ok(())
    }
}

impl std::fmt::Debug for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}
