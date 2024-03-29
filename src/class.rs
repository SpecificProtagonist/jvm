use parking_lot::{Condvar, Mutex, RwLock};
use std::{alloc::Layout, collections::HashMap, sync::Arc, thread::ThreadId};

use crate::{
    const_pool::ConstPool,
    field::{Field, FieldNaT},
    field_storage::FieldStorage,
    jvm::exception,
    jvm::JVMResult,
    jvm::Jvm,
    method::{MethodDescriptor, MethodNaT, MethodPtr},
    object::{self, Object},
    typ::Typ,
    verification::verify,
    AccessFlags,
};

/// Represents an interface, regular class, array class or enum.
/// Does not store its defining loader as the Jvm currently may only have one.
pub(crate) struct Class {
    /// Fully qualified name, e.g. "java.lang.Object[]"
    pub(crate) name: Arc<str>,
    pub(crate) super_class: Option<ClassPtr>,
    /// For array classes, this is the type of the array elements; None for normal classes
    pub(crate) element_type: Option<Typ>,
    /// When the class is initialized, write access is required for resolution.
    /// Afterward (such as when executing a function in this class), only read access is required.
    pub(crate) const_pool: RwLock<ConstPool>,
    pub(crate) access_flags: AccessFlags,
    pub(crate) interfaces: Vec<ClassPtr>,
    /// Does not contain inherited fields.
    /// As far as I can tell, JVM supports field overloading.
    pub(crate) fields: HashMap<FieldNaT, Field>,
    /// At the moment, this includes any inherited methods. Change this?
    pub(crate) methods: HashMap<MethodNaT<'static>, MethodPtr>,
    pub(crate) static_storage: FieldStorage,
    /// Combined size of instance fields in normal class, 0 if array
    pub(crate) object_size: usize,
    pub(crate) init: Mutex<ClassInitState>,
    pub(crate) init_waiter: Condvar,
}

pub(crate) enum ClassInitState {
    Uninit,
    InProgress(ThreadId),
    /// Initialization succeeded.
    Done,
    /// Initialization failed; holds the exception thrown.
    Error(Object),
}

pub(crate) type ClassPtr = &'static Class;

impl Eq for ClassPtr {}
impl PartialEq for ClassPtr {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(*self, *other)
    }
}

impl std::hash::Hash for ClassPtr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::hash::Hash::hash(&(*self as *const Class as usize), state)
    }
}

impl Class {
    pub(crate) fn set_layouts(
        super_class: Option<&Class>,
        mut fields: Vec<Field>,
    ) -> (
        HashMap<FieldNaT, Field>,
        /*static size*/ usize,
        /*instance size*/ usize,
    ) {
        fields.sort_by_key(|field| field.nat.typ.layout().align());
        let mut static_layout = Layout::new::<()>();
        let mut object_layout = Layout::from_size_align(
            match super_class {
                Some(super_class) => super_class.object_size,
                None => object::header_size(),
            },
            8,
        )
        .unwrap();
        let fields = fields
            .into_iter()
            .map(|mut field| {
                let layout = field.nat.typ.layout();
                let fields_layout = if field.access_flags.contains(AccessFlags::STATIC) {
                    &mut static_layout
                } else {
                    &mut object_layout
                };
                let result = fields_layout.extend(layout).unwrap();
                *fields_layout = result.0;
                field.byte_offset = result.1;
                (field.nat.clone(), field)
            })
            .collect();

        (fields, static_layout.size(), object_layout.size())
    }

    pub(crate) fn interface(&self) -> bool {
        self.access_flags.contains(AccessFlags::INTERACE)
    }

    /// Convenience method for getting this class' type
    pub(crate) fn typ(&self) -> Typ {
        Typ::Ref(self.name.clone())
    }

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
    pub fn ensure_init(&'static self, jvm: &Jvm) -> JVMResult<()> {
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
    fn init(&'static self, jvm: &Jvm) -> JVMResult<()> {
        let mut const_pool = self.const_pool.write();
        // Preparation
        for field in self.fields.values() {
            if let Some(index) = field.const_value_index {
                match &field.nat.typ {
                    Typ::Boolean | Typ::Byte => unsafe {
                        self.static_storage.write_i8(
                            field.byte_offset,
                            const_pool.get_int(jvm, index)? as i8,
                            true,
                        )
                    },
                    Typ::Short | Typ::Char => unsafe {
                        self.static_storage.write_i16(
                            field.byte_offset,
                            const_pool.get_int(jvm, index)? as i16,
                            true,
                        )
                    },
                    Typ::Int => unsafe {
                        self.static_storage.write_i32(
                            field.byte_offset,
                            const_pool.get_int(jvm, index)?,
                            true,
                        )
                    },
                    Typ::Float => unsafe {
                        self.static_storage.write_f32(
                            field.byte_offset,
                            const_pool.get_float(jvm, index)?,
                            true,
                        )
                    },
                    Typ::Double => unsafe {
                        self.static_storage.write_f64(
                            field.byte_offset,
                            const_pool.get_double(jvm, index)?,
                            true,
                        )
                    },
                    Typ::Long => unsafe {
                        self.static_storage.write_i64(
                            field.byte_offset,
                            const_pool.get_long(jvm, index)?,
                            true,
                        )
                    },
                    Typ::Ref(name) => {
                        if name.as_ref() == "java.lang.String" {
                            unsafe {
                                self.static_storage.write_ptr(
                                    field.byte_offset,
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
            typ: &MethodDescriptor {
                args: vec![],
                returns: None,
            },
        }) {
            crate::interpreter::invoke_initializer(jvm, initializer)?;
        }

        Ok(())
    }
}

impl std::fmt::Debug for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}
