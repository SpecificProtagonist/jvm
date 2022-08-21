use parking_lot::{Condvar, Mutex, RwLock};
use std::{collections::HashMap, thread::ThreadId};

use crate::{
    const_pool::ConstPool,
    exception,
    field::{Field, FieldNaT},
    field_storage::FieldStorage,
    heap::Heap,
    object::Object,
    verification::verify,
    AccessFlags, IntStr, JVMResult, MaybeInteredString, Method, MethodDescriptor, MethodNaT, Typ,
    JVM,
};

/// Represents a regular class, an array class or (in the future) an enum or an interface
pub struct Class<'a> {
    // TODO: use normal name, not internal name
    /// Fully qualified name
    pub(crate) name: IntStr<'a>,
    pub(crate) super_class: Option<&'a Class<'a>>,
    /// For array classes, this is the type of the array elements; None for normal classes
    pub(crate) element_type: Option<Typ<'a>>,
    /// When the class is initialized, write access is required for resolution.
    /// Afterward (such as when executing a function in this class), only read access is required.
    pub(crate) const_pool: RwLock<ConstPool<'a>>,
    pub(crate) access_flags: AccessFlags,
    #[allow(unused)]
    pub(crate) interfaces: Vec<&'a Class<'a>>,
    /// As far as I can tell, JVM supports field overloading
    pub(crate) fields: HashMap<FieldNaT<'a>, Field<'a>>,
    /// At the moment, this includes any inherited methods. Change this?
    pub(crate) methods: HashMap<MethodNaT<'a>, &'a Method<'a>>,
    pub(crate) static_storage: FieldStorage,
    /// Combined size of instance fields in normal class, 0 if array
    pub(crate) object_size: usize,
    pub(crate) init: Mutex<ClassInitState<'a>>,
    pub(crate) init_waiter: Condvar,
}

pub(crate) enum ClassInitState<'a> {
    Uninit,
    InProgress(ThreadId),
    Done,
    Error(Object<'a>),
}

impl<'a, 'b> Eq for &'b Class<'a> {}
impl<'a, 'b> PartialEq for &'b Class<'a> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(*self, *other)
    }
}

impl<'a> Class<'a> {
    /// Returns the fully qualified internal name (using '/' instead of '.')
    pub fn name(&'a self) -> IntStr<'a> {
        self.name
    }

    /// Is Some() for all classes except `java.lang.Object`
    pub fn super_class(&'a self) -> Option<&'a Self> {
        self.super_class
    }

    pub fn field<'b>(
        &'a self,
        jvm: &'b JVM<'a>,
        name: impl Into<MaybeInteredString<'a, 'b>>,
        typ: Typ<'a>,
    ) -> Option<&'a Field<'a>> {
        let nat = FieldNaT {
            name: name.into().get(jvm),
            typ,
        };
        self.field_by_nat(nat)
    }

    pub fn field_by_nat(&'a self, nat: FieldNaT<'a>) -> Option<&'a Field<'a>> {
        self.fields
            .get(&nat)
            .or_else(|| self.super_class.and_then(|c| c.field_by_nat(nat)))
    }

    pub fn method(&self, nat: &MethodNaT) -> Option<&'a Method<'a>> {
        self.methods.get(nat).copied()
    }

    // TODO: provide fields() and methods()
    // figure out how to present distinguished between inherited and not

    pub fn true_subclass_of(&'a self, other: IntStr<'a>) -> bool {
        self.super_class
            .map(|sc| sc.assignable_to(other))
            .unwrap_or(false)
    }

    pub fn assignable_to(&'a self, other: IntStr<'a>) -> bool {
        (self.name == other) || self.true_subclass_of(other)
    }

    pub(crate) fn dummy_class(heap: &Heap) -> Self {
        Class {
            element_type: None,
            const_pool: Default::default(),
            access_flags: AccessFlags::empty(),
            name: IntStr(""),
            super_class: None,
            interfaces: Default::default(),
            static_storage: FieldStorage::new(heap, 0),
            object_size: 0,
            fields: Default::default(),
            methods: Default::default(),
            init: ClassInitState::Uninit.into(),
            init_waiter: Condvar::new(),
        }
    }

    /// Ensures this class is initialized. Includes linking, verification, preparation & resolution.
    /// This happens automatically when a method belonging to this class is called.
    // May only be called by the library consumer or instuctions new, getstatic, putstatic or invokestatic or by being the initial class
    pub fn ensure_init<'b>(&'b self, jvm: &'b JVM<'a>) -> JVMResult<'a, ()> {
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
    fn init<'b>(&'b self, jvm: &'b JVM<'a>) -> JVMResult<'a, ()>
    where
        'a: 'b,
    {
        let mut const_pool = self.const_pool.write();
        // Preparation
        for field in self.fields.values() {
            if let Some(index) = field.const_value_index {
                match field.nat.typ {
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
                        if name.0 == "java/lang/String" {
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
        if let Some(method) = self.methods.get(&MethodNaT {
            name: jvm.intern_str("<clinit>"),
            typ: &MethodDescriptor(vec![], None),
        }) {
            crate::interp::invoke_initializer(jvm, method)?;
        }

        Ok(())
    }
}

impl<'a> std::fmt::Debug for Class<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}
