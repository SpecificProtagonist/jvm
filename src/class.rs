use anyhow::{anyhow, bail, Result};
use std::{
    cell::Cell,
    collections::HashMap,
    fmt::Debug,
    sync::{Condvar, Mutex},
    thread::ThreadId,
};

use crate::{const_pool::ConstPool, field_storage::FieldStorage, AccessFlags, IntStr, Typ, JVM};

#[derive(Debug)]
pub enum RefType<'a> {
    Class(Class<'a>),
    Array {
        base: Typ<'a>,
        super_class: &'a RefType<'a>,
    },
}

impl<'a> RefType<'a> {
    pub fn super_class(&self) -> Option<&'a RefType<'a>> {
        match self {
            Self::Class(class) => class.super_class,
            Self::Array { super_class, .. } => Some(super_class),
        }
    }

    pub fn assignable_to(&'a self, other: &'a RefType<'a>) -> bool {
        (self == other)
            || self
                .super_class()
                .map(|sc| (sc == other) || sc.assignable_to(other))
                .unwrap_or(false)
    }

    pub(crate) fn name(&'a self, jvm: &JVM<'a>) -> IntStr<'a> {
        match self {
            Self::Array { base, .. } => jvm.intern_str(&format!("[{}", base)),
            Self::Class(class) => class.name,
        }
    }

    pub fn method<'b>(
        &'a self,
        name: IntStr<'a>,
        typ: &'b MethodDescriptor<'a>,
    ) -> Option<&'a Method<'a>> {
        match self {
            Self::Class(class) => class.methods.get(&MethodNaT { name, typ }).map(|m| *m),
            Self::Array { super_class, .. } => super_class.method(name, typ),
        }
    }

    pub(crate) fn resolve_field(&'a self, field: FieldNaT<'a>) -> Result<&'a Field<'a>> {
        if let RefType::Class(class) = self {
            if let Some(field) = class.fields.get(&field) {
                Ok(field)
            } else {
                let super_class = self
                    .super_class()
                    .ok_or_else(|| anyhow!("Failed to resolve field {}", field.name))?;
                super_class.resolve_field(field)
            }
        } else {
            bail!("Tried to resolve field of array")
        }
    }
}

impl<'a> Eq for &'a RefType<'a> {}
impl<'a> PartialEq for &'a RefType<'a> {
    fn eq(&self, other: &Self) -> bool {
        *self as *const RefType == *other as *const RefType
    }
}

pub struct Class<'a> {
    pub(crate) name: IntStr<'a>,
    pub(crate) super_class: Option<&'a RefType<'a>>,
    #[allow(unused)]
    pub(crate) version: (u16, u16),
    pub(crate) const_pool: ConstPool<'a>,
    pub(crate) access_flags: AccessFlags,
    #[allow(unused)]
    pub(crate) interfaces: Vec<IntStr<'a>>,
    /// As far as I can tell, JVM supports field overloading
    pub(crate) fields: HashMap<FieldNaT<'a>, Field<'a>>,
    pub(crate) methods: HashMap<MethodNaT<'a>, &'a Method<'a>>,
    pub(crate) static_storage: FieldStorage,
    pub(crate) object_size: usize,
    pub(crate) init: Mutex<ClassInitState>,
    pub(crate) init_waiter: Condvar,
}

pub(crate) enum ClassInitState {
    Uninit,
    InProgress(ThreadId),
    Done,
    Error,
}

impl<'a> Class<'a> {
    pub fn field(&'a self, name: IntStr<'a>, typ: Typ<'a>) -> Option<&'a Field<'a>> {
        self.fields.get(&FieldNaT { name, typ })
    }

    pub fn subclass_of(&'a self, other: &'a RefType<'a>) -> bool {
        self.super_class
            .map(|sc| (sc == other) || sc.assignable_to(other))
            .unwrap_or(false)
    }

    pub(crate) fn dummy_class() -> Self {
        Class {
            version: (0, 0),
            const_pool: ConstPool { items: Vec::new() },
            access_flags: AccessFlags::empty(),
            name: IntStr(""),
            super_class: None,
            interfaces: Default::default(),
            static_storage: FieldStorage::new(0),
            object_size: 0,
            fields: Default::default(),
            methods: Default::default(),
            init: ClassInitState::Uninit.into(),
            init_waiter: Condvar::new(),
        }
    }

    /// Ensures this class is initialized. This includes linking, verification, preparation & resolution
    /// May only be called by the instuctions new, getstatic, putstatic or invokestatic or by being the initial class
    pub(crate) fn ensure_init(&self, jvm: &JVM<'a>) -> Result<()> {
        let mut guard = self.init.lock().unwrap();
        match *guard {
            ClassInitState::Done => Ok(()),
            ClassInitState::Error => Err(anyhow!("NoClassDefFoundError")),
            ClassInitState::InProgress(thread) => {
                if thread == std::thread::current().id() {
                    Ok(())
                } else {
                    loop {
                        guard = self.init_waiter.wait(guard).unwrap();
                        match *guard {
                            ClassInitState::Done => return Ok(()),
                            ClassInitState::Error => return Err(anyhow!("NoClassDefFoundError")),
                            _ => (),
                        }
                    }
                }
            }
            ClassInitState::Uninit => {
                *guard = ClassInitState::InProgress(std::thread::current().id());
                drop(guard);

                let init_result = self.init(jvm);

                *self.init.lock().unwrap() = if init_result.is_ok() {
                    ClassInitState::Done
                } else {
                    ClassInitState::Error
                };

                self.init_waiter.notify_all();

                Ok(())
            }
        }
    }

    /// This may only be called from self.ensure_init
    fn init(&self, jvm: &JVM<'a>) -> Result<()> {
        // Preparation
        for field in self.fields.values() {
            if let Some(index) = field.const_value_index {
                match field.descriptor {
                    Typ::Bool | Typ::Byte => self
                        .static_storage
                        .write_i8(field.byte_offset, self.const_pool.get_int(index)? as i8)
                        .unwrap(),
                    Typ::Short | Typ::Char => self
                        .static_storage
                        .write_i16(field.byte_offset, self.const_pool.get_int(index)? as i16)
                        .unwrap(),
                    Typ::Int => self
                        .static_storage
                        .write_i32(field.byte_offset, self.const_pool.get_int(index)?)
                        .unwrap(),
                    Typ::Float => self
                        .static_storage
                        .write_f32(field.byte_offset, self.const_pool.get_float(index)?)
                        .unwrap(),
                    Typ::Double => self
                        .static_storage
                        .write_f64(field.byte_offset, self.const_pool.get_double(index)?)
                        .unwrap(),
                    Typ::Long => self
                        .static_storage
                        .write_i64(field.byte_offset, self.const_pool.get_long(index)?)
                        .unwrap(),
                    Typ::Ref(name) => {
                        if name.0 == "java/lang/String" {
                            todo!()
                        } else {
                            bail!("final static non-null object")
                        }
                    }
                }
            }
        }

        // Initialization
        if let Some(method) = self.methods.get(&MethodNaT {
            name: jvm.intern_str("<clinit>"),
            typ: &MethodDescriptor(vec![], None),
        }) {
            crate::interp::run(jvm, method, &[])?;
        }

        Ok(())
    }
}

impl<'a> Drop for Class<'a> {
    fn drop(&mut self) {
        // Safety: This is the only reference to this storage
        unsafe { self.static_storage.delete() };
    }
}

impl<'a> std::fmt::Debug for Class<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl<'a> Eq for &'a Field<'a> {}
impl<'a> PartialEq for &'a Field<'a> {
    fn eq(&self, other: &Self) -> bool {
        *self as *const Field == *other as *const Field
    }
}

#[derive(Debug)]
pub struct Field<'a> {
    pub(crate) name: IntStr<'a>,
    pub(crate) class: Cell<&'a Class<'a>>,
    pub(crate) access_flags: AccessFlags,
    pub(crate) descriptor: Typ<'a>,
    // Java has no multiple inheritance for fields, therefore each field can be at a set position
    pub(crate) byte_offset: u32,
    pub(crate) const_value_index: Option<u16>,
}

impl<'a> Eq for &'a Method<'a> {}
impl<'a> PartialEq for &'a Method<'a> {
    fn eq(&self, other: &Self) -> bool {
        *self as *const Method == *other as *const Method
    }
}

pub struct Method<'a> {
    pub(crate) nat: MethodNaT<'a>,
    pub(crate) access_flags: AccessFlags,
    pub(crate) class: Cell<&'a Class<'a>>,
    pub(crate) code: Option<Code>,
}

impl<'a> Debug for Method<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.nat)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldNaT<'a> {
    pub(crate) name: IntStr<'a>,
    pub(crate) typ: Typ<'a>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct MethodNaT<'a> {
    pub(crate) name: IntStr<'a>,
    pub(crate) typ: &'a MethodDescriptor<'a>,
}

impl<'a> std::fmt::Display for MethodNaT<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ret) = self.typ.1 {
            write!(f, "{}", ret)?;
        } else {
            write!(f, "void")?;
        }
        write!(f, " {}(", self.name)?;
        for arg in &self.typ.0 {
            write!(f, "{},", arg)?;
        }
        write!(f, ")")?;
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MethodDescriptor<'a>(pub Vec<Typ<'a>>, pub Option<Typ<'a>>);

pub(crate) struct Code {
    pub max_stack: u16,
    pub max_locals: u16,
    pub bytes: Vec<u8>,
}
