use anyhow::{anyhow, Result};
use std::{
    alloc::Layout, cell::Cell, collections::HashMap, fmt::Debug, sync::Mutex, thread::ThreadId,
};

use crate::{const_pool::ConstPool, field_storage::FieldStorage, AccessFlags, Code, IntStr, Typ};

impl<'a> Eq for &'a Class<'a> {}
impl<'a> PartialEq for &'a Class<'a> {
    fn eq(&self, other: &Self) -> bool {
        *self as *const Class == *other as *const Class
    }
}

pub struct Class<'a> {
    pub(crate) name: IntStr<'a>,
    pub(crate) super_class: Option<&'a Class<'a>>,
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
    pub(crate) object_layout: Layout,
    /// Thread doing the initialization. If None, this class is already initialized.
    pub(crate) initializer: Cell<Option<ThreadId>>,
    pub(crate) init_lock: Mutex<()>,
}

impl<'a> Class<'a> {
    pub(crate) fn resolve_field(&'a self, field: FieldNaT<'a>) -> Result<&'a Field<'a>> {
        if let Some(field) = self.fields.get(&field) {
            Ok(field)
        } else {
            let super_class = self
                .super_class
                .ok_or_else(|| anyhow!("Failed to resolve field {}", field.name))?;
            super_class.resolve_field(field)
        }
    }

    pub fn method<'b>(
        &'a self,
        name: IntStr<'a>,
        typ: &'b MethodDescriptor<'a>,
    ) -> Option<&'a Method<'a>> {
        self.methods.get(&MethodNaT { name, typ }).map(|m| *m)
    }

    pub fn field(&'a self, name: IntStr<'a>, typ: &'a Typ<'a>) -> Option<&'a Field<'a>> {
        self.fields.get(&FieldNaT { name, typ })
    }

    pub(crate) fn dummy_class() -> Self {
        Class {
            version: (0, 0),
            const_pool: ConstPool { items: Vec::new() },
            access_flags: AccessFlags::empty(),
            name: IntStr(""),
            super_class: None,
            interfaces: Default::default(),
            static_storage: FieldStorage::new(Layout::new::<()>()),
            object_layout: Layout::new::<()>(),
            fields: Default::default(),
            methods: Default::default(),
            initializer: Default::default(),
            init_lock: Default::default(),
        }
    }
}

impl<'a> std::fmt::Debug for Class<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Class({})", self.name)
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
    pub(crate) descriptor: &'a Typ<'a>,
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
    pub(crate) typ: &'a Typ<'a>,
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
pub struct MethodDescriptor<'a>(pub Vec<&'a Typ<'a>>, pub Option<&'a Typ<'a>>);
