use anyhow::{anyhow, Result};
use std::{alloc::Layout, cell::Cell, collections::HashMap, sync::Mutex, thread::ThreadId};

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
    pub(crate) fields: HashMap<FieldNaT<'a>, FieldMeta<'a>>,
    pub(crate) methods: HashMap<MethodNaT<'a>, &'a MethodMeta<'a>>,
    pub(crate) static_storage: FieldStorage,
    pub(crate) object_layout: Layout,
    /// Thread doing the initialization. If None, this class is already initialized.
    pub(crate) initializer: Cell<Option<ThreadId>>,
    pub(crate) init_lock: Mutex<()>,
}

impl<'a> Class<'a> {
    pub(crate) fn resolve_field(&'a self, field: FieldNaT<'a>) -> Result<Field<'a>> {
        if let Some(meta) = self.fields.get(&field) {
            Ok(Field { class: self, meta })
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
    ) -> Option<Method<'a>> {
        self.methods
            .get(&MethodNaT { name, typ })
            .map(|data| Method { class: self, data })
    }

    pub fn field(&'a self, name: IntStr<'a>, typ: &'a Typ<'a>) -> Option<Field<'a>> {
        self.fields
            .get(&FieldNaT { name, typ })
            .map(|meta| Field { class: self, meta })
    }
}

impl<'a> std::fmt::Debug for Class<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Class({})", self.name)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Field<'a> {
    pub(crate) class: &'a Class<'a>,
    pub(crate) meta: &'a FieldMeta<'a>,
}

impl<'a> Eq for &'a FieldMeta<'a> {}
impl<'a> PartialEq for &'a FieldMeta<'a> {
    fn eq(&self, other: &Self) -> bool {
        *self as *const FieldMeta == *other as *const FieldMeta
    }
}

pub(crate) struct FieldMeta<'a> {
    pub name: IntStr<'a>,
    pub access_flags: AccessFlags,
    pub descriptor: &'a Typ<'a>,
    // Java has no multiple inheritance for fields, therefore each field can be at a set position
    pub byte_offset: u32,
    pub const_value_index: Option<u16>,
}

#[derive(Clone, Copy)]
pub struct Method<'a> {
    pub(crate) class: &'a Class<'a>,
    pub(crate) data: &'a MethodMeta<'a>,
}

impl<'a> Eq for &'a Method<'a> {}
impl<'a> PartialEq for &'a Method<'a> {
    fn eq(&self, other: &Self) -> bool {
        *self as *const Method == *other as *const Method
    }
}

pub(crate) struct MethodMeta<'a> {
    pub nat: MethodNaT<'a>,
    pub access_flags: AccessFlags,
    pub code: Option<Code>,
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
