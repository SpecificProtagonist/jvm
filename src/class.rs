use anyhow::{anyhow, Result};
use std::{alloc::Layout, collections::HashMap};

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
    pub(crate) methods: HashMap<MethodNaT<'a, 'a>, &'a MethodMeta<'a>>,
    pub(crate) static_storage: FieldStorage,
    pub(crate) object_layout: Layout,
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
    pub nat: MethodNaT<'a, 'a>,
    pub access_flags: AccessFlags,
    pub code: Option<Code>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldNaT<'a> {
    pub(crate) name: IntStr<'a>,
    pub(crate) typ: &'a Typ<'a>,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct MethodNaT<'a, 'b: 'a> {
    // TODO: remove 'b
    pub(crate) name: IntStr<'a>,
    pub(crate) typ: &'b MethodDescriptor<'a>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MethodDescriptor<'a>(pub Vec<&'a Typ<'a>>, pub Option<&'a Typ<'a>>);
