use std::alloc::Layout;

use crate::{
    const_pool::ConstPool, field_storage::FieldStorage, type_interner::TypId, AccessFlags, Code,
    IntStr, MethodDescriptor,
};

impl<'a> PartialEq for &'a Class<'a> {
    fn eq(&self, other: &Self) -> bool {
        *self as *const Class == *other as *const Class
    }
}

pub(crate) struct Class<'a> {
    pub name: IntStr<'a>,
    pub super_class: Option<IntStr<'a>>,
    #[allow(unused)]
    pub version: (u16, u16),
    pub const_pool: ConstPool<'a>,
    pub access_flags: AccessFlags,
    #[allow(unused)]
    pub interfaces: Vec<IntStr<'a>>,
    pub methods: Vec<MethodMeta<'a>>,
    pub fields: Vec<FieldMeta<'a>>,
    pub static_storage: FieldStorage,
    pub object_layout: Layout,
}

#[derive(Clone, Copy)]
pub(crate) struct Field<'a> {
    pub class: &'a Class<'a>,
    pub meta: &'a FieldMeta<'a>,
}

pub(crate) struct FieldMeta<'a> {
    pub name: IntStr<'a>,
    pub access_flags: AccessFlags,
    pub descriptor: TypId,
    // Java has no multiple inheritance for fields, therefore each field can be at a set position
    pub byte_offset: u32,
}

#[derive(Clone, Copy)]
pub(crate) struct Method<'a> {
    pub class: &'a Class<'a>,
    pub meta: &'a MethodMeta<'a>,
}

pub(crate) struct MethodMeta<'a> {
    pub name: IntStr<'a>,
    pub access_flags: AccessFlags,
    pub descriptor: MethodDescriptor,
    pub code: Option<Code>,
}
