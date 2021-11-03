use std::alloc::Layout;

use crate::{
    datastructures::Id, field_storage::FieldStorage, AccessFlags, Code, ConstPool,
    MethodDescriptor, Typ,
};

pub type Class = &'static ClassMeta;
pub struct ClassMeta {
    pub name: Id<String>,
    pub super_class: Option<Id<String>>,
    #[allow(unused)]
    pub version: (u16, u16),
    pub const_pool: ConstPool,
    pub access_flags: AccessFlags,
    pub interfaces: Vec<Id<String>>,
    pub methods: Vec<MethodMeta>,
    pub fields: Vec<FieldMeta>,
    pub static_storage: FieldStorage,
    pub object_layout: Layout,
}

#[derive(Clone, Copy)]
pub struct Field {
    pub class: Class,
    pub meta: &'static FieldMeta,
}

pub struct FieldMeta {
    pub name: Id<String>,
    pub access_flags: AccessFlags,
    pub descriptor: Id<Typ>,
    // Java has no multiple inheritance for fields, therefore each field can be at a set position
    pub byte_offset: u32,
}

#[derive(Clone, Copy)]
pub struct Method {
    pub class: Class,
    pub meta: &'static MethodMeta,
}

pub struct MethodMeta {
    pub name: Id<String>,
    pub access_flags: AccessFlags,
    pub descriptor: MethodDescriptor,
    pub code: Option<Code>,
}
