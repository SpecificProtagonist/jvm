use std::alloc::Layout;

use crate::{class::Class, field_storage::FieldStorage};

pub type Object = &'static ObjectData;

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        *self as *const ObjectData == *other as *const ObjectData
    }
}

pub struct ObjectData {
    pub data: FieldStorage,
}

impl ObjectData {
    // TODO: arrays
    pub(crate) fn class(&self) -> &Class {
        unsafe { std::mem::transmute(self.data.read_usize(0)) }
    }
}

impl std::fmt::Debug for ObjectData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.class().name) // TODO: data
    }
}

pub fn min_object_layout() -> Layout {
    Layout::new::<Class>()
}
