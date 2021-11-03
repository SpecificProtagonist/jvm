use std::alloc::Layout;

use crate::{class::Class, datastructures::Id, field_storage::FieldStorage, Typ};

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
    pub fn typ(&self) -> Id<Typ> {
        assert_eq!(std::mem::size_of::<Id<Class>>(), 4);
        unsafe { std::mem::transmute(self.data.read_u32(0)) }
    }
}

impl std::fmt::Debug for ObjectData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Object") // TODO: class name, data etc
    }
}

pub fn min_object_layout() -> Layout {
    Layout::new::<Id<Typ>>()
}
