use std::hash::Hash;

use bimap::BiMap;

use crate::Typ;

#[derive(Eq, PartialEq, Hash, Clone, Copy)]
pub(crate) struct TypId(u32);

impl std::fmt::Debug for TypId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

pub(crate) struct TypInterner<'a>(BiMap<Typ<'a>, TypId>);

impl<'a> TypInterner<'a> {
    pub(crate) fn intern(&mut self, item: Typ<'a>) -> TypId {
        if let Some(interned) = self.0.get_by_left(&item) {
            *interned
        } else {
            let interned = TypId(self.0.len() as u32);
            self.0.insert(item, interned);
            interned
        }
    }

    pub(crate) fn get(&self, interned: TypId) -> &Typ<'a> {
        self.0
            .get_by_right(&interned)
            .expect("Tried to retrieve item from different from interner")
    }
}

impl<'a> Default for TypInterner<'a> {
    fn default() -> Self {
        Self(Default::default())
    }
}
