use std::{hash::Hash, marker::PhantomData};

use bimap::BiMap;

#[derive(Eq, Hash)]
pub struct Id<T>(pub u32, PhantomData<T>);

impl<T> Id<T> {
    pub const fn with_index(id: u32) -> Self {
        Id(id, PhantomData)
    }
}

impl<T> Copy for Id<T> {}
impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> std::fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

// TODO: replace with typed-arena
pub struct Interner<T: Eq + Hash>(BiMap<T, Id<T>>);

impl<T: Eq + Hash> Interner<T> {
    pub fn intern(&mut self, item: T) -> Id<T> {
        if let Some(interned) = self.0.get_by_left(&item) {
            *interned
        } else {
            let interned = Id(self.0.len() as u32, PhantomData::default());
            self.0.insert(item, interned);
            interned
        }
    }

    pub fn get(&self, interned: Id<T>) -> &T {
        self.0
            .get_by_right(&interned)
            .expect("Tried to retrieve item from different from interner")
    }
}

impl Interner<String> {
    pub fn intern_str(&mut self, string: &str) -> Id<String> {
        if let Some(id) = self.0.get_by_left(string) {
            *id
        } else {
            self.intern(string.into())
        }
    }
}

impl<T: Eq + Hash> Default for Interner<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}
