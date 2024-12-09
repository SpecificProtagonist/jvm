//! This module implements object identity hashcodes.
//! At the moment this is not needed – it could be derived from the address –
//! but will be necessary for a moving GC.
//! Alternatively the hash could be embedded in the object, but that would be wasteful
//! as most objects won't need an identity hash code.
use std::sync::{RwLock, RwLockWriteGuard};

use hashbrown::HashMap;

use crate::{default, object::Object};

type Map = HashMap<Object, i32>;

/// Stores hash codes by address. TODO: ake this more parallelizable in the future.
#[derive(Default)]
pub struct IdentityHashes {
    map: RwLock<Map>,
}

impl IdentityHashes {
    /// TODO: Don't lookup if the object hasn't been moved since the first time the
    /// identity hash code has been read.
    pub fn get_identity_hash(&self, obj: Object) -> i32 {
        let map = self.map.read().unwrap();
        if let Some(&hash) = map.get(&obj) {
            hash
        } else {
            drop(map);
            let hash = (obj.0.get() >> 3) as i32;
            self.map.write().unwrap().insert(obj, hash);
            hash
        }
    }

    #[allow(unused)]
    pub fn lock_for_gc(&self) -> IdentityHashesGcLock<'_> {
        IdentityHashesGcLock {
            lock: self.map.write().unwrap(),
            new: default(),
        }
    }
}

pub struct IdentityHashesGcLock<'a> {
    lock: RwLockWriteGuard<'a, Map>,
    new: Map,
}

impl IdentityHashesGcLock<'_> {
    #[allow(unused)]
    pub fn move_object(&mut self, old: Object, new: Object) {
        if let Some(&hash) = self.lock.get(&old) {
            self.new.insert(new, hash);
        }
    }
}

impl Drop for IdentityHashesGcLock<'_> {
    fn drop(&mut self) {
        *self.lock = std::mem::take(&mut self.new);
    }
}
