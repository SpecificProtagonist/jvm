#![allow(unused)]
mod mutex;
mod thread_parking_lot;
mod thread_queue;

use std::{
    cell::RefCell,
    collections::{btree_map::*, BTreeMap},
    sync::atomic::{AtomicPtr, AtomicU32, Ordering::*},
};

use crate::object::{Object, MONITOR_LOCKED_BIT};

#[derive(Default)]
struct Monitor {
    waiter: AtomicU32,
}

thread_local! {
    static LOCK_COUNT: RefCell<BTreeMap<Object, u32>> = const {RefCell::new(BTreeMap::new()) };
}

// pub fn enter(obj: Object) {
//     let mark_ref = obj.mark_word();
//     let mark = mark_ref.load(Relaxed);
//     let unlocked = mark & !MONITOR_LOCKED_BIT;
//     let locked = mark & MONITOR_LOCKED_BIT;
//     LOCK_COUNT.with_borrow_mut(|map| match map.entry(obj) {
//         Entry::Occupied(mut count) => {
//             // Already locked
//             *count.get_mut() = count.get_mut().saturating_add(1);
//         }
//         Entry::Vacant(vacant_entry) => {
//             match mark_ref.compare_exchange(unlocked, locked, Acquire, Relaxed) {
//                 Ok(_) => {
//                     // Sucessfully locked object
//                     vacant_entry.insert(1);
//                 }
//                 Err(mark) => enter_locked_by_other_thread(obj),
//             }
//         }
//     });
// }

// fn enter_locked_by_other_thread(obj: Object) {
//     let monitor = self.map.entry(obj).or_default();
//     let wait_value = monitor.waiter.load(Relaxed);
//     // TODO: Set contended bit
//     atomic_wait::wait(&monitor.waiter, wait_value);
// }
