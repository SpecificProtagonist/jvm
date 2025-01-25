use std::cell::UnsafeCell;

use crate::sync::*;
use thread::Thread;

pub struct ThreadQueue {
    head: *mut ThreadData,
    tail: *mut ThreadData,
}

impl Default for ThreadQueue {
    fn default() -> Self {
        Self {
            head: std::ptr::null_mut(),
            tail: std::ptr::null_mut(),
        }
    }
}

/// Intrusive linked list used to park threads.
/// As each thread can only park on one queue at each point in time,
/// we can allocate it in thread local data.
pub struct ThreadData {
    next: *mut ThreadData,
    thread: Thread,
}

thread_local! {
    static THREAD_DATA: UnsafeCell<ThreadData> = UnsafeCell::new(ThreadData{
        thread: thread::current(),
        next: std::ptr::null_mut()
    });
}

impl ThreadQueue {
    pub fn push(&mut self) {
        THREAD_DATA.with(|current| {
            // TODO SAFETY doc
            unsafe {
                let current = (current as *const _ as *mut ThreadData).as_mut_unchecked();
                current.next = std::ptr::null_mut();
                match self.tail.as_mut() {
                    Some(tail) => {
                        tail.next = current as _;
                    }
                    None => {
                        self.head = current as _;
                    }
                }
                self.tail = current as _;
            }
        });
    }

    /// Pops and wakes one parked thread.
    /// Returns true if the queue is now empty.
    pub fn wake_one(&mut self, pre_wake: impl FnOnce(bool)) {
        THREAD_DATA.with(|current| {
            // TODO SAFETY doc
            unsafe {
                let old_head = self.head.as_ref().expect("popped empty thread queue");
                self.head = old_head.next;
                let empty = self.head.is_null();
                pre_wake(empty);
                old_head.thread.unpark();
            }
        })
    }
}
