use core::cell::{SyncUnsafeCell, UnsafeCell};

use super::*;
use crate::miri_println;

/// Queue of parked threads.
///
/// This is implemented as a intrusive linked list.
/// As each thread can only park on one queue at each point in time,
/// we can allocate it in thread local data.
pub(super) struct ThreadQueue {
    head: *const ThreadData,
    /// Only valid when `head` is not null.
    tail: *const ThreadData,
}

unsafe impl Sync for ThreadQueue {}

impl Default for ThreadQueue {
    fn default() -> Self {
        Self {
            head: core::ptr::null_mut(),
            tail: core::ptr::null_mut(),
        }
    }
}

/// List entry
struct ThreadData {
    thread: Thread,
    /// Indicates that the thread was woken deliberately (as opposed to a spurious wakeup).
    woken: AtomicBool,
    next: UnsafeCell<*const ThreadData>,
}

impl ThreadQueue {
    /// Parks the current thread on this queue.
    ///
    /// SAFETY: Must have exclusive access to queue until `pre_park` is called.
    pub(super) unsafe fn park(queue: &SyncUnsafeCell<ThreadQueue>, pre_park: impl FnOnce()) {
        thread_local! {
            static THREAD_DATA: ThreadData = ThreadData{
                thread: thread::current(),
                woken: AtomicBool::new(false),
                next: UnsafeCell::new(core::ptr::null_mut()),
            };
        }

        THREAD_DATA.with(|current| {
            {
                // SAFETY: Not in any queue
                unsafe { *current.next.get() = core::ptr::null_mut() }
                // SAFETY: Function precondition
                let queue = unsafe { &mut *queue.get() };
                if queue.head.is_null() {
                    queue.head = current as _;
                } else {
                    // SAFETY: We have exclusive access to the queue
                    unsafe { *queue.tail.as_ref_unchecked().next.get() = current as _ };
                }
                queue.tail = current as _;
            }

            // We no longer hold the &mut to the queue here.
            pre_park();

            // Consume unpark token if thread has been unparked between bucket unlock
            // and now, otherwise wait for unpark token.
            // To guard against wakeups not caused by `Self::wake_one`, we use out own token.
            while !current.woken.swap(false, Acquire) {
                miri_println!("[{:?}] calling park", thread::current().id());
                thread::park();
            }
            miri_println!("[{:?}] woken", thread::current().id());
        });
    }

    /// Pops and wakes one parked thread.
    /// `on_cleared` gets called if the queue will now be empty.
    pub(super) fn wake_one(&mut self, on_cleared: impl FnOnce()) {
        // SAFETY: All pointers are either null or point to a valid list entry
        let thread_to_wake = unsafe { self.head.as_ref().expect("popped empty thread queue") };
        // SAFETY: `next` only gets written to in `Self::park`, which requires a mutable ref
        self.head = unsafe { *thread_to_wake.next.get() };
        if self.head.is_null() {
            on_cleared();
        }
        thread_to_wake.woken.store(true, Release);
        miri_println!("waking {:?}", thread_to_wake.thread);
        thread_to_wake.thread.unpark();
    }
}

#[cfg(test)]
mod tests {
    use core::{
        cell::SyncUnsafeCell,
        sync::atomic::{AtomicI32, Ordering},
        time::Duration,
    };

    use super::ThreadQueue;

    #[test]
    fn thread_queue_wakeup() {
        let queue = SyncUnsafeCell::new(ThreadQueue::default());
        let state = AtomicI32::new(0);
        std::thread::scope(|s| {
            let thread = s.spawn(|| {
                unsafe {
                    ThreadQueue::park(&queue, || {
                        state.store(1, Ordering::SeqCst);
                    });
                    assert_eq!(state.load(Ordering::SeqCst), 2);
                };
            });

            while state.load(Ordering::SeqCst) == 0 {}

            // Test that waking the thread doesn't do anything
            thread.thread().unpark();

            std::thread::sleep(Duration::from_millis(100));

            unsafe { &mut *queue.get() }.wake_one(|| {
                state.store(2, Ordering::SeqCst);
            });
        });
    }
}
