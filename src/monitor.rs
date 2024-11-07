use std::{
    mem::size_of,
    sync::atomic::{AtomicU32, AtomicUsize, Ordering::*},
};

/// Reentrant futex embedded in every object.
///
/// This type will never actually be instantiated, as it has padding
/// we don't want. Rather, all operations on it take a pointer and
/// don't interact with padding.
/// Zeroed memory can represent an unlocked monitor.
#[repr(C)]
pub(crate) struct Monitor {
    // Custom thread id
    locked_by: AtomicUsize,
    /// How many times it is currently locked.
    /// This gets waited on.
    /// Overflow is handled by perma-locking the object.
    count: AtomicU32,
}

pub const SIZE: usize = size_of::<usize>() + size_of::<u32>();
pub const ALIGN: usize = align_of::<usize>();

const NO_THREAD: usize = 0;

// Custom thread id, never NO_THREAD
fn thread_id() -> usize {
    thread_local!(static KEY: u8 = const {0});
    KEY.with(|x| x as *const _ as usize)
}

#[inline]
fn locked_by(monitor: *const Monitor) -> &'static AtomicUsize {
    unsafe {
        let id = &raw const (*monitor).locked_by;
        id.as_ref_unchecked()
    }
}

#[inline]
fn count(monitor: *const Monitor) -> &'static AtomicU32 {
    unsafe {
        let ptr = &raw const (*monitor).count;
        ptr.as_ref_unchecked()
    }
}

pub fn lock(monitor: *const Monitor) {
    loop {
        match count(monitor).compare_exchange(0, 1, Acquire, Relaxed) {
            Ok(_) => {
                // Previous locked_by value was NO_THREAD
                locked_by(monitor).store(thread_id(), Release);
                return;
            }
            Err(current) => {
                if locked_by(monitor).load(Relaxed) == thread_id() {
                    // If we reach Count::MAX, we never unlock the object again.
                    count(monitor).store(current.saturating_add(1), Relaxed);
                    return;
                } else {
                    atomic_wait::wait(count(monitor), current);
                }
            }
        }
    }
}

/// Must be paired with a lock() on the same thread.
pub fn unlock(monitor: *const Monitor) {
    let current = count(monitor).load(Relaxed);
    if current == u32::MAX {
        return;
    }
    locked_by(monitor).store(NO_THREAD, Release);
    count(monitor).store(current - 1, Release);
    if current == 1 {
        // TODO: Only notify when contended
        // (store contended status in count or in lsb of locked_by?)
        notify_one(monitor);
    }
}

pub fn notify_one(monitor: *const Monitor) {
    atomic_wait::wake_one(count(monitor));
}

pub fn notify_all(monitor: *const Monitor) {
    atomic_wait::wake_all(count(monitor));
}

#[cfg(test)]
mod test {
    use std::{
        alloc::{alloc_zeroed, dealloc, Layout},
        thread::scope,
    };

    use super::*;

    struct Wrapper(*const Monitor);

    impl Wrapper {
        fn new() -> Self {
            Self(
                unsafe { alloc_zeroed(Layout::from_size_align(SIZE, ALIGN).unwrap()) }
                    as *const Monitor,
            )
        }
    }

    impl Drop for Wrapper {
        fn drop(&mut self) {
            unsafe {
                dealloc(
                    self.0 as *mut u8,
                    Layout::from_size_align(SIZE, ALIGN).unwrap(),
                );
            }
        }
    }

    unsafe impl Sync for Wrapper {}

    #[test]
    fn monitor_reentrancy() {
        let monitor = Wrapper::new();
        lock(monitor.0);
        lock(monitor.0);
        unlock(monitor.0);
        unlock(monitor.0);
        assert_eq!(count(monitor.0).load(SeqCst), 0);
    }

    #[test]
    fn monitor_contention() {
        let threads = 16;
        let per_thread = 1000;

        let monitor = Wrapper::new();
        let sum = AtomicU32::new(0);

        scope(|scope| {
            for _ in 0..threads {
                scope.spawn(|| {
                    for _ in 0..per_thread {
                        let _ = &monitor;
                        lock(monitor.0);
                        lock(monitor.0);
                        sum.store(sum.load(Relaxed) + 1, Relaxed);
                        unlock(monitor.0);
                        unlock(monitor.0);
                    }
                });
            }
        });

        lock(monitor.0);
        assert_eq!(sum.load(Relaxed), threads * per_thread);
    }
}
