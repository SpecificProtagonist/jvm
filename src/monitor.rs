use std::{
    mem::size_of,
    sync::atomic::{AtomicU32, AtomicUsize, Ordering::*},
};

/// Reentrant futex embedded in every object.
/// Overflowing the lock count permanently locks the object.
///
/// This type will never actually be instantiated, as it has padding
/// we don't want. Rather, all operations on it take a pointer and
/// don't interact with padding.
/// Zeroed memory can represent an unlocked monitor.
#[repr(C)]
pub(crate) struct Monitor {
    // Custom thread id
    locked_by: AtomicUsize,
    /// Upper 31 bits: How many times it is currently locked.
    /// Lowermost bit: Whether the lock was contended.
    /// This gets waited on.
    /// Overflow is handled by perma-locking the object.
    count: AtomicU32,
}

pub const SIZE: usize = size_of::<usize>() + size_of::<u32>();
pub const ALIGN: usize = align_of::<usize>();

const CONTENDED: u32 = 1;
const STEP: u32 = 2;
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

#[inline]
pub fn lock(monitor: *const Monitor) {
    match count(monitor).compare_exchange(0, STEP, Acquire, Relaxed) {
        Ok(_) => {
            // Locked without contention
            locked_by(monitor).store(thread_id(), Release);
        }
        Err(loaded) => lock_locked(monitor, loaded),
    }
}

#[cold]
fn lock_locked(monitor: *const Monitor, loaded: u32) {
    // Already locked, checked if by the same thread
    if locked_by(monitor).load(Acquire) == thread_id() {
        // If we reach u32::MAX, we never unlock the object again.
        count(monitor).store(loaded.saturating_add(STEP), Relaxed);
        return;
    }
    loop {
        match count(monitor).compare_exchange(0, STEP + CONTENDED, Acquire, Relaxed) {
            Ok(_) => {
                // Locked; contention flag set because it got cleared during the previous unlock
                // Previous locked_by value was NO_THREAD
                locked_by(monitor).store(thread_id(), Release);
                return;
            }
            Err(loaded) => {
                atomic_wait::wait(count(monitor), loaded | CONTENDED);
            }
        }
    }
}

pub struct NotLockedByThread;

pub fn unlock(monitor: *const Monitor) -> Result<(), NotLockedByThread> {
    if locked_by(monitor).load(Relaxed) != thread_id() {
        return Err(NotLockedByThread);
    }
    let current = count(monitor).load(Relaxed);
    if current == u32::MAX {
        return Ok(());
    }

    // Clear the CONTENDED flag & notify waiter only if releasing the lock
    if current < 2 * STEP {
        locked_by(monitor).store(NO_THREAD, Release);
        if count(monitor).swap(0, AcqRel) == STEP + CONTENDED {
            notify_one(monitor);
        }
    } else {
        count(monitor).swap(current - STEP, AcqRel);
    }
    Ok(())
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
