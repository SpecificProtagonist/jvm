use std::{
    cell::Cell,
    mem::size_of,
    sync::{
        atomic::{AtomicU32, Ordering::*},
        Mutex,
    },
};

#[cfg(all(miri, feature = "miri_monitor_debug_print"))]
extern "Rust" {
    fn miri_write_to_stdout(bytes: &[u8]);
}

macro_rules! miri_println {
    ($fmt:literal $(, $exprs:expr)*) => {
        #[cfg(all(miri, feature = "miri_monitor_debug_print"))]
        unsafe {
            let message = format!($fmt, $($exprs),*) + "\n";
            miri_write_to_stdout(message.as_bytes());
        }
    };
}

/// Reëntrant futex embedded in every object.
/// Overflowing the lock count permanently locks the object.
///
/// This type will never actually be instantiated, as it has padding
/// we don't want. Rather, all operations on it take a pointer and
/// don't interact with padding.
/// Zeroed memory can represent an unlocked monitor.
// TODO: This is outdated, there's no padding anymore. Consider moving back to references.
#[repr(C)]
pub(crate) struct Monitor {
    /// Custom thread id
    locked_by: AtomicU32,
    /// This gets waited on.
    /// Upper 31 bits: How many times it is currently locked.
    /// Lowermost bit: Whether the lock was contended.
    /// Overflow is handled by perma-locking the object.
    count: AtomicU32,
}

impl Monitor {
    pub const SIZE: usize = size_of::<u32>() + size_of::<u32>();
}

const CONTENDED: u32 = 1;
const INCREMENT: u32 = 2;

const NO_THREAD: u32 = 0;

struct ThreadIdAlloc {
    previous: u32,
    freelist: Vec<u32>,
}

static THREAD_ID_ALLOC: Mutex<ThreadIdAlloc> = Mutex::new(ThreadIdAlloc {
    previous: NO_THREAD,
    freelist: Vec::new(),
});

thread_local!(static THREAD_ID: SmallThreadId = const { SmallThreadId(Cell::new(NO_THREAD)) });

// Custom thread id, never NO_THREAD after `init_thread_id` was called
struct SmallThreadId(Cell<u32>);

impl Drop for SmallThreadId {
    fn drop(&mut self) {
        let id = self.0.get();
        if id != NO_THREAD {
            THREAD_ID_ALLOC.lock().unwrap().freelist.push(id);
        }
    }
}

fn thread_id() -> u32 {
    THREAD_ID.with(|id| id.0.get())
}

pub(crate) fn ensure_thread_id_initialized() {
    if thread_id() != NO_THREAD {
        return;
    }
    let mut alloc = THREAD_ID_ALLOC.lock().unwrap();
    let id = alloc.freelist.pop().unwrap_or_else(|| {
        let id = alloc.previous.checked_add(1).expect("Too many threads");
        alloc.previous = id;
        id
    });
    THREAD_ID.with(|key| key.0.set(id))
}

#[inline]
fn locked_by(monitor: *const Monitor) -> &'static AtomicU32 {
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
pub fn enter(monitor: *const Monitor) {
    ensure_thread_id_initialized();
    match count(monitor).compare_exchange(0, INCREMENT, Acquire, Relaxed) {
        Ok(_) => {
            miri_println!("[{}] {} locked uncontended", thread_id(), monitor);
            locked_by(monitor).store(thread_id(), Release);
        }
        Err(loaded) => enter_locked(monitor, loaded),
    }
}

#[cold]
fn enter_locked(monitor: *const Monitor, loaded: u32) {
    // Already locked, checked if by the same thread
    if locked_by(monitor).load(Acquire) == thread_id() {
        // If we reach u32::MAX, we never unlock the object again.
        let new_count = loaded.saturating_add(INCREMENT);
        // Try to increase count, but don't overwrite contended flag
        if count(monitor).swap(new_count, Relaxed) & CONTENDED != 0 {
            count(monitor).store(new_count | CONTENDED, Relaxed);
        }
        miri_println!("[{}] {} locked recursive", thread_id(), monitor);
        return;
    }
    loop {
        // TODO: Do a load first to not claim ownership of a cache line
        match count(monitor).compare_exchange(0, INCREMENT + CONTENDED, Acquire, Relaxed) {
            Ok(_) => {
                // Locked; contention flag set because it got cleared during the previous unlock
                // Previous locked_by value was NO_THREAD
                miri_println!("[{}] {} locked contended", thread_id(), monitor);
                locked_by(monitor).store(thread_id(), Release);
                return;
            }
            Err(loaded) => {
                if (loaded & CONTENDED == 0)
                    && count(monitor)
                        .compare_exchange(loaded, loaded | CONTENDED, Release, Relaxed)
                        .is_ok()
                {
                    miri_println!("[{}] {} set contended flag", thread_id(), monitor);
                }
                miri_println!(
                    "[{}] {} trying to wait with {}",
                    thread_id(),
                    monitor,
                    loaded | CONTENDED
                );
                atomic_wait::wait(count(monitor), loaded | CONTENDED);
            }
        }
    }
}

#[derive(Debug)]
pub struct NotLockedByThread;

pub fn exit(monitor: *const Monitor) -> Result<(), NotLockedByThread> {
    if locked_by(monitor).load(Relaxed) != thread_id() {
        return Err(NotLockedByThread);
    }
    let current = count(monitor).load(Relaxed);
    if current == u32::MAX {
        miri_println!("[{}] {} overflow detected", thread_id(), monitor);
        return Ok(());
    }

    if current > INCREMENT | CONTENDED {
        miri_println!(
            "[{}] {} decrease count to {}",
            thread_id(),
            monitor,
            current - INCREMENT
        );
        let loaded = count(monitor).swap(current - INCREMENT, AcqRel);
        if (current & CONTENDED == 0) & (loaded & CONTENDED == CONTENDED) {
            count(monitor).store((current - INCREMENT) | CONTENDED, Relaxed);
        }
    } else {
        miri_println!("[{}] {} release lock", thread_id(), monitor);
        locked_by(monitor).store(NO_THREAD, Release);
        if count(monitor).swap(0, AcqRel) == INCREMENT + CONTENDED {
            miri_println!("[{}] {} wake one", thread_id(), monitor);
            atomic_wait::wake_one(count(monitor));
        }
    }
    Ok(())
}

pub fn wait(monitor: *const Monitor) {
    atomic_wait::wait(count(monitor), 0);
}

pub fn notify_one(monitor: *const Monitor) -> Result<(), NotLockedByThread> {
    if locked_by(monitor).load(Relaxed) != thread_id() {
        return Err(NotLockedByThread);
    }
    atomic_wait::wake_one(count(monitor));
    Ok(())
}

pub fn notify_all(monitor: *const Monitor) -> Result<(), NotLockedByThread> {
    if locked_by(monitor).load(Relaxed) != thread_id() {
        return Err(NotLockedByThread);
    }
    atomic_wait::wake_all(count(monitor));
    Ok(())
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
            Self(unsafe {
                alloc_zeroed(Layout::from_size_align(Monitor::SIZE, align_of::<usize>()).unwrap())
            } as *const Monitor)
        }
    }

    impl Drop for Wrapper {
        fn drop(&mut self) {
            unsafe {
                dealloc(
                    self.0 as *mut u8,
                    Layout::from_size_align(Monitor::SIZE, align_of::<usize>()).unwrap(),
                );
            }
        }
    }

    unsafe impl Sync for Wrapper {}

    #[test]
    fn monitor_reentrancy() {
        let monitor = Wrapper::new();
        enter(monitor.0);
        enter(monitor.0);
        exit(monitor.0).unwrap();
        exit(monitor.0).unwrap();
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
                        enter(monitor.0);
                        enter(monitor.0);
                        sum.store(sum.load(Relaxed) + 1, Relaxed);
                        exit(monitor.0).unwrap();
                        exit(monitor.0).unwrap();
                    }
                });
            }
        });

        enter(monitor.0);
        assert_eq!(sum.load(Relaxed), threads * per_thread);
    }
}
