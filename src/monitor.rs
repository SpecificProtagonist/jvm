use std::{
    cell::Cell,
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
/// Zeroed memory can represent an unlocked monitor.
#[repr(C)]
pub(crate) struct Monitor {
    /// Custom thread id. Get's waited on when waiting on the object.
    locked_by: AtomicU32,
    /// Get's waited on when locking the object.
    /// Upper 31 bits: How many times it is currently locked.
    /// Lowermost bit: Whether the lock was contended.
    /// Overflow is handled by perma-locking the object.
    count: AtomicU32,
}

const INCREMENT: u32 = 1;

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

// Never NO_THREAD after `init_thread_id` was called
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
        let id = alloc
            .previous
            .checked_add(1)
            .expect("Too many live threads accessing JVM");
        alloc.previous = id;
        id
    });
    THREAD_ID.with(|key| key.0.set(id))
}

#[derive(Debug)]
pub struct LockNotHeld;

impl Monitor {
    #[inline]
    pub fn enter(&self) {
        ensure_thread_id_initialized();
        match self.count.compare_exchange(0, INCREMENT, Acquire, Relaxed) {
            Ok(_) => {
                miri_println!(
                    "[{}] {:p} locked uncontended",
                    thread_id(),
                    self as *const _
                );
                self.locked_by.store(thread_id(), Release);
            }
            Err(loaded) => self.enter_locked(loaded),
        }
    }

    #[cold]
    fn enter_locked(&self, mut loaded: u32) {
        // Already locked, checked if by the same thread
        if self.locked_by.load(Acquire) == thread_id() {
            // If we reach u32::MAX, we never unlock the object again.
            let new_count = loaded.saturating_add(INCREMENT);
            self.count.store(new_count, Relaxed);
            miri_println!("[{}] {:p} locked recursive", thread_id(), self as *const _);
            return;
        }
        loop {
            // Only sleep if contended flag got set and lock didn't get released
            atomic_wait::wait(&self.count, loaded);

            loaded = self.count.load(Relaxed);
            if loaded == 0 {
                match self.count.compare_exchange(0, INCREMENT, Acquire, Relaxed) {
                    Ok(_) => {
                        miri_println!("[{}] {:p} locked contended", thread_id(), self as *const _);
                        self.locked_by.store(thread_id(), Release);
                        return;
                    }
                    Err(value) => {
                        loaded = value;
                    }
                }
            }
        }
    }

    pub fn exit(&self) -> Result<(), LockNotHeld> {
        if self.locked_by.load(Relaxed) != thread_id() {
            return Err(LockNotHeld);
        }
        let current = self.count.load(Relaxed);
        if current == u32::MAX {
            miri_println!("[{}] {:p} overflow detected", thread_id(), self as *const _);
            return Ok(());
        }

        if current > INCREMENT {
            miri_println!(
                "[{}] {:p} decrease count to {}",
                thread_id(),
                self as *const _,
                current - INCREMENT
            );
            self.count.store(current - INCREMENT, Release);
        } else {
            miri_println!("[{}] {:p} release lock", thread_id(), self as *const _);
            self.locked_by.store(NO_THREAD, Release);
            if self.count.swap(0, AcqRel) == INCREMENT {
                miri_println!("[{}] {:p} wake one", thread_id(), self as *const _);
                atomic_wait::wake_one(&self.count);
            }
        }
        Ok(())
    }

    pub fn wait(&self) -> Result<(), LockNotHeld> {
        if self.locked_by.load(Relaxed) != thread_id() {
            return Err(LockNotHeld);
        }

        // First we need to release the lock. This differs from exit() in that we release the lock
        // no matter how high the lock count is, and in that we don't clear the thread id.
        let count = self.count.swap(0, AcqRel);
        atomic_wait::wake_one(&self.count);

        // Wait, but only if the lock hasn't been taken since
        // (which would be required for sending notifications).
        // If the lock is contended and another thread aquires the lock before the wait call,
        // this will lead to a spurious wakeup (or rather, the sleep won't happen in the first case).
        // That's… not great, but it's semantically correct and I don't think `Object::wait` is all that common.
        atomic_wait::wait(&self.locked_by, thread_id());

        // Now take the lock again, but set the count to whatever it was before.
        loop {
            let mut loaded = self.count.load(Relaxed);
            if loaded == 0 {
                match self.count.compare_exchange(0, count, Acquire, Relaxed) {
                    Ok(_) => {
                        // Locked; contention flag set because it got cleared during the previous unlock
                        self.locked_by.store(thread_id(), Release);
                        return Ok(());
                    }
                    Err(value) => {
                        loaded = value;
                    }
                }
            }

            atomic_wait::wait(&self.count, loaded);
        }
    }

    pub fn notify_one(&self) -> Result<(), LockNotHeld> {
        if self.locked_by.load(Relaxed) != thread_id() {
            return Err(LockNotHeld);
        }
        atomic_wait::wake_one(&self.locked_by);
        Ok(())
    }

    pub fn notify_all(&self) -> Result<(), LockNotHeld> {
        if self.locked_by.load(Relaxed) != thread_id() {
            return Err(LockNotHeld);
        }
        atomic_wait::wake_all(&self.locked_by);
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::default;
    use std::{sync::atomic::AtomicBool, thread, time::Duration};

    fn new() -> Box<Monitor> {
        Box::new(Monitor {
            locked_by: default(),
            count: default(),
        })
    }

    #[test]
    fn monitor_reentrancy() {
        let monitor = new();
        monitor.enter();
        monitor.enter();
        monitor.exit().unwrap();
        monitor.exit().unwrap();
        assert_eq!(monitor.count.load(SeqCst), 0);
    }

    #[test]
    fn monitor_contention() {
        let threads = 16;
        let per_thread = 1000;

        let monitor = new();
        let sum = AtomicU32::new(0);

        thread::scope(|scope| {
            for _ in 0..threads {
                scope.spawn(|| {
                    for _ in 0..per_thread {
                        let _ = &monitor;
                        monitor.enter();
                        monitor.enter();
                        sum.store(sum.load(Relaxed) + 1, Relaxed);
                        monitor.exit().unwrap();
                        monitor.exit().unwrap();
                    }
                });
            }
        });

        monitor.enter();
        assert_eq!(sum.load(Relaxed), threads * per_thread);
    }

    #[test]
    fn monitor_condvar_wakeup() {
        let value = AtomicBool::new(false);
        let monitor = new();
        let mut wakeups = 0;

        thread::scope(|s| {
            s.spawn(|| {
                thread::sleep(Duration::from_secs(1));
                monitor.enter();
                value.store(true, SeqCst);
                monitor.notify_one().unwrap();
                monitor.exit().unwrap();
            });

            thread::sleep(Duration::from_millis(100));

            monitor.enter();
            while !value.load(SeqCst) {
                monitor.wait().unwrap();
                wakeups += 1;
            }
        });

        // Check that the main thread actually did wait (not busy-loop),
        // while still allowing for a few spurious wake ups.
        assert!(wakeups < 10);
    }
}
