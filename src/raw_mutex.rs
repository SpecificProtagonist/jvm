#![cfg_attr(loom, allow(unused))]
// Can't use loom as it doesn't mock atomic-wait
use std::sync::atomic::{AtomicU32, Ordering::*};

// TODO: Use atomic version of ThreadQueue instead of atomic_wait?

pub(super) struct RawMutex {
    lock_word: AtomicU32,
}

const LOCKED_BIT: u32 = 1;
const CONTENDED_BIT: u32 = 2;

impl RawMutex {
    #[allow(clippy::declare_interior_mutable_const)]
    pub const INIT: Self = Self {
        lock_word: AtomicU32::new(0),
    };

    #[inline]
    pub fn lock(&self) {
        if let Err(loaded) = self
            .lock_word
            .compare_exchange(0, LOCKED_BIT, Acquire, Relaxed)
        {
            self.lock_locked(loaded);
        }
    }

    #[cold]
    fn lock_locked(&self, mut loaded: u32) {
        loop {
            if loaded & CONTENDED_BIT == 0 {
                let _ = self.lock_word.compare_exchange_weak(
                    loaded,
                    loaded | CONTENDED_BIT,
                    Release,
                    Relaxed,
                );
            }

            // Only sleep if contended flag got set and lock didn't get released
            atomic_wait::wait(&self.lock_word, loaded | CONTENDED_BIT);

            // Do a load first so we don't need to acquire ownership of the cache line every time
            loaded = self.lock_word.load(Relaxed);
            if loaded & !CONTENDED_BIT == 0 {
                match self.lock_word.compare_exchange(
                    loaded,
                    LOCKED_BIT | CONTENDED_BIT,
                    Acquire,
                    Relaxed,
                ) {
                    Ok(_) => {
                        return;
                    }
                    Err(value) => {
                        loaded = value;
                    }
                }
            }
        }
    }

    /// SAFETY: Mutex must be locked
    pub unsafe fn unlock(&self) {
        if self.lock_word.swap(0, AcqRel) == LOCKED_BIT | CONTENDED_BIT {
            atomic_wait::wake_one(&self.lock_word);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::thread;

    #[test]
    fn mutex() {
        let threads = 16;
        let per_thread = 1000;

        let mutex = RawMutex::INIT;
        let sum = AtomicU32::new(0);

        thread::scope(|scope| {
            for _ in 0..threads {
                scope.spawn(|| {
                    for _ in 0..per_thread {
                        mutex.lock();
                        sum.store(sum.load(Relaxed) + 1, Relaxed);
                        unsafe {
                            mutex.unlock();
                        }
                    }
                });
            }
        });

        mutex.lock();
        assert_eq!(sum.load(Relaxed), threads * per_thread);
    }
}
