use std::{
    cell::Cell,
    sync::atomic::{AtomicU32, Ordering::*},
};

// use crate::miri_println;

// TODO: Use atomic version of ThreadQueue instead of atomic_wait?

#[derive(Default)]
pub(super) struct Mutex {
    lock_word: AtomicU32,
}

const LOCKED_BIT: u32 = 1;
const CONTENDED_BIT: u32 = 2;

impl Mutex {
    #[inline]
    pub fn lock(&self) {
        match self
            .lock_word
            .compare_exchange(0, LOCKED_BIT, Acquire, Relaxed)
        {
            Ok(_) => {
                // miri_println!(
                //     "[{:?}] {:p} locked uncontended",
                //     std::thread::current().id(),
                //     self as *const _
                // );
            }
            Err(loaded) => self.lock_locked(loaded),
        }
    }

    #[cold]
    fn lock_locked(&self, mut loaded: u32) {
        loop {
            if (loaded & CONTENDED_BIT == 0)
                && self
                    .lock_word
                    .compare_exchange_weak(loaded, loaded | CONTENDED_BIT, Release, Relaxed)
                    .is_ok()
            {
                // miri_println!("[{:?}] set contended flag", std::thread::current().id());
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
                        // miri_println!(
                        //     "[{:?}] {:p} locked contended",
                        //     std::thread::current().id(),
                        //     self as *const _
                        // );
                        return;
                    }
                    Err(value) => {
                        loaded = value;
                    }
                }
            }
        }
    }

    pub fn unlock(&self) {
        let current = self.lock_word.load(Relaxed);

        // miri_println!(
        //     "[{:?}] {:p} release lock",
        //     std::thread::current().id(),
        //     self as *const _
        // );
        if self.lock_word.swap(0, AcqRel) == LOCKED_BIT | CONTENDED_BIT {
            // miri_println!(
            //     "[{:?}] {:p} wake one",
            //     std::thread::current().id(),
            //     self as *const _
            // );
            atomic_wait::wake_one(&self.lock_word);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::default;
    use std::thread;

    #[test]
    fn mutex() {
        let threads = 16;
        let per_thread = 1000;

        let mutex = Mutex::default();
        let sum = AtomicU32::new(0);

        thread::scope(|scope| {
            for _ in 0..threads {
                scope.spawn(|| {
                    for _ in 0..per_thread {
                        mutex.lock();
                        sum.store(sum.load(Relaxed) + 1, Relaxed);
                        mutex.unlock();
                    }
                });
            }
        });

        mutex.lock();
        assert_eq!(sum.load(Relaxed), threads * per_thread);
    }
}
