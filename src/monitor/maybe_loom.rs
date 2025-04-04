#[cfg(loom)]
pub(crate) use loom::{
    lock_api::RawMutex,
    sync::atomic::{Ordering::*, *},
    thread::{self, current, Thread},
};
#[cfg(not(loom))]
pub(crate) use {
    crate::raw_mutex::RawMutex,
    core::sync::atomic::{Ordering::*, *},
    std::thread::{self, current, Thread},
};
