#[cfg(loom)]
pub(crate) use loom::{
    lock_api::RawMutex,
    sync::{atomic::*, *},
    thread,
};

#[cfg(not(loom))]
pub(crate) use {
    crate::raw_mutex::RawMutex,
    std::{
        sync::{atomic::*, *},
        thread,
    },
};

pub(crate) use std::sync::atomic::Ordering::*;
