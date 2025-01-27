#![macro_use]

//! Swaps concurrency primitives for loom's version when run with RUSTFLAGS="--cfg loom"
//! which allows running all possible executions during tests.

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

pub(crate) use std::sync::{atomic::Ordering::*, Arc};

/// Log provided info if loom is active
#[cfg(loom)]
macro_rules! loom_info {
    // span with thread id already set by loom
    ($($tokens:tt)*) => {
        tracing::info!($($tokens)*);
    };
}

#[cfg(not(loom))]
macro_rules! loom_info {
    ($($tokens:tt)*) => {};
}

// #[cfg(loom)]
// macro_rules! loom_span {
//     ($name:literal, $($tokens:tt)*) => {
//         let _span =
//             tracing::info_span!($name, $($tokens)*).entered();
//     };
// }

// #[cfg(not(loom))]
// macro_rules! loom_span {
//     ($name:literal, $($tokens:tt)*) => {};
// }

pub(crate) use loom_info;
// pub(crate) use loom_span;
