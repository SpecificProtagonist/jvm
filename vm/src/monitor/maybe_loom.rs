#![allow(unused_imports)]

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

#[allow(unused)]
pub(crate) fn loom_model<F>(fun: F)
where
    F: Fn() + Sync + Send + 'static,
{
    #[cfg(not(loom))]
    fun();
    #[cfg(loom)]
    loom::model(fun);
}
