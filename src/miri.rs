#[cfg(all(miri, feature = "miri_debug_print"))]
extern "Rust" {
    fn miri_write_to_stdout(bytes: &[u8]);
}

/// Prints a message (if enabled). Unlike println!(), this doesn't cause
/// synchronization in miri's memory model and therefor is suitable for
/// debugging data races.
#[macro_export]
macro_rules! miri_println {
    ($fmt:literal $(, $exprs:expr)*) => {
        #[cfg(all(miri, feature = "miri_debug_print"))]
        unsafe {
            let message = format!($fmt, $($exprs),*) + "\n";
            miri_write_to_stdout(message.as_bytes());
        }
    };
}
