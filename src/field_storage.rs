use std::{
    alloc::{alloc_zeroed, Layout},
    cell::UnsafeCell,
    slice,
    sync::atomic::{AtomicU16, AtomicU32, AtomicU64, AtomicU8, AtomicUsize, Ordering},
};

pub struct FieldStorage(UnsafeCell<Box<[u8]>>);

impl FieldStorage {
    pub fn new(layout: Layout) -> Self {
        unsafe {
            Self(
                Box::from_raw(slice::from_raw_parts_mut(
                    alloc_zeroed(layout),
                    layout.size(),
                ))
                .into(),
            )
        }
    }

    pub fn read_u8(&self, offset: u32) -> u8 {
        unsafe {
            let atomic = &mut (*self.0.get())[offset as usize] as *mut u8 as *const AtomicU8;
            (&*atomic).load(Ordering::Relaxed)
        }
    }

    /// Unsafe: Needs correct alignment
    pub unsafe fn read_u16(&self, offset: u32) -> u16 {
        let atomic = &mut (*self.0.get())[offset as usize] as *mut u8 as *const AtomicU16;
        (&*atomic).load(Ordering::Relaxed)
    }

    /// Unsafe: Needs correct alignment
    pub unsafe fn read_u32(&self, offset: u32) -> u32 {
        let atomic = &mut (*self.0.get())[offset as usize] as *mut u8 as *const AtomicU32;
        (&*atomic).load(Ordering::Relaxed)
    }

    /// Unsafe: Needs correct alignment
    pub unsafe fn read_u64(&self, offset: u32) -> u64 {
        let atomic = &mut (*self.0.get())[offset as usize] as *mut u8 as *const AtomicU64;
        (&*atomic).load(Ordering::Relaxed)
    }

    /// Unsafe: Needs correct alignment
    pub unsafe fn read_usize(&self, offset: u32) -> usize {
        let atomic = &mut (*self.0.get())[offset as usize] as *mut u8 as *const AtomicUsize;
        (&*atomic).load(Ordering::Relaxed)
    }

    pub fn write_u8(&self, offset: u32, value: u8) {
        unsafe {
            let atomic = &mut (*self.0.get())[offset as usize] as *mut u8 as *const AtomicU8;
            (&*atomic).store(value, Ordering::Relaxed)
        }
    }

    /// Unsafe: Needs correct alignment
    pub unsafe fn write_u16(&self, offset: u32, value: u16) {
        let atomic = &mut (*self.0.get())[offset as usize] as *mut u8 as *const AtomicU16;
        (&*atomic).store(value, Ordering::Relaxed)
    }

    /// Unsafe: Needs correct alignment
    pub unsafe fn write_u32(&self, offset: u32, value: u32) {
        let atomic = &mut (*self.0.get())[offset as usize] as *mut u8 as *const AtomicU32;
        (&*atomic).store(value, Ordering::Relaxed)
    }

    /// Unsafe: Needs correct alignment
    pub unsafe fn write_u64(&self, offset: u32, value: u64) {
        let atomic = &mut (*self.0.get())[offset as usize] as *mut u8 as *const AtomicU64;
        (&*atomic).store(value, Ordering::Relaxed)
    }

    /// Unsafe: Needs correct alignment
    pub unsafe fn write_usize(&self, offset: u32, value: usize) {
        let atomic = &mut (*self.0.get())[offset as usize] as *mut u8 as *const AtomicUsize;
        (&*atomic).store(value, Ordering::Relaxed)
    }
}
