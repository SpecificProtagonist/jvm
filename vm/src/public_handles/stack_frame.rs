use core::ops::{Bound, Deref};

use crate::method::Code;

use super::Method;

pub struct StackFrame<'a> {
    pub(crate) jvm: &'a crate::jvm::Jvm,
    pub(crate) inner: &'a crate::interpreter::Frame<'a>,
}

impl StackFrame<'_> {
    pub fn parent(&self) -> Option<Self> {
        self.inner.parent.map(|p| Self {
            jvm: self.jvm,
            inner: p,
        })
    }

    pub fn method(&self) -> Method {
        Method::new(self.jvm, self.inner.method)
    }

    pub fn line_number(&self) -> Option<u16> {
        let lock = self.inner.method.code.read().unwrap();
        let Code::Bytecode(bytecode) = lock.deref() else {
            return None;
        };
        let pc = self.inner.pc;
        bytecode
            .line_number_table
            .upper_bound(Bound::Included(&pc))
            .next()
            .map(|(_, line)| *line)
    }
}
