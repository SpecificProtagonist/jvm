use std::hash::Hash;

use crate::JVM;

// TODO: replace or augment with Object<'a>
#[derive(Debug, Clone, Copy, Eq)]
pub struct IntStr<'a>(pub(crate) &'a str);

impl<'a> IntStr<'a> {
    pub fn get(&self) -> &'a str {
        self.0
    }
}

impl<'a> PartialEq for IntStr<'a> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0, other.0)
    }
}

impl<'a> Hash for IntStr<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.as_ptr().hash(state);
    }
}

impl<'a> std::fmt::Display for IntStr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

pub enum MaybeInteredString<'a, 'b> {
    Interned(IntStr<'a>),
    Str(&'b str),
    String(String),
}

impl<'a, 'b> From<IntStr<'a>> for MaybeInteredString<'a, 'b> {
    fn from(str: IntStr<'a>) -> Self {
        Self::Interned(str)
    }
}

impl<'a, 'b> From<&'b str> for MaybeInteredString<'a, 'b> {
    fn from(str: &'b str) -> Self {
        Self::Str(str)
    }
}

impl<'a, 'b> From<String> for MaybeInteredString<'a, 'b> {
    fn from(str: String) -> Self {
        Self::String(str)
    }
}

impl<'a, 'b> MaybeInteredString<'a, 'b> {
    pub(crate) fn get(self, jvm: &JVM<'a>) -> IntStr<'a> {
        match self {
            MaybeInteredString::Interned(str) => str,
            MaybeInteredString::Str(str) => jvm.intern_str(str),
            MaybeInteredString::String(str) => jvm.intern_str(str),
        }
    }
}
