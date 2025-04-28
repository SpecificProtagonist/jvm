/// Guard types ensure values can't be used without the vm they belong to / be used with the wrong vm.
// TODO: Change this to an implementation similar to `Arc`
// in preparation for garbage collectionn (and a more pleasant api).
//
// When unsafe binder types are available, try them out.
macro_rules! define_guard {
    ($(#[$doc:meta])* $guard_typ:ident, $impl_typ:ty) => {
        $(#[$doc])*
        #[derive(Clone, Eq)]
        pub struct $guard_typ {
            pub(crate) value: &'static $impl_typ,
            pub(crate) jvm: Jvm,
        }

        impl $guard_typ {
            pub(crate) fn new(jvm: impl Into<Jvm>, value: &'static $impl_typ) -> Self {
                Self { value, jvm: jvm.into() }
            }

            #[allow(dead_code)]
            pub(crate) fn as_inner(&self, jvm: &Jvm) -> &'static $impl_typ {
                if &self.jvm == jvm {
                    unsafe {(self.value as *const $impl_typ).as_ref_unchecked()}
                } else {
                    panic!("Wrong JVM accessed")
                }
            }
        }

        impl Debug for $guard_typ {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                write!(f, "{:p}", self.value as *const $impl_typ)
            }
        }

        impl Hash for $guard_typ {
            fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
                // This is safe because pointers are hashed instead of dereferenced
                self.value.hash(state);
            }
        }

        impl PartialEq for $guard_typ {
            fn eq(&self, other: &Self) -> bool {
                // This is safe because pointers are compared instead of dereferenced
                self.value == other.value
            }
        }
    };
}

mod class;
mod field;
mod jvm;
mod method;
mod object;
mod value;

pub use class::*;
pub use field::*;
pub use jvm::*;
pub use method::*;
pub use object::*;
pub use value::*;

use crate::Type;

/// Err is an object that extends Throwable.
/// Currently no call stack is provided.
pub type JvmResult<T> = Result<T, Object>;

/// Java primitives:
///
/// `bool` ↔ `boolean`
/// `i8` ↔ `byte`
/// `i16` ↔ `short`
/// `u16` ↔ `char`
/// `i32` ↔ `int`
/// `i64` ↔ `long`
/// `f32` ↔ `float`
/// `f64` ↔ `double`
// This is a sealed trait for safety
#[allow(private_bounds)]
pub trait Primitive: crate::object::Data + Copy {
    const TYPE: Type;
}

impl Primitive for bool {
    const TYPE: Type = Type::Boolean;
}

impl Primitive for i8 {
    const TYPE: Type = Type::Byte;
}

impl Primitive for u16 {
    const TYPE: Type = Type::Char;
}

impl Primitive for i16 {
    const TYPE: Type = Type::Short;
}

impl Primitive for i32 {
    const TYPE: Type = Type::Int;
}

impl Primitive for i64 {
    const TYPE: Type = Type::Long;
}

impl Primitive for f32 {
    const TYPE: Type = Type::Float;
}

impl Primitive for f64 {
    const TYPE: Type = Type::Double;
}
