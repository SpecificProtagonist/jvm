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

/// Array element:
///
/// `bool` ↔ `boolean`
/// `i8` ↔ `byte`
/// `i16` ↔ `short`
/// `u16` ↔ `char`
/// `i32` ↔ `int`
/// `i64` ↔ `long`
/// `f32` ↔ `float`
/// `f64` ↔ `double`
/// `Option<Object>` / `Object` ↔ `java.lang.Object`
#[allow(private_bounds)]
pub trait Datum: DatumSealed {}

impl<T: DatumSealed> Datum for T {}

// This is a sealed trait for safety
#[allow(private_bounds)]
pub trait DatumSealed {
    const STATIC_TYPE: Option<Type>;
    type AsData: crate::object::Data;
    #[inline]
    fn write_valid(&self, _elem_type: &Type) -> bool {
        true
    }
    fn read(jvm: &Jvm, value: Self::AsData) -> Self;
    fn write(jvm: &Jvm, value: &Self) -> Self::AsData;
}

macro_rules! define_datumsealed {
    ($rust:ty, $jvm:ident) => {
        impl DatumSealed for $rust {
            const STATIC_TYPE: Option<Type> = Some(Type::$jvm);

            type AsData = Self;

            #[inline]
            fn read(_jvm: &Jvm, value: Self::AsData) -> Self {
                value
            }

            #[inline]
            fn write(_jvm: &Jvm, value: &Self) -> Self::AsData {
                *value
            }
        }
    };
}

define_datumsealed!(bool, Boolean);
define_datumsealed!(i8, Byte);
define_datumsealed!(u16, Char);
define_datumsealed!(i16, Short);
define_datumsealed!(i32, Int);
define_datumsealed!(i64, Long);
define_datumsealed!(f32, Float);
define_datumsealed!(f64, Double);

impl DatumSealed for Option<Object> {
    const STATIC_TYPE: Option<Type> = None;

    type AsData = usize;

    #[inline]
    fn read(jvm: &Jvm, value: Self::AsData) -> Self {
        crate::object::Object::from_addr(value).map(|obj| Object::new(jvm.clone(), obj))
    }

    fn write_valid(&self, elem_type: &Type) -> bool {
        if let Type::Ref(elem_type) = elem_type {
            self.as_ref()
                .map(|obj| {
                    obj.value
                        .class(obj.jvm.deref().heap.ptrs())
                        .true_subclass_of(elem_type)
                })
                .unwrap_or(true)
        } else {
            false
        }
    }

    #[inline]
    fn write(jvm: &Jvm, value: &Self) -> Self::AsData {
        value
            .as_ref()
            .map(|o| o.as_inner(jvm).into_addr().get())
            .unwrap_or(0)
    }
}
