#![allow(private_interfaces)]

// TODO: Directly use &[IVal]
// TODO: Allow first argument to be Object (for instance methods)

use crate::{
    method::Code, object, AccessFlags, Jvm, JvmResult, Method, MethodDescriptor, Object, Typ, Value,
};

#[derive(Debug)]
pub enum NativeBindError {
    SignatureMismatch,
    MethodNotNative,
}

#[allow(private_bounds)]
pub fn bind_native_code<F, T>(method: &Method, code: F) -> Result<(), NativeBindError>
where
    F: NativeFn<T>,
{
    if !valid_signature::<F, T>(&method.value.nat.typ) {
        return Err(NativeBindError::SignatureMismatch);
    }
    // closure_lifetime_binder isn't stable yet
    fn specify_lifetimes(
        f: impl for<'b> Fn(&'b Jvm, &'b [Value<'b>]) -> JvmResult<'b, Value<'b>>,
    ) -> Box<impl for<'b> Fn(&'b Jvm, &'b [Value<'b>]) -> JvmResult<'b, Value<'b>>> {
        Box::new(f)
    }
    let boxed = specify_lifetimes(move |jvm, args| F::invoke(&code, jvm, args));
    if method.value.access_flags.contains(AccessFlags::NATIVE) {
        *method.value.code.write() = Code::Native(boxed);
        Ok(())
    } else {
        Err(NativeBindError::MethodNotNative)
    }
}

pub(crate) fn valid_signature<F, T>(descriptor: &MethodDescriptor) -> bool
where
    F: NativeFn<T>,
{
    fn ok(types: (&Typ, ArgTyp)) -> bool {
        matches!(
            types,
            (Typ::Boolean, ArgTyp::Boolean)
                | (Typ::Byte, ArgTyp::Byte)
                | (Typ::Short, ArgTyp::Short)
                | (Typ::Char, ArgTyp::Char)
                | (Typ::Int, ArgTyp::Int)
                | (Typ::Long, ArgTyp::Long)
                | (Typ::Float, ArgTyp::Float)
                | (Typ::Double, ArgTyp::Double)
                | (Typ::Ref(_), ArgTyp::Object)
        )
    }
    let Some((args, ret)) = F::descriptor() else {
        return true;
    };
    descriptor.args.iter().zip(args).all(ok)
        & match (&descriptor.returns, ret) {
            (Some(a), Some(b)) => ok((a, b)),
            (None, None) => true,
            _ => false,
        }
}

enum ArgTyp {
    Boolean,
    Byte,
    Short,
    Char,
    Int,
    Long,
    Float,
    Double,
    Object,
}

#[allow(private_interfaces)]
pub(crate) trait NativeFn<T>: Send + Sync + 'static {
    fn descriptor() -> Option<(Vec<ArgTyp>, Option<ArgTyp>)>;
    fn invoke<'a>(&self, jvm: &'a Jvm, args: &'a [Value<'a>]) -> JvmResult<'a, Value<'a>>;
}

trait Arg {
    type Item<'new>;
    fn typ() -> ArgTyp;
    fn get<'a>(values: &[Value<'a>], index: &mut usize) -> Self::Item<'a>;
}

impl Arg for bool {
    type Item<'new> = bool;

    fn typ() -> ArgTyp {
        ArgTyp::Boolean
    }

    fn get<'a>(values: &[Value<'a>], index: &mut usize) -> Self::Item<'a> {
        match values[*index] {
            Value::Boolean(value) => {
                *index += 1;
                value
            }
            _ => unreachable!(),
        }
    }
}

impl Arg for i8 {
    type Item<'new> = i8;

    fn typ() -> ArgTyp {
        ArgTyp::Byte
    }

    fn get<'a>(values: &[Value<'a>], index: &mut usize) -> Self::Item<'a> {
        match values[*index] {
            Value::Byte(value) => {
                *index += 1;
                value
            }
            _ => unreachable!(),
        }
    }
}

impl Arg for i16 {
    type Item<'new> = i16;

    fn typ() -> ArgTyp {
        ArgTyp::Short
    }

    fn get<'a>(values: &[Value<'a>], index: &mut usize) -> Self::Item<'a> {
        match values[*index] {
            Value::Short(value) => {
                *index += 1;
                value
            }
            _ => unreachable!(),
        }
    }
}

impl Arg for u16 {
    type Item<'new> = u16;

    fn typ() -> ArgTyp {
        ArgTyp::Char
    }

    fn get<'a>(values: &[Value<'a>], index: &mut usize) -> Self::Item<'a> {
        match values[*index] {
            Value::Char(value) => {
                *index += 1;
                value
            }
            _ => unreachable!(),
        }
    }
}

impl Arg for i32 {
    type Item<'new> = i32;

    fn typ() -> ArgTyp {
        ArgTyp::Int
    }

    fn get<'a>(values: &[Value<'a>], index: &mut usize) -> Self::Item<'a> {
        match values[*index] {
            Value::Int(value) => {
                *index += 1;
                value
            }
            _ => unreachable!(),
        }
    }
}

impl Arg for i64 {
    type Item<'new> = i64;

    fn typ() -> ArgTyp {
        ArgTyp::Long
    }

    fn get<'a>(values: &[Value<'a>], index: &mut usize) -> Self::Item<'a> {
        match values[*index] {
            Value::Long(value) => {
                *index += 1;
                value
            }
            _ => unreachable!(),
        }
    }
}

impl Arg for f32 {
    type Item<'new> = f32;

    fn typ() -> ArgTyp {
        ArgTyp::Float
    }

    fn get<'a>(values: &[Value<'a>], index: &mut usize) -> Self::Item<'a> {
        match values[*index] {
            Value::Float(value) => {
                *index += 1;
                value
            }
            _ => unreachable!(),
        }
    }
}

impl Arg for f64 {
    type Item<'new> = f64;

    fn typ() -> ArgTyp {
        ArgTyp::Double
    }

    fn get<'a>(values: &[Value<'a>], index: &mut usize) -> Self::Item<'a> {
        match values[*index] {
            Value::Double(value) => {
                *index += 1;
                value
            }
            _ => unreachable!(),
        }
    }
}

impl<'old> Arg for Option<Object<'old>> {
    fn typ() -> ArgTyp {
        ArgTyp::Object
    }

    type Item<'new> = Option<Object<'new>>;

    fn get<'a>(values: &[Value<'a>], index: &mut usize) -> Self::Item<'a> {
        match values[*index] {
            Value::Ref(value) => {
                *index += 1;
                value
            }
            _ => unreachable!(),
        }
    }
}

trait Return: Into<Value<'static>> {
    fn typ() -> Option<ArgTyp>;
}

impl Return for () {
    fn typ() -> Option<ArgTyp> {
        None
    }
}

impl Return for bool {
    fn typ() -> Option<ArgTyp> {
        Some(ArgTyp::Boolean)
    }
}

impl Return for i8 {
    fn typ() -> Option<ArgTyp> {
        Some(ArgTyp::Byte)
    }
}

impl Return for i16 {
    fn typ() -> Option<ArgTyp> {
        Some(ArgTyp::Short)
    }
}
impl Return for u16 {
    fn typ() -> Option<ArgTyp> {
        Some(ArgTyp::Char)
    }
}
impl Return for i32 {
    fn typ() -> Option<ArgTyp> {
        Some(ArgTyp::Int)
    }
}
impl Return for i64 {
    fn typ() -> Option<ArgTyp> {
        Some(ArgTyp::Long)
    }
}
impl Return for f32 {
    fn typ() -> Option<ArgTyp> {
        Some(ArgTyp::Float)
    }
}
impl Return for f64 {
    fn typ() -> Option<ArgTyp> {
        Some(ArgTyp::Double)
    }
}

impl<F> NativeFn<&'static ()> for F
where
    F: for<'a> Fn(&'a Jvm, &[Value<'a>]) -> JvmResult<'a, ()> + Send + Sync + 'static,
{
    fn descriptor() -> Option<(Vec<ArgTyp>, Option<ArgTyp>)> {
        None
    }

    fn invoke<'a>(&self, jvm: &'a Jvm, args: &'a [Value<'a>]) -> JvmResult<'a, Value<'a>> {
        self(jvm, args).map(|_| Value::Void)
    }
}

impl<F, R> NativeFn<(R, &'static ())> for F
where
    F: for<'a> Fn(&'a Jvm, &[Value<'a>]) -> JvmResult<'a, R> + Send + Sync + 'static,
    R: Return,
{
    fn descriptor() -> Option<(Vec<ArgTyp>, Option<ArgTyp>)> {
        None
    }

    fn invoke<'a>(&self, jvm: &'a Jvm, args: &'a [Value<'a>]) -> JvmResult<'a, Value<'a>> {
        self(jvm, args).map(|r| r.into())
    }
}

macro_rules! impl_args {
    ($($param: ident),*) => {
        impl<F, $($param,)*> NativeFn<(object::Object,$($param,)*)> for F
        where
            for<'a> F: Fn(&'a Jvm, $($param),*) -> JvmResult<'a, Option<Object>>
                + Fn(&'a Jvm, $($param::Item<'a>),*) -> JvmResult<'a, Option<Object<'a>>>
                + Send
                + Sync
                + 'static,
            $($param: Arg,)*
        {
            fn descriptor() -> Option<(Vec<ArgTyp>, Option<ArgTyp>)> {
                Some((vec![$($param::typ()),*], Some(ArgTyp::Object)))
            }

            #[allow(unused_variables, unused_mut)]
            fn invoke<'a>(&self, jvm: &'a Jvm, args: &'a [Value<'a>]) -> JvmResult<'a, Value<'a>> {
                let mut i = 0;
                self(jvm, $($param::get(args, &mut i)),*).map(|r| r.into())
            }
        }
        impl<F, $($param,)* R> NativeFn<(R,$($param,)*)> for F
        where
            for<'a> F: Fn(&'a Jvm, $($param),*) -> JvmResult<'a, R>
                + Fn(&'a Jvm, $($param::Item<'a>),*) -> JvmResult<'a, R>
                + Send
                + Sync
                + 'static,
            $($param: Arg,)*
            R: Return,
        {
            fn descriptor() -> Option<(Vec<ArgTyp>, Option<ArgTyp>)> {
                Some((vec![$($param::typ()),*], R::typ()))
            }

            #[allow(unused_variables, unused_mut)]
            fn invoke<'a>(&self, jvm: &'a Jvm, args: &'a [Value<'a>]) -> JvmResult<'a, Value<'a>> {
                let mut i = 0;
                self(jvm, $($param::get(args, &mut i)),*).map(|r| r.into())
            }
        }
    }
}

impl_args!();
impl_args!(T1);
impl_args!(T1, T2);
impl_args!(T1, T2, T3);
impl_args!(T1, T2, T3, T4);
impl_args!(T1, T2, T3, T4, T5);
impl_args!(T1, T2, T3, T4, T5, T6);
impl_args!(T1, T2, T3, T4, T5, T6, T7);
impl_args!(T1, T2, T3, T4, T5, T6, T7, T8);
impl_args!(T1, T2, T3, T4, T5, T6, T7, T8, T9);
impl_args!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10);
impl_args!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11);
impl_args!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12);
impl_args!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13);
impl_args!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14);
impl_args!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15);
impl_args!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16);
