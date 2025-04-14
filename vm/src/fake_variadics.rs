//! This module implements the binding of native methods;
//! available format of native functions is documented on `Method::bind_native_code`.
#![allow(private_interfaces)]

// TODO: Directly use &[IVal]

use crate::{
    AccessFlags, Jvm, JvmResult, Object, Type, Value,
    method::{Code, Method},
};

#[derive(Debug)]
pub enum NativeBindError {
    SignatureMismatch,
    MethodNotNative,
}

#[allow(private_bounds)]
pub(crate) fn bind_native_code<F, T>(method: &Method, code: F) -> Result<(), NativeBindError>
where
    F: NativeFn<T>,
{
    if !valid_signature::<F, T>(method) {
        return Err(NativeBindError::SignatureMismatch);
    }
    fn specify_lifetimes(
        f: impl for<'a, 'b> Fn(&'a Jvm, &'b [Value]) -> JvmResult<Option<Value>>,
    ) -> Box<impl for<'a, 'b> Fn(&'a Jvm, &'b [Value]) -> JvmResult<Option<Value>>> {
        Box::new(f)
    }
    let boxed = specify_lifetimes(move |jvm, args| F::invoke(&code, jvm, args));
    if method.access_flags.contains(AccessFlags::NATIVE) {
        *method.code.write().unwrap() = Code::Native(boxed);
        Ok(())
    } else {
        Err(NativeBindError::MethodNotNative)
    }
}

fn valid_signature<F, T>(method: &Method) -> bool
where
    F: NativeFn<T>,
{
    fn ok(types: (&Type, ArgTyp)) -> bool {
        matches!(
            types,
            (Type::Boolean, ArgTyp::Boolean)
                | (Type::Byte, ArgTyp::Byte)
                | (Type::Short, ArgTyp::Short)
                | (Type::Char, ArgTyp::Char)
                | (Type::Int, ArgTyp::Int)
                | (Type::Long, ArgTyp::Long)
                | (Type::Float, ArgTyp::Float)
                | (Type::Double, ArgTyp::Double)
                | (Type::Ref(_), ArgTyp::Object)
        )
    }
    let Some((args, ret)) = F::descriptor() else {
        return true;
    };
    let mut iter = method.nat.typ.args.iter();
    let mut args = args.into_iter();
    if !method.access_flags.contains(AccessFlags::STATIC) {
        iter.next();
        if !matches!(args.next(), Some(ArgTyp::This | ArgTyp::Object)) {
            return false;
        }
    }
    iter.zip(args).all(ok)
        & match (&method.nat.typ.returns, ret) {
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
    This,
}

/// Implemented for functions of the following form:
/// - optionally, a first argument of type `&Jvm`
/// - the remaining arguments consist of either `&[Value]` or up to 16 arguments of a translateable type
/// - the return type must be a translatable type, optionally wrapped in a `JvmResult`
///
/// Translatable types:  
/// `bool`, `i8`, `i16`, `u16`, `i32`, `i64`, `f32`, `f64`, `()` (only for return type), `Option<Object>`, `Object` (only for `this`)
///
/// # Examples
/// `static native int add(short a, short b);` can be bound to:
/// - `fn(i16, i16) -> i32`
/// - `fn(&Jvm, i16, i16) -> i32`
/// - `fn(i16, i16) -> JvmResult<i32>`
/// - `fn(&[Value]) -> i32`
/// - etc
///
/// `native void run(Runner runner);` can be bound to:
/// - `fn(Object, Option<Object>)`
/// - `fn(&Jvm, Object, Option<Object>) -> JvmResult<()>`
/// - etc
pub trait NativeFn<T>: Send + Sync + 'static {
    fn descriptor() -> Option<(Vec<ArgTyp>, Option<ArgTyp>)>;
    fn invoke(&self, jvm: &Jvm, args: &[Value]) -> JvmResult<Option<Value>>;
}

trait Arg {
    fn typ() -> ArgTyp;
    fn get(values: &[Value], index: &mut usize) -> Self;
}

impl Arg for bool {
    fn typ() -> ArgTyp {
        ArgTyp::Boolean
    }

    fn get(values: &[Value], index: &mut usize) -> Self {
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
    fn typ() -> ArgTyp {
        ArgTyp::Byte
    }

    fn get(values: &[Value], index: &mut usize) -> Self {
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
    fn typ() -> ArgTyp {
        ArgTyp::Short
    }

    fn get(values: &[Value], index: &mut usize) -> Self {
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
    fn typ() -> ArgTyp {
        ArgTyp::Char
    }

    fn get(values: &[Value], index: &mut usize) -> Self {
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
    fn typ() -> ArgTyp {
        ArgTyp::Int
    }

    fn get(values: &[Value], index: &mut usize) -> Self {
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
    fn typ() -> ArgTyp {
        ArgTyp::Long
    }

    fn get(values: &[Value], index: &mut usize) -> Self {
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
    fn typ() -> ArgTyp {
        ArgTyp::Float
    }

    fn get(values: &[Value], index: &mut usize) -> Self {
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
    fn typ() -> ArgTyp {
        ArgTyp::Double
    }

    fn get(values: &[Value], index: &mut usize) -> Self {
        match values[*index] {
            Value::Double(value) => {
                *index += 1;
                value
            }
            _ => unreachable!(),
        }
    }
}

impl Arg for Option<Object> {
    fn typ() -> ArgTyp {
        ArgTyp::Object
    }

    fn get(values: &[Value], index: &mut usize) -> Self {
        match values[*index] {
            Value::Ref(value) => {
                *index += 1;
                value
            }
            _ => unreachable!(),
        }
    }
}

impl Arg for Object {
    fn typ() -> ArgTyp {
        ArgTyp::This
    }

    fn get(values: &[Value], index: &mut usize) -> Self {
        match values[*index] {
            Value::Ref(value) => {
                *index += 1;
                value.unwrap()
            }
            _ => unreachable!(),
        }
    }
}

trait ReturnSuccess {
    fn typ() -> Option<ArgTyp>;
    fn value(self) -> Option<Value>;
}

impl ReturnSuccess for () {
    fn typ() -> Option<ArgTyp> {
        None
    }

    fn value(self) -> Option<Value> {
        None
    }
}

impl ReturnSuccess for bool {
    fn typ() -> Option<ArgTyp> {
        Some(ArgTyp::Boolean)
    }

    fn value(self) -> Option<Value> {
        Some(self.into())
    }
}

impl ReturnSuccess for i8 {
    fn typ() -> Option<ArgTyp> {
        Some(ArgTyp::Byte)
    }

    fn value(self) -> Option<Value> {
        Some(self.into())
    }
}

impl ReturnSuccess for i16 {
    fn typ() -> Option<ArgTyp> {
        Some(ArgTyp::Short)
    }

    fn value(self) -> Option<Value> {
        Some(self.into())
    }
}

impl ReturnSuccess for u16 {
    fn typ() -> Option<ArgTyp> {
        Some(ArgTyp::Char)
    }

    fn value(self) -> Option<Value> {
        Some(self.into())
    }
}

impl ReturnSuccess for i32 {
    fn typ() -> Option<ArgTyp> {
        Some(ArgTyp::Int)
    }

    fn value(self) -> Option<Value> {
        Some(self.into())
    }
}

impl ReturnSuccess for i64 {
    fn typ() -> Option<ArgTyp> {
        Some(ArgTyp::Long)
    }

    fn value(self) -> Option<Value> {
        Some(self.into())
    }
}

impl ReturnSuccess for f32 {
    fn typ() -> Option<ArgTyp> {
        Some(ArgTyp::Float)
    }

    fn value(self) -> Option<Value> {
        Some(self.into())
    }
}

impl ReturnSuccess for f64 {
    fn typ() -> Option<ArgTyp> {
        Some(ArgTyp::Double)
    }

    fn value(self) -> Option<Value> {
        Some(self.into())
    }
}

impl ReturnSuccess for Option<Object> {
    fn typ() -> Option<ArgTyp> {
        Some(ArgTyp::Object)
    }

    fn value(self) -> Option<Value> {
        Some(self.into())
    }
}

trait Return {
    fn typ() -> Option<ArgTyp>;
    fn value(self) -> JvmResult<Option<Value>>;
}

impl<T: ReturnSuccess> Return for T {
    fn typ() -> Option<ArgTyp> {
        <T as ReturnSuccess>::typ()
    }

    fn value(self) -> JvmResult<Option<Value>> {
        Ok(ReturnSuccess::value(self))
    }
}

impl<T: ReturnSuccess> Return for JvmResult<T> {
    fn typ() -> Option<ArgTyp> {
        <T as ReturnSuccess>::typ()
    }

    fn value(self) -> JvmResult<Option<Value>> {
        self.map(|inner| ReturnSuccess::value(inner))
    }
}

impl<F, R> NativeFn<(R, &'static ())> for F
where
    F: Fn(&[Value]) -> R + Send + Sync + 'static,
    R: Return,
{
    fn descriptor() -> Option<(Vec<ArgTyp>, Option<ArgTyp>)> {
        None
    }

    fn invoke(&self, _: &Jvm, args: &[Value]) -> JvmResult<Option<Value>> {
        self(args).value()
    }
}

impl<F, R> NativeFn<(Jvm, R, &'static ())> for F
where
    F: for<'a> Fn(&'a Jvm, &[Value]) -> R + Send + Sync + 'static,
    R: Return,
{
    fn descriptor() -> Option<(Vec<ArgTyp>, Option<ArgTyp>)> {
        None
    }

    fn invoke(&self, jvm: &Jvm, args: &[Value]) -> JvmResult<Option<Value>> {
        self(jvm, args).value()
    }
}

macro_rules! impl_args {
    ($($param: ident),*) => {
        impl<F, $($param,)* R> NativeFn<(R,$($param,)*)> for F
        where
            F: Fn($($param),*) -> R
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
            fn invoke(&self, _: &Jvm, args: &[Value]) -> JvmResult<Option<Value>> {
                let mut i = 0;
                self($($param::get(args, &mut i)),*).value()
            }
        }

        impl<F, $($param,)* R> NativeFn<(Jvm, R,$($param,)*)> for F
        where
            F: Fn(&Jvm, $($param),*) -> R
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
            fn invoke(&self, jvm: &Jvm, args: &[Value]) -> JvmResult<Option<Value>> {
                let mut i = 0;
                self(jvm, $($param::get(args, &mut i)),*).value()
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
impl_args!(
    T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15
);
impl_args!(
    T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16
);
