use core::ops::Not;
use core::{
    cell::SyncUnsafeCell,
    ops::Deref,
    sync::atomic::{AtomicU8, Ordering::Relaxed},
};

use crate::{
    AccessFlags,
    class::{ClassRef, ClassState},
    const_pool::{ConstPool, ConstPoolItem},
    field::Field,
    heap::Ptrs,
    jvm::{Jvm, JvmResult, throw},
    method::{Bytecode, Code, Method, MethodNaT, NativeCode},
    object::{FieldStorage, Object},
    opcodes::*,
    typ::Type,
    unreachable_verified,
    value::Value,
};

pub(crate) fn invoke(
    jvm: &Jvm,
    method: &Method,
    args: impl IntoIterator<Item = Value>,
) -> JvmResult<Option<Value>> {
    method.class().ensure_init(jvm)?;
    let ptrs = jvm.heap.ptrs();
    let mut args = args.into_iter();
    let mut arg_values = Vec::new();
    if !method.access_flags.contains(AccessFlags::STATIC) {
        match args.next() {
            Some(Value::Ref(Some(this)))
                if this.class(ptrs).assignable_to(&method.class().name) =>
            {
                arg_values.push(this.into())
            }
            _ => panic!("Invoked {method} with values of wrong type"),
        }
    }
    for (declared, value) in method.nat.typ.args.iter().zip(args) {
        match (declared, value) {
            (Type::Ref(declared), Value::Ref(object))
                if object
                    .map(|o| o.class(ptrs).assignable_to(declared))
                    .unwrap_or(true) =>
            {
                arg_values.push(object.into())
            }
            (
                Type::Boolean | Type::Byte | Type::Short | Type::Char | Type::Int,
                Value::Int(value),
            ) => arg_values.push(value.into()),
            (Type::Long, Value::Long(value)) => {
                if TWO_STACK_SLOTS_PER_LONG_AND_DOUBLES {
                    let (low, high) = IVal::from_long(value);
                    arg_values.push(low);
                    arg_values.push(high);
                } else {
                    arg_values.push(IVal(value as u64 as usize));
                }
            }
            (Type::Float, Value::Float(value)) => arg_values.push(value.into()),
            (Type::Double, Value::Double(value)) => {
                if TWO_STACK_SLOTS_PER_LONG_AND_DOUBLES {
                    let (low, high) = IVal::from_double(value);
                    arg_values.push(low);
                    arg_values.push(high);
                } else {
                    arg_values.push(IVal(value.to_bits() as usize));
                }
            }
            _ => panic!("Invoked {method} with values of wrong type"),
        }
    }
    invoke_initialized(jvm, method, &arg_values)
}

/// Called on <clinit> during class initialization
pub(crate) fn invoke_initializer(jvm: &Jvm, initializer: &Method) -> JvmResult<()> {
    invoke_initialized(jvm, initializer, &[]).map(|_| ())
}

/// Stack or local entry.
/// Doesn't store type information as it was already checked during verification.
///
/// While in the specification longs and doubles always take two slots,
/// on 64-bit systems, we only use one stack slot or them.
/// When stored in local variables they currently always take two slots
/// (though on 64 bit they only use the first one), as readjusting locals
/// is a bit tricky because slots can change type.
#[derive(Clone, Copy, PartialEq)]
#[repr(transparent)]
struct IVal(usize);

impl core::fmt::Debug for IVal {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub const TWO_STACK_SLOTS_PER_LONG_AND_DOUBLES: bool = cfg!(target_pointer_width = "32");

/// Requires that the class is already initialized, or method is the initializer
fn invoke_initialized(jvm: &Jvm, method: &Method, args: &[IVal]) -> JvmResult<Option<Value>> {
    #[cfg(not(miri))]
    {
        // Try to check for stack exhaustion. This is far from foolproof, e.g.
        // a malicious class can already cause a stack overflow during loading.
        // Also doesn't work on wasm.
        // TODO: Figure out what value is reliable without being too big
        const STACK_CHECK: usize = 30_000;
        if stacker::remaining_stack().unwrap_or(usize::MAX) < STACK_CHECK {
            return Err(throw(jvm, "StackOverflowError", None));
        }
    }

    let method_monitor = if method.access_flags.contains(AccessFlags::SYNCHRONIZED) {
        Some(if method.access_flags.contains(AccessFlags::STATIC) {
            &method.class().static_monitor
        } else {
            args[0].as_obj().unwrap().mark_word()
        })
    } else {
        None
    };

    if let Some(m) = method_monitor {
        jvm.parking_lot.monitor_enter(m);
    }

    let code = method.code.read().unwrap();
    let ret = match &*code {
        Code::None => {
            if method.access_flags.contains(AccessFlags::ABSTRACT) {
                Err(throw(jvm, "AbstractMethodError", &*format!("{method}")))
            } else if method.access_flags.contains(AccessFlags::NATIVE) {
                Err(throw(jvm, "UnsatisfiedLinkError", &*format!("{method}")))
            } else {
                unreachable!()
            }
        }
        Code::Bytecode(code) => interpret(jvm, method, code, args),
        Code::Native(code) => native(jvm, method, code, args),
    };

    if let Some(m) = method_monitor {
        if jvm.parking_lot.monitor_exit(m).is_err() {
            return Err(throw(jvm, "IllegalMonitorStateException", None));
        }
    }

    ret
}

/// Requires that the class is already initialized, or method is the initializer
fn native(
    jvm: &Jvm,
    method: &Method,
    code: &NativeCode,
    args: &[IVal],
) -> JvmResult<Option<Value>> {
    let mut values = Vec::new();
    let mut i = 0;
    if !method.access_flags.contains(AccessFlags::STATIC) {
        values.push(crate::Value::Ref(
            args[0]
                .as_obj()
                .map(|value| crate::Object::new(jvm.id, value)),
        ));
        i += 1;
    }
    for arg in &method.nat.typ.args {
        match arg {
            Type::Boolean => {
                values.push(crate::Value::Boolean(args[i].as_int() != 0));
                i += 1;
            }
            Type::Byte => {
                values.push(crate::Value::Byte(args[i].as_int() as i8));
                i += 1;
            }
            Type::Short => {
                values.push(crate::Value::Short(args[i].as_int() as i16));
                i += 1;
            }
            Type::Char => {
                values.push(crate::Value::Char(args[i].as_int() as u16));
                i += 1;
            }
            Type::Int => {
                values.push(crate::Value::Int(args[i].as_int()));
                i += 1;
            }
            Type::Long => {
                if TWO_STACK_SLOTS_PER_LONG_AND_DOUBLES {
                    values.push(crate::Value::Long(IVal::as_long(args[i], args[i + 1])));
                    i += 2;
                } else {
                    values.push(crate::Value::Long(args[i].0 as u64 as i64));
                    i += 1;
                }
            }
            Type::Float => {
                values.push(crate::Value::Float(args[i].as_float()));
                i += 1;
            }
            Type::Double => {
                if TWO_STACK_SLOTS_PER_LONG_AND_DOUBLES {
                    values.push(crate::Value::Double(IVal::as_double(args[i], args[i + 1])));
                    i += 2;
                } else {
                    values.push(crate::Value::Double(f64::from_bits(args[i].0 as u64)));
                    i += 1;
                }
            }
            Type::Ref(_) => {
                values.push(crate::Value::Ref(
                    args[i]
                        .as_obj()
                        .map(|value| crate::Object::new(jvm.id, value)),
                ));
                i += 1;
            }
        }
    }
    match code(jvm, &values) {
        Ok(ret) => {
            let returned = ret.map(|v| v.into_inner(jvm.id));
            if match (returned, &method.nat.typ.returns) {
                (Some(returned), Some(typ)) => !returned.assignable_to(jvm.heap.ptrs(), typ),
                (None, None) => false,
                _ => true,
            } {
                panic!("native method {method} returned value of wrong type");
            }
            Ok(returned)
        }
        Err(err) => Err(err.into_inner(jvm.id)),
    }
}

/// Requires that the class is already initialized, or method is the initializer
fn interpret(
    jvm: &Jvm,
    method: &Method,
    code: &Bytecode,
    args: &[IVal],
) -> JvmResult<Option<Value>> {
    debug_assert!(
        (method.nat.name.deref() == "<clinit>")
            | matches!(
                *method.class().init.lock().unwrap(),
                ClassState::Initialized
            )
    );
    // The class is already initialized, so we can hold the read lock for as long as we like
    let const_pool = method.class().const_pool.read().unwrap();

    let vars_len = code.max_locals as usize + code.max_stack as usize;
    let mut locals_and_stack = Vec::with_capacity(vars_len);
    // On 64 bit systems, longs & doubles take 1 slot on the stack but two slots on the local variables.
    if TWO_STACK_SLOTS_PER_LONG_AND_DOUBLES {
        locals_and_stack.extend_from_slice(args);
    } else {
        for (long_or_double, &value) in method
            .access_flags
            .contains(AccessFlags::STATIC)
            .not()
            .then_some(false)
            .into_iter()
            .chain(
                method
                    .nat
                    .typ
                    .args
                    .iter()
                    .map(|typ| matches!(typ, Type::Long | Type::Double)),
            )
            .zip(args)
        {
            locals_and_stack.push(value);
            if long_or_double {
                locals_and_stack.push(IVal(0));
            }
        }
    }
    while locals_and_stack.len() < vars_len {
        locals_and_stack.push(IVal(0));
    }
    let (locals, stack) = locals_and_stack.split_at_mut(code.max_locals as usize);
    let mut frame = Frame {
        code_bytes: &code.bytes,
        locals,
        stack,
        stack_depth: 0,
        pc: 0,
    };

    if !method.access_flags.contains(AccessFlags::STATIC) && args[0].0 == 0 {
        return Err(throw(jvm, "NullPointerException", None));
    }

    'run: loop {
        match run_until_exception_or_return(jvm, method, &const_pool, &mut frame) {
            Ok(result) => {
                return Ok(result);
            }
            Err(thrown) => {
                // Check whether the exception gets caught or rethrown
                for handler in &code.exception_table {
                    let in_range = (handler.start_pc..=handler.end_pc).contains(&frame.pc);
                    if in_range
                        && (handler.catch_type == 0
                            || thrown.class(jvm.heap.ptrs()).assignable_to(
                                &const_pool.get_class_verified(handler.catch_type).name,
                            ))
                    {
                        frame.clear_stack();
                        frame.push(thrown.into());
                        frame.pc = handler.handler_pc;
                        continue 'run;
                    }
                }
                return Err(thrown);
            }
        }
    }
}

#[inline(always)]
fn run_until_exception_or_return(
    jvm: &Jvm,
    method: &Method,
    const_pool: &ConstPool,
    frame: &mut Frame,
) -> JvmResult<Option<Value>> {
    let ptrs = jvm.heap.ptrs();
    loop {
        match frame.read_opcode() {
            NOP => {}
            ACONST_NULL => frame.push(IVal(0)),
            ICONST_M1 => frame.push((-1).into()),
            ICONST_0 => frame.push(0.into()),
            ICONST_1 => frame.push(1.into()),
            ICONST_2 => frame.push(2.into()),
            ICONST_3 => frame.push(3.into()),
            ICONST_4 => frame.push(4.into()),
            ICONST_5 => frame.push(5.into()),
            LCONST_0 => frame.push_long(0),
            LCONST_1 => frame.push_long(1),
            FCONST_0 => frame.push(0.0.into()),
            FCONST_1 => frame.push(1.0.into()),
            FCONST_2 => frame.push(2.0.into()),
            DCONST_0 => frame.push_double(0.0),
            DCONST_1 => frame.push_double(1.0),
            BIPUSH => {
                let value = frame.read_code_u8() as i8;
                frame.push((value as i32).into());
            }
            SIPUSH => {
                let value = frame.read_code_u16() as i16;
                frame.push((value as i32).into());
            }
            LDC => {
                let index = frame.read_code_u8() as u16;
                ldc(frame, const_pool, index);
            }
            LDC_W | LDC2_W => {
                let index = frame.read_code_u16();
                ldc(frame, const_pool, index);
            }
            ILOAD | FLOAD | ALOAD => {
                let index = frame.read_code_u8() as u16;
                frame.load_local(index as usize);
            }
            LLOAD | DLOAD => {
                let index = frame.read_code_u8() as u16;
                frame.load_local_2(index as usize);
            }
            ILOAD_0 | FLOAD_0 | ALOAD_0 => frame.load_local(0),
            ILOAD_1 | FLOAD_1 | ALOAD_1 => frame.load_local(1),
            ILOAD_2 | FLOAD_2 | ALOAD_2 => frame.load_local(2),
            ILOAD_3 | FLOAD_3 | ALOAD_3 => frame.load_local(3),
            LLOAD_0 | DLOAD_0 => frame.load_local_2(0),
            LLOAD_1 | DLOAD_1 => frame.load_local_2(1),
            LLOAD_2 | DLOAD_2 => frame.load_local_2(2),
            LLOAD_3 | DLOAD_3 => frame.load_local_2(3),
            IALOAD | FALOAD => {
                let index = frame.pop().as_int();
                let obj = frame
                    .pop()
                    .as_obj()
                    .ok_or_else(|| throw(jvm, "NullPointerException", None))?;
                frame.push(obj.array_read::<i32>(jvm, index)?.into());
            }
            LALOAD | DALOAD => {
                let index = frame.pop().as_int();
                let obj = frame
                    .pop()
                    .as_obj()
                    .ok_or_else(|| throw(jvm, "NullPointerException", None))?;
                frame.push_long(obj.array_read::<i64>(jvm, index)?);
            }
            AALOAD => {
                let index = frame.pop().as_int();
                let obj = frame
                    .pop()
                    .as_obj()
                    .ok_or_else(|| throw(jvm, "NullPointerException", None))?;
                frame.push(IVal(obj.array_read::<usize>(jvm, index)?));
            }
            BALOAD => {
                let index = frame.pop().as_int();
                let obj = frame
                    .pop()
                    .as_obj()
                    .ok_or_else(|| throw(jvm, "NullPointerException", None))?;
                frame.push((obj.array_read::<i8>(jvm, index)? as i32).into());
            }
            CALOAD => {
                let index = frame.pop().as_int();
                let obj = frame
                    .pop()
                    .as_obj()
                    .ok_or_else(|| throw(jvm, "NullPointerException", None))?;
                let tmp = obj.array_read::<i16>(jvm, index)?;
                frame.push((tmp as u16 as i32).into());
            }
            SALOAD => {
                let index = frame.pop().as_int();
                let obj = frame
                    .pop()
                    .as_obj()
                    .ok_or_else(|| throw(jvm, "NullPointerException", None))?;
                frame.push((obj.array_read::<i16>(jvm, index)? as i32).into());
            }
            ISTORE | FSTORE | ASTORE => {
                let index = frame.read_code_u8() as u16;
                let value = frame.pop();
                frame.set_local(index as usize, value);
            }
            LSTORE | DSTORE => {
                let index = frame.read_code_u8() as u16;
                if TWO_STACK_SLOTS_PER_LONG_AND_DOUBLES {
                    let value_1 = frame.pop();
                    let value_2 = frame.pop();
                    frame.set_local(index as usize + 1, value_1);
                    frame.set_local(index as usize, value_2);
                } else {
                    let value = frame.pop();
                    frame.set_local(index as usize, value);
                }
            }
            ISTORE_0 | FSTORE_0 | ASTORE_0 => frame.locals[0] = frame.pop(),
            ISTORE_1 | FSTORE_1 | ASTORE_1 => frame.locals[1] = frame.pop(),
            ISTORE_2 | FSTORE_2 | ASTORE_2 => frame.locals[2] = frame.pop(),
            ISTORE_3 | FSTORE_3 | ASTORE_3 => frame.locals[3] = frame.pop(),
            LSTORE_0 | DSTORE_0 => {
                if TWO_STACK_SLOTS_PER_LONG_AND_DOUBLES {
                    frame.locals[1] = frame.pop();
                    frame.locals[0] = frame.pop();
                } else {
                    frame.locals[0] = frame.pop();
                }
            }
            LSTORE_1 | DSTORE_1 => {
                if TWO_STACK_SLOTS_PER_LONG_AND_DOUBLES {
                    frame.locals[2] = frame.pop();
                    frame.locals[1] = frame.pop();
                } else {
                    frame.locals[1] = frame.pop();
                }
            }
            LSTORE_2 | DSTORE_2 => {
                if TWO_STACK_SLOTS_PER_LONG_AND_DOUBLES {
                    frame.locals[3] = frame.pop();
                    frame.locals[2] = frame.pop();
                } else {
                    frame.locals[2] = frame.pop();
                }
            }
            LSTORE_3 | DSTORE_3 => {
                if TWO_STACK_SLOTS_PER_LONG_AND_DOUBLES {
                    frame.locals[4] = frame.pop();
                    frame.locals[3] = frame.pop();
                } else {
                    frame.locals[3] = frame.pop();
                }
            }
            IASTORE | FASTORE => {
                let value = frame.pop().as_int();
                let index = frame.pop().as_int();
                let obj = frame
                    .pop()
                    .as_obj()
                    .ok_or_else(|| throw(jvm, "NullPointerException", None))?;
                obj.array_write::<i32>(jvm, index, value)?
            }
            LASTORE | DASTORE => {
                let value = frame.pop_long();
                let index = frame.pop().as_int();
                let obj = frame
                    .pop()
                    .as_obj()
                    .ok_or_else(|| throw(jvm, "NullPointerException", None))?;
                obj.array_write::<i64>(jvm, index, value)?
            }
            AASTORE => {
                let value = frame.pop();
                let index = frame.pop().as_int();
                let obj = frame
                    .pop()
                    .as_obj()
                    .ok_or_else(|| throw(jvm, "NullPointerException", None))?;
                let Some(Type::Ref(component)) = &obj.class(ptrs).element_type else {
                    unreachable_verified()
                };
                // Yay for covariant arrays, another consequence of Java launching without generics…
                if value
                    .as_obj()
                    .map(|element| !element.class(ptrs).assignable_to(component))
                    .unwrap_or(false)
                {
                    return Err(throw(
                        jvm,
                        "ArrayStoreException",
                        &*format!(
                            "Trying to store {} into array of {component}",
                            obj.class(ptrs).name
                        ),
                    ));
                }
                obj.array_write::<usize>(jvm, index, value.0)?
            }
            BASTORE => {
                let value = frame.pop().as_int();
                let index = frame.pop().as_int();
                let obj = frame
                    .pop()
                    .as_obj()
                    .ok_or_else(|| throw(jvm, "NullPointerException", None))?;
                obj.array_write::<i8>(jvm, index, value as u8 as i8)?
            }
            CASTORE => {
                let value = frame.pop().as_int();
                let index = frame.pop().as_int();
                let obj = frame
                    .pop()
                    .as_obj()
                    .ok_or_else(|| throw(jvm, "NullPointerException", None))?;
                obj.array_write::<i16>(jvm, index, value as u16 as i16)?
            }
            SASTORE => {
                let value = frame.pop().as_int();
                let index = frame.pop().as_int();
                let obj = frame
                    .pop()
                    .as_obj()
                    .ok_or_else(|| throw(jvm, "NullPointerException", None))?;
                obj.array_write::<i16>(jvm, index, value as u16 as i16)?
            }
            POP => {
                frame.pop();
            }
            POP2 => {
                frame.pop();
                frame.pop();
            }
            DUP => {
                let value = frame.pop();
                frame.push(value);
                frame.push(value);
            }
            DUP_X1 => {
                let first = frame.pop();
                let second = frame.pop();
                frame.push(first);
                frame.push(second);
                frame.push(first);
            }
            DUP_X2 => {
                let first = frame.pop();
                let second = frame.pop();
                let third = frame.pop();
                frame.push(first);
                frame.push(third);
                frame.push(second);
                frame.push(first);
            }
            DUP2 => {
                let first = frame.pop();
                let second = frame.pop();
                frame.push(second);
                frame.push(first);
                frame.push(second);
                frame.push(first);
            }
            DUP2_X1 => {
                let first = frame.pop();
                let second = frame.pop();
                let third = frame.pop();
                frame.push(second);
                frame.push(first);
                frame.push(third);
                frame.push(second);
                frame.push(first);
            }
            DUP2_X2 => {
                let first = frame.pop();
                let second = frame.pop();
                let third = frame.pop();
                let fourth = frame.pop();
                frame.push(second);
                frame.push(first);
                frame.push(fourth);
                frame.push(third);
                frame.push(second);
                frame.push(first);
            }
            SWAP => {
                let first = frame.pop();
                let second = frame.pop();
                frame.push(first);
                frame.push(second);
            }
            IADD | ISUB | IMUL | IDIV | IREM | ISHL | ISHR | IUSHR | IAND | IOR | IXOR => {
                let b = frame.pop().as_int();
                let a = frame.pop().as_int();
                let result = match frame.read_previous_byte() {
                    IADD => a.wrapping_add(b),
                    ISUB => a.wrapping_sub(b),
                    IMUL => a.wrapping_mul(b),
                    IDIV => a
                        .checked_div(b)
                        .ok_or_else(|| throw(jvm, "ArithmeticException", "division by zero"))?,
                    IREM => {
                        if b != 0 {
                            a % b
                        } else {
                            return Err(throw(jvm, "ArithmeticException", "division by zero"));
                        }
                    }
                    ISHL => a << (b & 0x1f),
                    ISHR => a >> (b & 0x1f),
                    IUSHR => (a as u32 >> (b & 0x1f)) as i32,
                    IAND => a & b,
                    IOR => a | b,
                    IXOR => a ^ b,
                    _ => unreachable_verified(),
                };
                frame.push(result.into());
            }
            LADD | LSUB | LMUL | LDIV | LREM | LSHL | LSHR | LUSHR | LAND | LOR | LXOR => {
                let b = frame.pop_long();
                let a = frame.pop_long();
                let result = match frame.read_previous_byte() {
                    LADD => a.wrapping_add(b),
                    LSUB => a.wrapping_sub(b),
                    LMUL => a.wrapping_mul(b),
                    LDIV => a
                        .checked_div(b)
                        .ok_or_else(|| throw(jvm, "ArithmeticException", "division by zero"))?,
                    LREM => {
                        if b != 0 {
                            a % b
                        } else {
                            return Err(throw(jvm, "ArithmeticException", "division by zero"));
                        }
                    }
                    LSHL => a << (b & 0x3f),
                    LSHR => a >> (b & 0x3f),
                    LUSHR => (a as u64 >> (b & 0x3f)) as i64,
                    LAND => a & b,
                    LOR => a | b,
                    LXOR => a ^ b,
                    _ => unreachable_verified(),
                };
                frame.push_long(result);
            }
            FADD | FSUB | FMUL | FDIV | FREM => {
                let b = frame.pop().as_float();
                let a = frame.pop().as_float();
                let result = match frame.read_previous_byte() {
                    FADD => a + b,
                    FSUB => a - b,
                    FMUL => a * b,
                    FDIV => a / b,
                    FREM => a % b,
                    _ => unreachable_verified(),
                };
                frame.push(result.into());
            }
            DADD | DSUB | DMUL | DDIV | DREM => {
                let b = frame.pop_double();
                let a = frame.pop_double();
                let result = match frame.read_previous_byte() {
                    DADD => a + b,
                    DSUB => a - b,
                    DMUL => a * b,
                    DDIV => a / b,
                    DREM => a % b,
                    _ => unreachable_verified(),
                };
                frame.push_double(result);
            }
            INEG => {
                let result = -frame.pop().as_int();
                frame.push(result.into());
            }
            LNEG => {
                let result = -frame.pop_long();
                frame.push_long(result);
            }
            FNEG => {
                let result = -frame.pop().as_float();
                frame.push(result.into());
            }
            DNEG => {
                let result = -frame.pop_double();
                frame.push_double(result);
            }
            IINC => {
                let index = frame.read_code_u8() as u16;
                let constant = frame.read_code_u8() as i32;
                frame.locals[index as usize] =
                    (frame.locals[index as usize].as_int() + constant).into();
            }
            I2L => {
                let value = frame.pop().as_int();
                frame.push_long(value as i64);
            }
            I2F => {
                let value = frame.pop().as_int() as f32;
                frame.push(value.into());
            }
            I2D => {
                let value = frame.pop().as_int();
                frame.push_double(value as f64);
            }
            L2I => {
                let value = frame.pop_long() as i32;
                frame.push(value.into());
            }
            L2F => {
                let value = frame.pop_long() as f32;
                frame.push(value.into());
            }
            L2D => {
                let value = frame.pop_long();
                frame.push_double(value as f64);
            }
            F2I => {
                let value = frame.pop().as_float() as i32;
                frame.push(value.into());
            }
            F2L => {
                let value = frame.pop().as_float();
                frame.push_long(value as i64);
            }
            F2D => {
                let value = frame.pop().as_float();
                frame.push_double(value as f64);
            }
            D2I => {
                let value = frame.pop_double() as i32;
                frame.push(value.into());
            }
            D2L => {
                let value = frame.pop_double();
                frame.push_long(value as i64);
            }
            D2F => {
                let value = frame.pop_double() as f32;
                frame.push(value.into());
            }
            I2B => {
                let value = frame.pop().as_int() as i8 as i32;
                frame.push(value.into());
            }
            I2C => {
                let value = frame.pop().as_int() as u16 as i32;
                frame.push(value.into());
            }
            I2S => {
                let value = frame.pop().as_int() as i16 as i32;
                frame.push(value.into());
            }
            LCMP => {
                let b = frame.pop_long();
                let a = frame.pop_long();
                frame.push(
                    match a - b {
                        x if x > 0 => 1,
                        0 => 0,
                        _ => -1,
                    }
                    .into(),
                )
            }
            FCMPL | FCMPG => {
                let b = frame.pop().as_float();
                let a = frame.pop().as_float();
                let result = if b.is_nan() | a.is_nan() {
                    if frame.read_previous_byte() == FCMPL {
                        -1
                    } else {
                        1
                    }
                } else if b > a {
                    -1
                } else {
                    i32::from(b != a)
                };
                frame.push(result.into())
            }
            DCMPL | DCMPG => {
                let b = frame.pop_double();
                let a = frame.pop_double();
                let result = if b.is_nan() | a.is_nan() {
                    if frame.read_previous_byte() == DCMPL {
                        -1
                    } else {
                        1
                    }
                } else if b > a {
                    -1
                } else {
                    i32::from(b != a)
                };
                frame.push(result.into())
            }
            IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE | IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT
            | IF_ICMPGE | IF_ICMPGT | IF_ICMPLE => {
                let base = frame.pc - 1;
                let (op, two_operand) = {
                    let instr = frame.read_previous_byte();
                    if instr > IFLE {
                        (instr - 6, true)
                    } else {
                        (instr, false)
                    }
                };
                let b = if two_operand { frame.pop().as_int() } else { 0 };
                let a = frame.pop().as_int();
                let target_offset = frame.read_code_u16() as i16;
                if match op {
                    IFEQ => a == b,
                    IFNE => a != b,
                    IFLT => a < b,
                    IFGE => a >= b,
                    IFGT => a > b,
                    IFLE => a <= b,
                    _ => unreachable_verified(),
                } {
                    frame.jump_relative(base, target_offset as i32);
                }
            }
            IF_ACMPEQ => {
                let base = frame.pc - 1;
                let target_offset = frame.read_code_u16() as i16;
                if frame.pop() == frame.pop() {
                    frame.jump_relative(base, target_offset as i32);
                }
            }
            IF_ACMPNE => {
                let base = frame.pc - 1;
                let target_offset = frame.read_code_u16() as i16;
                if frame.pop() != frame.pop() {
                    frame.jump_relative(base, target_offset as i32);
                }
            }
            GOTO => {
                let base = frame.pc - 1;
                let target_offset = frame.read_code_u16() as i16;
                frame.jump_relative(base, target_offset as i32)
            }
            TABLESWITCH => {
                let base = frame.pc - 1;

                // Skip up to 3 byte padding
                frame.pc += 3;
                frame.pc &= 0b1111111111111000;

                let index = frame.pop().as_int();
                let default = frame.read_code_u32() as i32;
                let low = frame.read_code_u32() as i32;
                let high = frame.read_code_u32() as i32;

                let target = if (index < low) | (index >= high) {
                    default
                } else {
                    frame.pc += (index - low) as u16 * 4;
                    frame.read_code_u32() as i32
                };
                frame.jump_relative(base, target);
            }
            LOOKUPSWITCH => {
                let base = frame.pc - 1;

                // Skip up to 3 byte padding
                frame.pc += 3;
                frame.pc &= 0b1111111111111000;

                let key = frame.pop().as_int();
                let default = frame.read_code_u32() as i32;
                let npairs = frame.read_code_u32();

                // TODO: binary search
                let mut target = default;
                for _ in 0..npairs {
                    let compare = frame.read_code_u32() as i32;
                    let value = frame.read_code_u32() as i32;
                    if key == compare {
                        target = value;
                        break;
                    }
                }
                frame.jump_relative(base, target);
            }
            IRETURN => return Ok(Some(frame.pop().as_int().into())),
            LRETURN => return Ok(Some(frame.pop_long().into())),
            FRETURN => return Ok(Some(frame.pop().as_float().into())),
            DRETURN => return Ok(Some(frame.pop_double().into())),
            ARETURN => return Ok(Some(frame.pop().as_obj().into())),
            RETURN => return Ok(None),
            GETSTATIC => {
                let index = frame.read_code_u16();
                let field = const_pool.get_field_verified(index);
                field.class().ensure_init(jvm)?;
                frame.pc -= 3;
                frame.set_opcode(GETSTATIC_INITIALIZED);
            }
            GETSTATIC_INITIALIZED => {
                let index = frame.read_code_u16();
                let field = const_pool.get_field_verified(index);
                get_field(ptrs, frame, field, field.class().static_storage);
            }
            PUTSTATIC => {
                let index = frame.read_code_u16();
                let field = const_pool.get_field_verified(index);
                field.class().ensure_init(jvm)?;
                frame.pc -= 3;
                frame.set_opcode(PUTSTATIC_INITIALIZED);
            }
            PUTSTATIC_INITIALIZED => {
                let index = frame.read_code_u16();
                let field = const_pool.get_field_verified(index);
                put_field(ptrs, frame, field, field.class().static_storage);
            }
            GETFIELD => {
                let index = frame.read_code_u16();
                let field = const_pool.get_field_verified(index);
                let object = frame
                    .pop()
                    .as_obj()
                    .ok_or_else(|| throw(jvm, "NullPointerException", None))?;
                get_field(ptrs, frame, field, object.fields());
            }
            PUTFIELD => {
                let index = frame.read_code_u16();
                let field = const_pool.get_field_verified(index);
                // Stack has object ref first, then value on top
                // So maybe inline `put_field`?
                let object = if TWO_STACK_SLOTS_PER_LONG_AND_DOUBLES
                    && matches!(field.nat.typ, Type::Long | Type::Double)
                {
                    let high = frame.pop();
                    let low = frame.pop();
                    let obj = frame.pop().as_obj();
                    frame.push(low);
                    frame.push(high);
                    obj
                } else {
                    let value = frame.pop();
                    let obj = frame.pop().as_obj();
                    frame.push(value);
                    obj
                }
                .ok_or_else(|| throw(jvm, "NullPointerException", None))?;
                put_field(ptrs, frame, field, object.fields());
            }
            INVOKEVIRTUAL => {
                let index = frame.read_code_u16();
                let (_, nat) = const_pool.get_virtual_method_verified(index);
                let obj_index = frame.stack_depth - nat.typ.arg_slots() - 1;
                let obj = frame.stack[obj_index]
                    .as_obj()
                    .ok_or_else(|| throw(jvm, "NullPointerException", None))?;
                // TODO: move method linking (incl NoSuchMethodError and IncompatibleClassChangeError) to class init
                let invoke_method = obj
                    .class(ptrs)
                    .methods
                    .get(nat)
                    .ok_or_else(|| throw(jvm, "NoSuchMethodError", None))?;
                if invoke_method.access_flags.contains(AccessFlags::STATIC) {
                    return Err(throw(jvm, "IncompatibleClassChangeError", None));
                }
                execute_invoke_instr(jvm, frame, invoke_method)?;
            }
            INVOKESPECIAL => {
                let index = frame.read_code_u16();
                let (named_class, nat) = const_pool.get_virtual_method_verified(index);

                let is_instance_init = nat.name.as_ref() == "<init>";

                let class =
                    if !is_instance_init & method.class().true_subclass_of(&named_class.name) {
                        method.class().super_class.unwrap()
                    } else {
                        named_class
                    };

                // TODO: move method linking (incl NoSuchMethodError and IncompatibleClassChangeError) to class init
                let invoke_method = class
                    .methods
                    .get(nat)
                    .ok_or_else(|| throw(jvm, "NoSuchMethodError", None))?;
                if is_instance_init && invoke_method.class() != named_class {
                    return Err(throw(jvm, "NoSuchMethodError", None));
                }
                if invoke_method.access_flags.contains(AccessFlags::STATIC) {
                    return Err(throw(jvm, "IncompatibleClassChangeError", None));
                }
                execute_invoke_instr(jvm, frame, invoke_method)?;
            }
            INVOKESTATIC => {
                let index = frame.read_code_u16();
                let invoke_method = const_pool.get_static_method_verified(index);
                invoke_method.class().ensure_init(jvm)?;
                frame.pc -= 3;
                frame.set_opcode(INVOKESTATIC_INITIALIZED);
            }
            INVOKESTATIC_INITIALIZED => {
                let index = frame.read_code_u16();
                let invoke_method = const_pool.get_static_method_verified(index);
                execute_invoke_instr(jvm, frame, invoke_method)?;
            }
            INVOKEINTERFACE => {
                let index = frame.read_code_u16();
                let (interface, nat) = const_pool.get_virtual_method_verified(index);
                let obj_index = frame.stack_depth - nat.typ.arg_slots() - 1;
                let obj = frame.stack[obj_index]
                    .as_obj()
                    .ok_or_else(|| throw(jvm, "NullPointerException", None))?;
                if !obj.class(ptrs).interfaces.contains(&interface) {
                    return Err(throw(jvm, "IncompatibleClassChangeError", None));
                }
                if let Some(invoke_method) = obj.class(ptrs).methods.get(nat) {
                    execute_invoke_instr(jvm, frame, invoke_method)?;
                }
                // TODO: Do this search at load time
                let mut default = None;
                let mut multiple_found = false;
                fn iter_interfaces(
                    in_class: ClassRef,
                    nat: &MethodNaT,
                    default: &mut Option<&Method>,
                    multiple_found: &mut bool,
                ) {
                    for interface in &in_class.interfaces {
                        match interface.methods.get(nat) {
                            Some(found)
                                if !found
                                    .access_flags
                                    .intersects(AccessFlags::ABSTRACT | AccessFlags::PRIVATE) =>
                            {
                                if default.is_none() {
                                    *default = Some(found);
                                } else {
                                    *multiple_found = true;
                                }
                            }
                            _ => iter_interfaces(interface, nat, default, multiple_found),
                        }
                    }
                }
                iter_interfaces(obj.class(ptrs), nat, &mut default, &mut multiple_found);
                match default {
                    Some(default) if !multiple_found => {
                        execute_invoke_instr(jvm, frame, default)?;
                    }
                    _ if multiple_found => {
                        return Err(throw(jvm, "IncompatibleClassChangeError", None));
                    }
                    _ => return Err(throw(jvm, "AbstractMethodError", None)),
                }
            }
            NEW => {
                let index = frame.read_code_u16();
                let class = const_pool.get_class_verified(index);
                class.ensure_init(jvm)?;
                frame.pc -= 3;
                frame.set_opcode(NEW_INITIALIZED);
            }
            NEW_INITIALIZED => {
                let index = frame.read_code_u16();
                let class = const_pool.get_class_verified(index);
                frame.push(jvm.create_object_uninitialized(class).into())
            }
            NEWARRAY => {
                let length = frame.pop().as_int();
                if length < 0 {
                    return Err(throw(jvm, "NegativeArraySizeException", None));
                }
                let component = match frame.read_code_u8() {
                    4 => Type::Boolean,
                    5 => Type::Char,
                    6 => Type::Float,
                    7 => Type::Double,
                    8 => Type::Byte,
                    9 => Type::Short,
                    10 => Type::Int,
                    11 => Type::Long,
                    _ => unreachable_verified(),
                };
                frame.push(jvm.create_array_of(&component, length).into());
            }
            ANEWARRAY => {
                let length = frame.pop().as_int();
                let index = frame.read_code_u16();
                let component = const_pool.get_class_verified(index);
                let class = jvm.resolve_array_of(&Type::Ref(component.name.clone()))?;
                if length < 0 {
                    return Err(throw(jvm, "NegativeArraySizeException", None));
                }
                frame.push(Object::new_array(&jvm.heap, class, length).into());
            }
            ARRAYLENGTH => {
                let obj = frame
                    .pop()
                    .as_obj()
                    .ok_or_else(|| throw(jvm, "NullPointerException", None))?;
                frame.push(obj.array_length().into());
            }
            ATHROW => {
                let obj = frame
                    .pop()
                    .as_obj()
                    .ok_or_else(|| throw(jvm, "NullPointerException", None))?;
                return Err(obj);
            }
            CHECKCAST => {
                let index = frame.read_code_u16();
                let check_class = const_pool.get_class_verified(index);
                if let Some(object) = frame.peek().as_obj() {
                    if !object.class(ptrs).assignable_to(&check_class.name) {
                        frame.pop();
                        return Err(throw(jvm, "ClassCastException", None));
                    }
                }
            }
            INSTANCEOF => {
                let index = frame.read_code_u16();
                let check_class = const_pool.get_class_verified(index);
                if let Some(object) = frame.pop().as_obj() {
                    if object.class(ptrs).assignable_to(&check_class.name) {
                        frame.push(IVal(1));
                    } else {
                        frame.push(IVal(0));
                    }
                } else {
                    frame.push(IVal(0));
                }
            }
            MONITORENTER => {
                let obj = frame
                    .pop()
                    .as_obj()
                    .ok_or_else(|| throw(jvm, "NullPointerException", None))?;

                jvm.parking_lot.monitor_enter(obj.mark_word());
            }
            MONITOREXIT => {
                let obj = frame
                    .pop()
                    .as_obj()
                    .ok_or_else(|| throw(jvm, "NullPointerException", None))?;
                if jvm.parking_lot.monitor_exit(obj.mark_word()).is_err() {
                    return Err(throw(jvm, "IllegalMonitorStateException", None));
                }
            }
            WIDE => {
                let op = frame.read_code_u8();
                let index = frame.read_code_u16();
                match op {
                    ILOAD | FLOAD | ALOAD => {
                        frame.load_local(index as usize);
                    }
                    LLOAD | DLOAD => {
                        frame.load_local_2(index as usize);
                    }
                    ISTORE | FSTORE | ASTORE => {
                        let value = frame.pop();
                        frame.set_local(index as usize, value);
                    }
                    LSTORE | DSTORE => {
                        let value_1 = frame.pop();
                        let value_2 = frame.pop();
                        frame.set_local(index as usize + 1, value_1);
                        frame.set_local(index as usize, value_2);
                    }
                    IINC => {
                        let constant = frame.read_code_u16() as i32;
                        frame.locals[index as usize] =
                            (frame.locals[index as usize].as_int() + constant).into();
                    }
                    _ => unreachable_verified(),
                }
            }
            MULTIANEWARRAY => {
                let index = frame.read_code_u16();
                let dimensions = frame.read_code_u8();
                let class = const_pool.get_class(jvm, index)?;
                fn create_array(jvm: &Jvm, dimensions: &[IVal], class: ClassRef) -> Object {
                    let len = dimensions[0].as_int();
                    let array = Object::new_array(&jvm.heap, class, len);
                    if let Some(Type::Ref(element_type)) = &class.element_type {
                        let class = jvm.resolve_class_internal(element_type).unwrap();
                        for i in 0..len {
                            let _ = array.array_write::<usize>(
                                jvm,
                                i,
                                create_array(jvm, &dimensions[1..], class).0.get(),
                            );
                        }
                    }
                    array
                }
                let array = create_array(
                    jvm,
                    &frame.stack[frame.stack_depth - dimensions as usize..],
                    class,
                );
                frame.truncate_stack_by(dimensions as usize);
                frame.push(array.into());
            }
            IFNULL => {
                let base = frame.pc - 1;
                let target_offset = frame.read_code_u16() as i16;
                if frame.pop().0 == 0 {
                    frame.jump_relative(base, target_offset as i32);
                }
            }
            IFNONNULL => {
                let base = frame.pc - 1;
                let target_offset = frame.read_code_u16() as i16;
                if frame.pop().0 != 0 {
                    frame.jump_relative(base, target_offset as i32);
                }
            }
            GOTO_W => {
                let base = frame.pc - 1;
                let target_offset = frame.read_code_u32() as i32;
                frame.jump_relative(base, target_offset)
            }
            other => todo!("Bytecode: {}", other),
        }
    }
}

fn ldc(frame: &mut Frame, const_pool: &ConstPool, index: u16) {
    match const_pool.items.get(index as usize) {
        Some(&ConstPoolItem::Integer(i)) => frame.push(i.into()),
        Some(&ConstPoolItem::Float(f)) => frame.push(f.into()),
        Some(ConstPoolItem::Long(l)) => frame.push_long(*l),
        Some(ConstPoolItem::Double(d)) => frame.push_double(*d),
        Some(&ConstPoolItem::String(object)) => frame.push(object.into()),
        Some(ConstPoolItem::Class(_)) => todo!("Class objects"),
        _ => unreachable_verified(),
    }
}

fn get_field(ptrs: Ptrs, frame: &mut Frame, field: &Field, storage: FieldStorage) {
    let volatile = field.access_flags.contains(AccessFlags::VOLATILE);
    match field.nat.typ {
        Type::Boolean | Type::Byte => {
            frame.push((storage.read::<i8>(ptrs, field.byte_offset, volatile) as i32).into())
        }
        Type::Short | Type::Char => {
            frame.push((storage.read::<i16>(ptrs, field.byte_offset, volatile) as i32).into())
        }
        Type::Int => frame.push(
            storage
                .read::<i32>(ptrs, field.byte_offset, volatile)
                .into(),
        ),
        Type::Float => frame.push(
            storage
                .read::<f32>(ptrs, field.byte_offset, volatile)
                .into(),
        ),
        Type::Long => frame.push_long(storage.read::<i64>(ptrs, field.byte_offset, volatile)),
        Type::Double => frame.push_double(storage.read::<f64>(ptrs, field.byte_offset, volatile)),
        Type::Ref(..) => frame.push(IVal(storage.read::<usize>(
            ptrs,
            field.byte_offset,
            volatile,
        ))),
    }
}

fn put_field(ptrs: Ptrs, frame: &mut Frame, field: &Field, storage: FieldStorage) {
    let volatile = field.access_flags.contains(AccessFlags::VOLATILE);
    match field.nat.typ {
        Type::Boolean | Type::Byte => storage.write::<i8>(
            ptrs,
            field.byte_offset,
            frame.pop().as_int() as i8,
            volatile,
        ),
        Type::Short | Type::Char => storage.write::<i16>(
            ptrs,
            field.byte_offset,
            frame.pop().as_int() as i16,
            volatile,
        ),
        Type::Int => storage.write::<i32>(ptrs, field.byte_offset, frame.pop().as_int(), volatile),
        Type::Float => {
            storage.write::<f32>(ptrs, field.byte_offset, frame.pop().as_float(), volatile)
        }
        Type::Long => storage.write::<i64>(ptrs, field.byte_offset, frame.pop_long(), volatile),
        Type::Double => storage.write::<f64>(ptrs, field.byte_offset, frame.pop_double(), volatile),
        Type::Ref(..) => storage.write::<usize>(ptrs, field.byte_offset, frame.pop().0, volatile),
    }
}

fn execute_invoke_instr(jvm: &Jvm, frame: &mut Frame, method: &Method) -> JvmResult<()> {
    let mut arg_slots = method.nat.typ.arg_slots();
    if !method.access_flags.contains(AccessFlags::STATIC) {
        // `this` is treated as an argument, but not mentioned in method descriptor
        arg_slots += 1;
    }

    let args = &frame.stack[frame.stack_depth - arg_slots..];
    let result = invoke_initialized(jvm, method, args)?;
    frame.truncate_stack_by(arg_slots);
    if let Some(value) = result {
        match value {
            Value::Ref(object) => frame.push(object.into()),
            Value::Int(value) => frame.push(value.into()),
            Value::Float(value) => frame.push(value.into()),
            Value::Long(value) => frame.push_long(value),
            Value::Double(value) => frame.push_double(value),
        };
    }
    Ok(())
}

impl From<i32> for IVal {
    fn from(value: i32) -> Self {
        Self(value as u32 as usize)
    }
}

impl From<f32> for IVal {
    fn from(value: f32) -> Self {
        Self(value.to_bits() as usize)
    }
}

impl From<Object> for IVal {
    fn from(value: Object) -> Self {
        Self(value.0.get())
    }
}

impl From<Option<Object>> for IVal {
    fn from(value: Option<Object>) -> Self {
        Self(match value {
            None => 0,
            Some(object) => object.0.get(),
        })
    }
}

impl IVal {
    /// Returns (low half, high half)
    fn from_long(value: i64) -> (Self, Self) {
        (
            Self((value as u64 & 0xFFFFFFFF) as usize),
            Self((value as u64 >> 32) as usize),
        )
    }

    /// Returns (low half, high half)
    fn from_double(value: f64) -> (Self, Self) {
        Self::from_long(value.to_bits() as i64)
    }

    fn as_int(self) -> i32 {
        self.0 as i32
    }

    fn as_float(self) -> f32 {
        #[allow(clippy::unnecessary_cast)]
        f32::from_bits(self.0 as u32)
    }

    fn as_long(low: IVal, high: IVal) -> i64 {
        (low.0 as u64 + ((high.0 as u64) << 32)) as i64
    }

    fn as_double(low: IVal, high: IVal) -> f64 {
        f64::from_bits(low.0 as u64 + ((high.0 as u64) << 32))
    }

    fn as_obj(self) -> Option<Object> {
        Object::from_addr(self.0)
    }
}

struct Frame<'a> {
    code_bytes: &'a [SyncUnsafeCell<u8>],
    locals: &'a mut [IVal],
    stack: &'a mut [IVal],
    stack_depth: usize,
    pc: u16,
}

impl Frame<'_> {
    fn jump_relative(&mut self, base: u16, target_offset: i32) {
        self.pc = (base as i32 + target_offset) as u16;
    }

    #[inline(always)]
    fn clear_stack(&mut self) {
        self.stack_depth = 0;
    }

    #[inline(always)]
    fn truncate_stack_by(&mut self, by: usize) {
        self.stack_depth -= by;
    }

    #[inline(always)]
    fn peek(&mut self) -> IVal {
        unsafe { *self.stack.get_unchecked(self.stack_depth - 1) }
    }

    #[inline(always)]
    fn push(&mut self, val: IVal) {
        if let Some(slot) = self.stack.get_mut(self.stack_depth) {
            *slot = val;
            self.stack_depth += 1;
        } else {
            unreachable_verified()
        }
    }

    #[inline(always)]
    fn pop(&mut self) -> IVal {
        self.stack_depth -= 1;
        unsafe { *self.stack.get_unchecked(self.stack_depth) }
    }

    #[inline(always)]
    fn pop_long(&mut self) -> i64 {
        if TWO_STACK_SLOTS_PER_LONG_AND_DOUBLES {
            let (high, low) = (self.pop(), self.pop());
            IVal::as_long(low, high)
        } else {
            self.pop().0 as i64
        }
    }

    #[inline(always)]
    fn pop_double(&mut self) -> f64 {
        f64::from_bits(self.pop_long() as u64)
    }

    #[inline(always)]
    fn push_long(&mut self, value: i64) {
        if TWO_STACK_SLOTS_PER_LONG_AND_DOUBLES {
            let (low, high) = IVal::from_long(value);
            self.push(low);
            self.push(high);
        } else {
            self.push(IVal(value as usize));
        }
    }

    #[inline(always)]
    fn push_double(&mut self, value: f64) {
        self.push_long(value.to_bits() as i64)
    }

    fn load_local(&mut self, index: usize) {
        self.push(self.local(index));
    }

    fn load_local_2(&mut self, index: usize) {
        self.push(self.local(index));
        if TWO_STACK_SLOTS_PER_LONG_AND_DOUBLES {
            self.push(self.local(index + 1));
        }
    }

    #[inline(always)]
    fn local(&self, index: usize) -> IVal {
        debug_assert!(index < self.locals.len());
        unsafe { *self.locals.get_unchecked(index) }
    }

    #[inline(always)]
    fn set_local(&mut self, index: usize, value: IVal) {
        debug_assert!(index < self.locals.len());
        unsafe { *self.locals.get_unchecked_mut(index) = value }
    }

    #[inline(always)]
    fn read_opcode(&mut self) -> u8 {
        debug_assert!(self.code_bytes.len() >= self.pc as usize);
        // Atomic read to make `set_opcode` possible.
        let byte = unsafe {
            (self.code_bytes.as_ptr().wrapping_byte_add(self.pc as usize) as *const AtomicU8)
                .as_ref_unchecked()
        }
        .load(Relaxed);
        self.pc += 1;
        byte
    }

    /// Overwrite the opcode at the current pc.
    /// This works because opcode (unlike operation arguments) reads are atomic.
    fn set_opcode(&mut self, new: u8) {
        unsafe {
            (self.code_bytes.get_unchecked(self.pc as usize) as *const _ as *const AtomicU8)
                .as_ref_unchecked()
        }
        .store(new, Relaxed);
    }

    #[inline(always)]
    fn read_code_u8(&mut self) -> u8 {
        debug_assert!(self.code_bytes.len() >= self.pc as usize);
        let byte = unsafe { *self.code_bytes.get_unchecked(self.pc as usize).get() };
        self.pc += 1;
        byte
    }

    #[inline(always)]
    fn read_code_u16(&mut self) -> u16 {
        ((self.read_code_u8() as u16) << 8) + self.read_code_u8() as u16
    }

    fn read_code_u32(&mut self) -> u32 {
        ((self.read_code_u16() as u32) << 16) + self.read_code_u16() as u32
    }

    fn read_previous_byte(&mut self) -> u8 {
        unsafe { *self.code_bytes.get_unchecked(self.pc as usize - 1).get() }
    }
}
