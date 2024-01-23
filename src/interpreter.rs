use crate::{
    class::ClassPtr,
    const_pool::{ConstPool, ConstPoolItem},
    field::Field,
    field_storage::FieldStorage,
    heap::{JVMPtr, NULL_PTR},
    instructions::*,
    jvm::exception,
    jvm::{JVMResult, Jvm, Value},
    method::Method,
    object::Object,
    typ::Typ,
    AccessFlags,
};

pub(crate) fn invoke(
    jvm: &Jvm,
    method: &Method,
    args: impl IntoIterator<Item = Value>,
) -> JVMResult<Value> {
    method.class().ensure_init(jvm)?;
    let mut arg_values = Vec::new();
    for arg in args {
        match arg {
            Value::Void => panic!("invoke with void argument"),
            Value::Ref(object) => arg_values.push(object.into()),
            Value::Int(value) => arg_values.push(value.into()),
            Value::Long(value) => {
                let (low, high) = IVal::from_long(value);
                arg_values.push(low);
                arg_values.push(high);
            }
            Value::Float(value) => arg_values.push(value.into()),
            Value::Double(value) => {
                let (low, high) = IVal::from_double(value);
                arg_values.push(low);
                arg_values.push(high);
            }
        }
    }
    invoke_initialized(jvm, method, &arg_values)
}

/// Called on <clinit> during class initialization
pub(crate) fn invoke_initializer(jvm: &Jvm, initializer: &Method) -> JVMResult<()> {
    invoke_initialized(jvm, initializer, &[]).map(|_| ())
}

// So, this is a bit of a mess maybe.
// TODO: Look into either rewriting references as two slots on 64bit (difficult, as that can change bytecodes)
// or rewriting long/double as one slot (bad in 32bit like wasm32, would make quite some code simpler)

/// Stack or local entry.
/// Doesn't store type information as it was already checked during verification.
/// Ideally we don't want to use 64 bits if we can avoid this, so
/// use the size of a possibly compressed pointer (but at least 32 bits).
#[cfg(target_pointer_width = "64")]
type SlotSize = JVMPtr;
#[cfg(any(target_pointer_width = "16", target_pointer_width = "32"))]
type SlotSize = u32;

/// Interpreter value
#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
struct IVal(SlotSize);

/// Requires that the class is already initialized, or method is the initializer
fn invoke_initialized(jvm: &Jvm, method: &Method, args: &[IVal]) -> JVMResult<Value> {
    if method.code.is_none() {
        todo!("Methods without code attribute");
    }

    // The class is already initialized, so we can hold the read lock for as long as we like
    let const_pool = method.class().const_pool.read();

    let code = method.code.as_ref().unwrap();

    let mut frame = Frame {
        method,
        code_bytes: &code.bytes,
        locals: {
            let max_locals = method.code.as_ref().unwrap().max_locals as usize;
            let mut locals = Vec::with_capacity(max_locals);
            locals.extend_from_slice(args);
            while locals.len() < max_locals {
                locals.push(IVal(0));
            }
            locals
        },
        stack: Vec::with_capacity(method.code.as_ref().unwrap().max_stack as usize),
        pc: 0,
    };

    if !method.access_flags.contains(AccessFlags::STATIC) && args[0].0 == 0 {
        return Err(exception(jvm, "NullPointerException"));
    }

    'run: loop {
        match run_until_exception_or_return(jvm, method, &const_pool, &mut frame) {
            Ok(result) => return Ok(result),
            Err(thrown) => {
                // Check whether the exception gets caught or rethrown
                for handler in &code.exception_table {
                    let in_range = (handler.start_pc..=handler.end_pc).contains(&frame.pc);
                    if in_range
                        && (handler.catch_type == 0
                            || thrown.class().assignable_to(
                                &const_pool.get_class(jvm, handler.catch_type)?.name,
                            ))
                    {
                        frame.stack.clear();
                        frame.stack.push(thrown.into());
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
) -> JVMResult<Value> {
    loop {
        match frame.read_code_u8() {
            NOP => {}
            ACONST_NULL => frame.stack.push(IVal(0)),
            ICONST_M1 => frame.stack.push((-1).into()),
            ICONST_0 => frame.stack.push(0.into()),
            ICONST_1 => frame.stack.push(1.into()),
            ICONST_2 => frame.stack.push(2.into()),
            ICONST_3 => frame.stack.push(3.into()),
            ICONST_4 => frame.stack.push(4.into()),
            ICONST_5 => frame.stack.push(5.into()),
            LCONST_0 => frame.push_long(0),
            LCONST_1 => frame.push_long(1),
            FCONST_0 => frame.stack.push(0.0.into()),
            FCONST_1 => frame.stack.push(1.0.into()),
            FCONST_2 => frame.stack.push(2.0.into()),
            DCONST_0 => frame.push_double(0.0),
            DCONST_1 => frame.push_double(1.0),
            BIPUSH => {
                let value = frame.read_code_u8() as i8;
                frame.stack.push((value as i32).into());
            }
            SIPUSH => {
                let value = frame.read_code_u16() as i16;
                frame.stack.push((value as i32).into());
            }
            LDC => {
                let index = frame.read_code_u8() as u16;
                ldc(frame, const_pool, index);
            }
            LDC_W => {
                let index = frame.read_code_u16();
                ldc(frame, const_pool, index);
            }
            LDC2_W => {
                let index = frame.read_code_u16();
                ldc(frame, const_pool, index);
            }
            ILOAD | FLOAD | ALOAD => {
                let index = frame.read_code_u8() as u16;
                frame.stack.push(frame.locals[index as usize]);
            }
            LLOAD => {
                let index = frame.read_code_u8() as u16;
                frame.push_long(frame.load_long(index));
            }
            DLOAD => {
                let index = frame.read_code_u8() as u16;
                frame.push_double(frame.load_double(index));
            }
            ILOAD_0 | FLOAD_0 | ALOAD_0 => frame.stack.push(frame.locals[0]),
            ILOAD_1 | FLOAD_1 | ALOAD_1 => frame.stack.push(frame.locals[1]),
            ILOAD_2 | FLOAD_2 | ALOAD_2 => frame.stack.push(frame.locals[2]),
            ILOAD_3 | FLOAD_3 | ALOAD_3 => frame.stack.push(frame.locals[3]),
            LLOAD_0 => frame.push_long(frame.load_long(0)),
            LLOAD_1 => frame.push_long(frame.load_long(1)),
            LLOAD_2 => frame.push_long(frame.load_long(2)),
            LLOAD_3 => frame.push_long(frame.load_long(3)),
            DLOAD_0 => frame.push_double(frame.load_double(0)),
            DLOAD_1 => frame.push_double(frame.load_double(1)),
            DLOAD_2 => frame.push_double(frame.load_double(2)),
            DLOAD_3 => frame.push_double(frame.load_double(3)),
            IALOAD => {
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() }
                    .ok_or_else(|| exception(jvm, "NullPointerException"))?;
                frame.stack.push(obj.ptr.array_read_i32(jvm, index)?.into());
            }
            LALOAD => {
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() }
                    .ok_or_else(|| exception(jvm, "NullPointerException"))?;
                frame.push_long(obj.ptr.array_read_i64(jvm, index)?);
            }
            FALOAD => {
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() }
                    .ok_or_else(|| exception(jvm, "NullPointerException"))?;
                frame.stack.push(obj.ptr.array_read_f32(jvm, index)?.into());
            }
            DALOAD => {
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() }
                    .ok_or_else(|| exception(jvm, "NullPointerException"))?;
                frame.push_double(obj.ptr.array_read_f64(jvm, index)?);
            }
            AALOAD => {
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() }
                    .ok_or_else(|| exception(jvm, "NullPointerException"))?;
                frame.stack.push(IVal(obj.ptr.array_read_ptr(jvm, index)?));
            }
            BALOAD => {
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() }
                    .ok_or_else(|| exception(jvm, "NullPointerException"))?;
                frame
                    .stack
                    .push((obj.ptr.array_read_i8(jvm, index)? as i32).into());
            }
            CALOAD => {
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() }
                    .ok_or_else(|| exception(jvm, "NullPointerException"))?;
                frame
                    .stack
                    .push((obj.ptr.array_read_i16(jvm, index)? as u16 as i32).into());
            }
            SALOAD => {
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() }
                    .ok_or_else(|| exception(jvm, "NullPointerException"))?;
                frame
                    .stack
                    .push((obj.ptr.array_read_i16(jvm, index)? as i32).into());
            }
            ISTORE | FSTORE | ASTORE => {
                let index = frame.read_code_u8() as u16;
                let value = frame.pop();
                frame.locals[index as usize] = value;
            }
            LSTORE | DSTORE => {
                let index = frame.read_code_u8() as u16;
                frame.locals[index as usize + 1] = frame.pop();
                frame.locals[index as usize] = frame.pop();
            }
            ISTORE_0 | FSTORE_0 | ASTORE_0 => frame.locals[0] = frame.pop(),
            ISTORE_1 | FSTORE_1 | ASTORE_1 => frame.locals[1] = frame.pop(),
            ISTORE_2 | FSTORE_2 | ASTORE_2 => frame.locals[2] = frame.pop(),
            ISTORE_3 | FSTORE_3 | ASTORE_3 => frame.locals[3] = frame.pop(),
            LSTORE_0 | DSTORE_0 => {
                frame.locals[1] = frame.pop();
                frame.locals[0] = frame.pop();
            }
            LSTORE_1 | DSTORE_1 => {
                frame.locals[2] = frame.pop();
                frame.locals[1] = frame.pop();
            }
            LSTORE_2 | DSTORE_2 => {
                frame.locals[3] = frame.pop();
                frame.locals[2] = frame.pop();
            }
            LSTORE_3 | DSTORE_3 => {
                frame.locals[4] = frame.pop();
                frame.locals[3] = frame.pop();
            }
            IASTORE => {
                let value = frame.pop().as_int();
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() }
                    .ok_or_else(|| exception(jvm, "NullPointerException"))?;
                obj.ptr.array_write_i32(jvm, index, value)?
            }
            LASTORE => {
                let value = frame.pop_long();
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() }
                    .ok_or_else(|| exception(jvm, "NullPointerException"))?;
                obj.ptr.array_write_i64(jvm, index, value)?
            }
            FASTORE => {
                let value = frame.pop().as_float();
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() }
                    .ok_or_else(|| exception(jvm, "NullPointerException"))?;
                obj.ptr.array_write_f32(jvm, index, value)?
            }
            DASTORE => {
                let value = frame.pop_double();
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() }
                    .ok_or_else(|| exception(jvm, "NullPointerException"))?;
                obj.ptr.array_write_f64(jvm, index, value)?
            }
            AASTORE => {
                let value = frame.pop();
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() }
                    .ok_or_else(|| exception(jvm, "NullPointerException"))?;
                let Some(Typ::Ref(component)) = &obj.class().element_type else {
                    unreachable!("checked during verification")
                };
                if !obj.class().assignable_to(component) {
                    return Err(exception(jvm, "ArrayStoreException"));
                }
                obj.ptr.array_write_ptr(jvm, index, value.0)?
            }
            BASTORE => {
                let value = frame.pop().as_int();
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() }
                    .ok_or_else(|| exception(jvm, "NullPointerException"))?;
                obj.ptr.array_write_i8(jvm, index, value as u8 as i8)?
            }
            CASTORE => {
                let value = frame.pop().as_int();
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() }
                    .ok_or_else(|| exception(jvm, "NullPointerException"))?;
                obj.ptr.array_write_i16(jvm, index, value as u16 as i16)?
            }
            SASTORE => {
                let value = frame.pop().as_int();
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() }
                    .ok_or_else(|| exception(jvm, "NullPointerException"))?;
                obj.ptr.array_write_i16(jvm, index, value as u16 as i16)?
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
                frame.stack.push(value);
                frame.stack.push(value);
            }
            DUP_X1 => {
                let first = frame.pop();
                let second = frame.pop();
                frame.stack.push(first);
                frame.stack.push(second);
                frame.stack.push(first);
            }
            DUP_X2 => {
                let first = frame.pop();
                let second = frame.pop();
                let third = frame.pop();
                frame.stack.push(first);
                frame.stack.push(third);
                frame.stack.push(second);
                frame.stack.push(first);
            }
            DUP2 => {
                let first = frame.pop();
                let second = frame.pop();
                frame.stack.push(second);
                frame.stack.push(first);
                frame.stack.push(second);
                frame.stack.push(first);
            }
            DUP2_X1 => {
                let first = frame.pop();
                let second = frame.pop();
                let third = frame.pop();
                frame.stack.push(second);
                frame.stack.push(first);
                frame.stack.push(third);
                frame.stack.push(second);
                frame.stack.push(first);
            }
            DUP2_X2 => {
                let first = frame.pop();
                let second = frame.pop();
                let third = frame.pop();
                let fourth = frame.pop();
                frame.stack.push(second);
                frame.stack.push(first);
                frame.stack.push(fourth);
                frame.stack.push(third);
                frame.stack.push(second);
                frame.stack.push(first);
            }
            SWAP => {
                let first = frame.pop();
                let second = frame.pop();
                frame.stack.push(first);
                frame.stack.push(second);
            }
            IADD | ISUB | IMUL | IDIV | IREM | ISHL | ISHR | IUSHR | IAND | IOR | IXOR => {
                let b = frame.pop().as_int();
                let a = frame.pop().as_int();
                let result = match frame.read_previous_byte() {
                    IADD => a + b,
                    ISUB => a - b,
                    IMUL => a * b,
                    IDIV => a
                        .checked_div(b)
                        .ok_or_else(|| exception(jvm, "ArithmeticException"))?,
                    IREM => {
                        if b != 0 {
                            a % b
                        } else {
                            return Err(exception(jvm, "ArithmeticException"));
                        }
                    }
                    ISHL => a << (b & 0x1f),
                    ISHR => a >> (b & 0x1f),
                    IUSHR => (a as u32 >> (b & 0x1f)) as i32,
                    IAND => a & b,
                    IOR => a | b,
                    IXOR => a ^ b,
                    _ => unreachable!(),
                };
                frame.stack.push(result.into());
            }
            LADD | LSUB | LMUL | LDIV | LREM | LSHL | LSHR | LUSHR | LAND | LOR | LXOR => {
                let b = frame.pop_long();
                let a = frame.pop_long();
                let result = match frame.read_previous_byte() {
                    LADD => a + b,
                    LSUB => a - b,
                    LMUL => a * b,
                    LDIV => a
                        .checked_div(b)
                        .ok_or_else(|| exception(jvm, "ArithmeticException"))?,
                    LREM => {
                        if b != 0 {
                            a % b
                        } else {
                            return Err(exception(jvm, "ArithmeticException"));
                        }
                    }
                    LSHL => a << (b & 0x3f),
                    LSHR => a >> (b & 0x3f),
                    LUSHR => (a as u64 >> (b & 0x3f)) as i64,
                    LAND => a & b,
                    LOR => a | b,
                    LXOR => a ^ b,
                    _ => unreachable!(),
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
                    _ => unreachable!(),
                };
                frame.stack.push(result.into());
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
                    _ => unreachable!(),
                };
                frame.push_double(result);
            }
            INEG => {
                let result = -frame.pop().as_int();
                frame.stack.push(result.into());
            }
            LNEG => {
                let result = -frame.pop_long();
                frame.push_long(result);
            }
            FNEG => {
                let result = -frame.pop().as_float();
                frame.stack.push(result.into());
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
                frame.stack.push(value.into());
            }
            I2D => {
                let value = frame.pop().as_int();
                frame.push_double(value as f64);
            }
            L2I => {
                let value = frame.pop_long() as i32;
                frame.stack.push(value.into());
            }
            L2F => {
                let value = frame.pop_long() as f32;
                frame.stack.push(value.into());
            }
            L2D => {
                let value = frame.pop_long();
                frame.push_double(value as f64);
            }
            F2I => {
                let value = frame.pop().as_float() as i32;
                frame.stack.push(value.into());
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
                frame.stack.push(value.into());
            }
            D2L => {
                let value = frame.pop_double();
                frame.push_long(value as i64);
            }
            D2F => {
                let value = frame.pop_double() as f32;
                frame.stack.push(value.into());
            }
            I2B => {
                let value = frame.pop().as_int() as i8 as i32;
                frame.stack.push(value.into());
            }
            I2C => {
                let value = frame.pop().as_int() as u16 as i32;
                frame.stack.push(value.into());
            }
            I2S => {
                let value = frame.pop().as_int() as i16 as i32;
                frame.stack.push(value.into());
            }
            LCMP => {
                let b = frame.pop_long();
                let a = frame.pop_long();
                frame.stack.push(
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
                frame.stack.push(result.into())
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
                frame.stack.push(result.into())
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
                    _ => unreachable!(),
                } {
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
                frame.pc &= 0xfffc;

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
                frame.pc &= 0xfffc;

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
            IRETURN => return Ok(frame.pop().as_int().into()),
            LRETURN => return Ok(frame.pop_long().into()),
            FRETURN => return Ok(frame.pop().as_float().into()),
            DRETURN => return Ok(frame.pop_double().into()),
            ARETURN => return Ok(unsafe { frame.pop().as_ref() }.into()),
            RETURN => return Ok(Value::Void),
            GETSTATIC => {
                let index = frame.read_code_u16();
                let field = const_pool.get_field(jvm, index).unwrap();
                field.class().ensure_init(jvm)?;
                get_field(frame, field, &field.class().static_storage);
            }
            PUTSTATIC => {
                let index = frame.read_code_u16();
                let field = const_pool.get_field(jvm, index).unwrap();
                field.class().ensure_init(jvm)?;
                put_field(frame, field, &field.class().static_storage);
            }
            GETFIELD => {
                let index = frame.read_code_u16();
                let field = const_pool.get_field(jvm, index)?;
                let object = unsafe { frame.pop().as_ref() }
                    .ok_or_else(|| exception(jvm, "NullPointerException"))?;
                get_field(frame, field, &object.ptr);
            }
            PUTFIELD => {
                let index = frame.read_code_u16();
                let field = const_pool.get_field(jvm, index)?;
                // Stack has object ref first, then value on top
                // This is ugly
                let object = if matches!(field.nat.typ, Typ::Long | Typ::Double) {
                    let high = frame.pop();
                    let low = frame.pop();
                    let obj = unsafe { frame.pop().as_ref() };
                    frame.stack.push(low);
                    frame.stack.push(high);
                    obj
                } else {
                    let value = frame.pop();
                    let obj = unsafe { frame.pop().as_ref() };
                    frame.stack.push(value);
                    obj
                }
                .ok_or_else(|| exception(jvm, "NullPointerException"))?;
                put_field(frame, field, &object.ptr);
            }
            INVOKEVIRTUAL => {
                let index = frame.read_code_u16();
                let (_, nat) = const_pool.get_virtual_method(jvm, index)?;
                let obj_index = frame.stack.len() - nat.typ.arg_slots() - 1;
                let obj = unsafe { frame.stack[obj_index].as_ref() }
                    .ok_or_else(|| exception(jvm, "NullPointerException"))?;
                // TODO: move method linking (incl NoSuchMethodError and IncompatibleClassChangeError) to class init
                let invoke_method = obj
                    .class()
                    .methods
                    .get(&nat)
                    .ok_or_else(|| exception(jvm, "NoSuchMethodError"))?;
                if invoke_method.access_flags.contains(AccessFlags::STATIC) {
                    return Err(exception(jvm, "IncompatibleClassChangeError"));
                }
                if invoke_method.access_flags.contains(AccessFlags::ABSTRACT) {
                    return Err(exception(jvm, "AbstractMethodError"));
                }
                execute_invoke_instr(jvm, frame, invoke_method)?;
            }
            INVOKESPECIAL => {
                let index = frame.read_code_u16();
                let (named_class, nat) = const_pool.get_virtual_method(jvm, index)?;

                let is_instance_init = nat.name.as_ref() == "<init>";

                let class = if !is_instance_init
                    & method.class().true_subclass_of(&named_class.name)
                    & method.class().access_flags.contains(AccessFlags::SUPER)
                {
                    method.class().super_class.unwrap()
                } else {
                    named_class
                };

                // TODO: move method linking (incl NoSuchMethodError and IncompatibleClassChangeError) to class init
                let invoke_method = class
                    .methods
                    .get(&nat)
                    .ok_or_else(|| exception(jvm, "NoSuchMethodError"))?;
                if is_instance_init && invoke_method.class() != named_class {
                    return Err(exception(jvm, "NoSuchMethodError"));
                }
                if invoke_method.access_flags.contains(AccessFlags::STATIC) {
                    return Err(exception(jvm, "IncompatibleClassChangeError"));
                }
                execute_invoke_instr(jvm, frame, invoke_method)?;
            }
            INVOKESTATIC => {
                let index = frame.read_code_u16();
                let invoke_method = const_pool.get_static_method(jvm, index)?;
                invoke_method.class().ensure_init(jvm)?;
                execute_invoke_instr(jvm, frame, invoke_method)?;
            }
            NEW => {
                let index = frame.read_code_u16();
                let class = const_pool.get_class(jvm, index)?;
                class.ensure_init(jvm)?;
                frame.stack.push(jvm.create_object(class).into())
            }
            NEWARRAY => {
                let length = frame.pop().as_int();
                if length < 0 {
                    return Err(exception(jvm, "NegativeArraySizeException"));
                }
                let component = match frame.read_code_u8() {
                    4 => Typ::Boolean,
                    5 => Typ::Byte,
                    6 => Typ::Char,
                    7 => Typ::Short,
                    8 => Typ::Double,
                    9 => Typ::Float,
                    10 => Typ::Int,
                    11 => Typ::Long,
                    _ => unreachable!(),
                };
                frame
                    .stack
                    .push(jvm.create_array_of(&component, length).into());
            }
            ANEWARRAY => {
                let length = frame.pop().as_int();
                let index = frame.read_code_u16();
                let component = Typ::Ref(const_pool.get_class(jvm, index)?.name.clone());
                if length < 0 {
                    return Err(exception(jvm, "NegativeArraySizeException"));
                }
                frame
                    .stack
                    .push(jvm.create_array_of(&component, length).into());
            }
            ARRAYLENGTH => {
                let obj = unsafe { frame.pop().as_ref() }
                    .ok_or_else(|| exception(jvm, "NullPointerException"))?;
                let Some(length) = obj.array_len() else {
                    unreachable!()
                };
                frame.stack.push(length.into());
            }
            ATHROW => {
                let obj = unsafe { frame.pop().as_ref() }
                    .ok_or_else(|| exception(jvm, "NullPointerException"))?;
                return Err(obj);
            }
            MULTIANEWARRAY => {
                let index = frame.read_code_u16();
                let dimensions = frame.read_code_u8();
                // Remove this check one it's in validation
                if dimensions == 0 {
                    return Err(exception(jvm, "ClassFormatError"));
                }
                let class = const_pool.get_class(jvm, index)?;
                fn create_array(jvm: &Jvm, dimensions: &[IVal], class: ClassPtr) -> Object {
                    let len = dimensions[0].as_int();
                    let array = jvm.create_array(class, len);
                    if let Some(Typ::Ref(element_type)) = &class.element_type {
                        let class = jvm.resolve_class(element_type).unwrap();
                        for i in 0..len {
                            array.array_write(i, create_array(jvm, &dimensions[1..], class).into());
                        }
                    }
                    array
                }
                let array = create_array(
                    jvm,
                    &frame.stack[frame.stack.len() - dimensions as usize..],
                    class,
                );
                frame.stack.push(array.into());
            }
            IF_NULL => {
                let base = frame.pc - 1;
                let target_offset = frame.read_code_u16() as i16;
                if frame.pop().0 == 0 {
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
        Some(&ConstPoolItem::Integer(i)) => frame.stack.push(i.into()),
        Some(&ConstPoolItem::Float(f)) => frame.stack.push(f.into()),
        Some(ConstPoolItem::Long(l)) => frame.push_long(*l),
        Some(ConstPoolItem::Double(d)) => frame.push_double(*d),
        Some(&ConstPoolItem::String(object)) => frame.stack.push(object.into()),
        Some(ConstPoolItem::Class(_)) => todo!("Class objects"),
        other => unreachable!("Invalid ldc/ldc_w/ldc2_w: {:?}", other),
    }
}

fn get_field(frame: &mut Frame, field: &Field, storage: &FieldStorage) {
    let volatile = field.access_flags.contains(AccessFlags::VOLATILE);
    unsafe {
        match field.nat.typ {
            Typ::Boolean | Typ::Byte => frame
                .stack
                .push((storage.read_i8(field.byte_offset, volatile) as i32).into()),
            Typ::Short | Typ::Char => frame
                .stack
                .push((storage.read_i16(field.byte_offset, volatile) as i32).into()),
            Typ::Int => frame
                .stack
                .push(storage.read_i32(field.byte_offset, volatile).into()),
            Typ::Float => frame
                .stack
                .push(storage.read_f32(field.byte_offset, volatile).into()),
            Typ::Long => frame.push_long(storage.read_i64(field.byte_offset, volatile)),
            Typ::Double => frame.push_double(storage.read_f64(field.byte_offset, volatile)),
            Typ::Ref(..) => frame
                .stack
                .push(IVal(storage.read_ptr(field.byte_offset, volatile))),
        }
    }
}

fn put_field(frame: &mut Frame, field: &Field, storage: &FieldStorage) {
    let volatile = field.access_flags.contains(AccessFlags::VOLATILE);
    unsafe {
        match field.nat.typ {
            Typ::Boolean | Typ::Byte => {
                storage.write_i8(field.byte_offset, frame.pop().as_int() as i8, volatile)
            }
            Typ::Short | Typ::Char => {
                storage.write_i16(field.byte_offset, frame.pop().as_int() as i16, volatile)
            }
            Typ::Int => storage.write_i32(field.byte_offset, frame.pop().as_int(), volatile),
            Typ::Float => storage.write_f32(field.byte_offset, frame.pop().as_float(), volatile),
            Typ::Long => storage.write_i64(field.byte_offset, frame.pop_long(), volatile),
            Typ::Double => storage.write_f64(field.byte_offset, frame.pop_double(), volatile),
            Typ::Ref(..) => storage.write_ptr(field.byte_offset, frame.pop().0, volatile),
        }
    }
}

fn execute_invoke_instr(jvm: &Jvm, frame: &mut Frame, method: &Method) -> JVMResult<()> {
    let mut arg_slots = method.nat.typ.arg_slots();
    if !method.access_flags.contains(AccessFlags::STATIC) {
        // `this` is treated as an argument, but not mentioned in method descriptor
        arg_slots += 1;
    }

    let args = &frame.stack[frame.stack.len() - arg_slots..];
    let result = invoke_initialized(jvm, method, args)?;
    frame.stack.truncate(frame.stack.len() - arg_slots);
    match result {
        Value::Void => (),
        Value::Ref(object) => frame.stack.push(object.into()),
        Value::Int(value) => frame.stack.push(value.into()),
        Value::Float(value) => frame.stack.push(value.into()),
        Value::Long(value) => frame.push_long(value),
        Value::Double(value) => frame.push_double(value),
    };
    Ok(())
}

impl From<i32> for IVal {
    fn from(value: i32) -> Self {
        Self(value as u32 as SlotSize)
    }
}

impl From<f32> for IVal {
    fn from(value: f32) -> Self {
        Self(value.to_bits() as SlotSize)
    }
}

impl From<Object> for IVal {
    fn from(value: Object) -> Self {
        Self(value.ptr().into())
    }
}

impl From<Option<Object>> for IVal {
    fn from(value: Option<Object>) -> Self {
        Self(match value {
            None => NULL_PTR,
            Some(object) => object.ptr().into(),
        })
    }
}

impl IVal {
    /// Returns (low half, high half)
    fn from_long(value: i64) -> (Self, Self) {
        (
            Self((value as u64 & 0xFFFFFFFF) as SlotSize),
            Self((value as u64 >> 32) as SlotSize),
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
        f32::from_bits(self.0)
    }

    fn as_long(low: IVal, high: IVal) -> i64 {
        (low.0 as u64 + ((high.0 as u64) << 32)) as i64
    }

    fn as_double(low: IVal, high: IVal) -> f64 {
        f64::from_bits(low.0 as u64 + ((high.0 as u64) << 32))
    }

    unsafe fn as_ref(self) -> Option<Object> {
        Object::from_ptr(self.0)
    }
}

struct Frame<'a> {
    method: &'a Method,
    code_bytes: &'a [u8],
    locals: Vec<IVal>,
    stack: Vec<IVal>,
    pc: u16,
}

impl<'jvm> Frame<'jvm> {
    fn jump_relative(&mut self, base: u16, target_offset: i32) {
        self.pc = (base as i32 + target_offset) as u16;
    }

    fn load_long(&self, index: u16) -> i64 {
        IVal::as_long(self.locals[index as usize], self.locals[index as usize + 1])
    }

    fn load_double(&self, index: u16) -> f64 {
        IVal::as_double(self.locals[index as usize], self.locals[index as usize + 1])
    }

    fn pop(&mut self) -> IVal {
        self.stack.pop().expect("Popped empty stack")
    }

    fn pop_long(&mut self) -> i64 {
        let (high, low) = (self.pop(), self.pop());
        IVal::as_long(low, high)
    }

    fn pop_double(&mut self) -> f64 {
        let (high, low) = (self.pop(), self.pop());
        IVal::as_double(low, high)
    }

    fn push_long(&mut self, value: i64) {
        let (low, high) = IVal::from_long(value);
        self.stack.push(low);
        self.stack.push(high);
    }

    fn push_double(&mut self, value: f64) {
        let (low, high) = IVal::from_double(value);
        self.stack.push(low);
        self.stack.push(high);
    }

    fn read_code_u8(&mut self) -> u8 {
        // SAFETY: This always succeeds because bounds were already checked during verification
        let byte = unsafe { *self.code_bytes.get_unchecked(self.pc as usize) };
        self.pc += 1;
        byte
    }

    fn read_code_u16(&mut self) -> u16 {
        ((self.read_code_u8() as u16) << 8) + self.read_code_u8() as u16
    }

    fn read_code_u32(&mut self) -> u32 {
        ((self.read_code_u16() as u32) << 16) + self.read_code_u16() as u32
    }

    fn read_previous_byte(&mut self) -> u8 {
        self.method.code.as_ref().unwrap().bytes[self.pc as usize - 1]
    }
}
