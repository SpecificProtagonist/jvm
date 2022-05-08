use std::{mem::size_of, usize};

use crate::{
    const_pool::{ConstPool, ConstPoolItem},
    exception,
    field_storage::FieldStorage,
    heap::JVMPtrSize,
    instructions::*,
    object::{self, Object},
    AccessFlags, Field, JVMResult, JVMValue, Method, Typ, JVM,
};

pub(crate) fn invoke<'a, 'b>(
    jvm: &'b JVM<'a>,
    method: &'a Method<'a>,
    args: &'b [JVMValue<'a>],
) -> JVMResult<'a, Option<JVMValue<'a>>> {
    method.class.load().ensure_init(jvm)?;
    let mut arg_values = Vec::new();
    for arg in args {
        match *arg {
            JVMValue::Ref(object) => arg_values.push(Value::from_ref(object)),
            JVMValue::Int(value) => arg_values.push(Value::from_int(value)),
            JVMValue::Long(value) => {
                let (low, high) = Value::from_long(value);
                arg_values.push(low);
                arg_values.push(high);
            }
            JVMValue::Float(value) => arg_values.push(Value::from_float(value)),
            JVMValue::Double(value) => {
                let (low, high) = Value::from_double(value);
                arg_values.push(low);
                arg_values.push(high);
            }
        }
    }
    invoke_initialized(jvm, method, &arg_values)
}

/// Stack or local entry.
/// Doesn't store type information as it was already checked during verification.
/// Ideally we don't want to use 64 bits if we can avoid this, so
/// use the size of a possibly compressed pointer (but at least 32 bits).
#[cfg(not(any(target_pointer_width = "8", target_pointer_width = "16")))]
type SlotSize = JVMPtrSize;
#[cfg(any(target_pointer_width = "8", target_pointer_width = "16"))]
type SlotSize = u32;

#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
struct Value(SlotSize);

pub(crate) fn invoke_initializer<'a, 'b>(
    jvm: &'b JVM<'a>,
    method: &'a Method<'a>,
) -> JVMResult<'a, ()> {
    invoke_initialized(jvm, method, &[]).map(|_| ())
}

// Requires that the class is already initialized
fn invoke_initialized<'a, 'b>(
    jvm: &'b JVM<'a>,
    method: &'a Method<'a>,
    args: &'b [Value],
) -> JVMResult<'a, Option<JVMValue<'a>>> {
    if method.code.is_none() {
        todo!("Methods without code attribute");
    }

    // The class is already initialized, so we can hold the read lock for as long as we like
    let const_pool = method.class.load().const_pool.read();

    let mut frame = Frame {
        method,
        code_bytes: &method.code.as_ref().unwrap().bytes,
        locals: {
            let max_locals = method.code.as_ref().unwrap().max_locals as usize;
            let mut locals = Vec::with_capacity(max_locals);
            locals.extend_from_slice(args);
            while locals.len() < max_locals {
                locals.push(Value(0));
            }
            locals
        },
        stack: Vec::with_capacity(method.code.as_ref().unwrap().max_stack as usize),
        pc: 0,
    };

    if !method.access_flags.contains(AccessFlags::STATIC) && args[0].0 == 0 {
        return Err(exception(jvm, "NullPointerException"));
    }

    // Todo: bytecode verification, esp. about pc
    loop {
        match frame.read_code_u8() {
            NOP => {}
            ICONST_M1 => frame.stack.push(Value::from_int(-1)),
            ICONST_0 => frame.stack.push(Value::from_int(0)),
            ICONST_1 => frame.stack.push(Value::from_int(1)),
            ICONST_2 => frame.stack.push(Value::from_int(2)),
            ICONST_3 => frame.stack.push(Value::from_int(3)),
            ICONST_4 => frame.stack.push(Value::from_int(4)),
            ICONST_5 => frame.stack.push(Value::from_int(5)),
            LCONST_0 => frame.push_long(0),
            LCONST_1 => frame.push_long(1),
            FCONST_0 => frame.stack.push(Value::from_float(0.0)),
            FCONST_1 => frame.stack.push(Value::from_float(1.0)),
            FCONST_2 => frame.stack.push(Value::from_float(2.0)),
            DCONST_0 => frame.push_double(0.0),
            DCONST_1 => frame.push_double(1.0),
            BIPUSH => {
                let value = frame.read_code_u8() as i8;
                frame.stack.push(Value::from_int(value as i32));
            }
            SIPUSH => {
                let value = frame.read_code_u16() as i16;
                frame.stack.push(Value::from_int(value as i32));
            }
            LDC => {
                let index = frame.read_code_u8() as u16;
                ldc(&mut frame, &const_pool, index);
            }
            LDC_W => {
                let index = frame.read_code_u16();
                ldc(&mut frame, &const_pool, index);
            }
            LDC2_W => {
                let index = frame.read_code_u16();
                ldc(&mut frame, &const_pool, index);
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
                let obj = unsafe { frame.pop().as_ref() };
                if obj.null() {
                    return Err(exception(jvm, "NullPointerException"));
                }
                frame.stack.push(Value::from_int(
                    obj.ptr
                        .read_i32((object::header_size() as isize + 4 * index as isize) as u32)
                        .ok_or_else(|| exception(jvm, "ArrayIndexOutOfBoundsException"))?,
                ));
            }
            LALOAD => {
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() };
                if obj.null() {
                    return Err(exception(jvm, "NullPointerException"));
                }
                frame.push_long(
                    obj.ptr
                        .read_i64((object::header_size() as isize + 8 * index as isize) as u32)
                        .ok_or_else(|| exception(jvm, "ArrayIndexOutOfBoundsException"))?,
                );
            }
            FALOAD => {
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() };
                if obj.null() {
                    return Err(exception(jvm, "NullPointerException"));
                }
                frame.stack.push(Value::from_float(
                    obj.ptr
                        .read_f32((object::header_size() as isize + 4 * index as isize) as u32)
                        .ok_or_else(|| exception(jvm, "ArrayIndexOutOfBoundsException"))?,
                ));
            }
            DALOAD => {
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() };
                if obj.null() {
                    return Err(exception(jvm, "NullPointerException"));
                }
                frame.push_double(
                    obj.ptr
                        .read_f64((object::header_size() as isize + 8 * index as isize) as u32)
                        .ok_or_else(|| exception(jvm, "ArrayIndexOutOfBoundsException"))?,
                );
            }
            AALOAD => {
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() };
                if obj.null() {
                    return Err(exception(jvm, "NullPointerException"));
                }
                frame.stack.push(Value::from_ref(unsafe {
                    Object::from_ptr(
                        obj.ptr
                            .read_ptr(
                                (object::header_size() as isize
                                    + size_of::<usize>() as isize * index as isize)
                                    as u32,
                            )
                            .ok_or_else(|| exception(jvm, "ArrayIndexOutOfBoundsException"))?,
                    )
                }));
            }
            BALOAD => {
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() };
                if obj.null() {
                    return Err(exception(jvm, "NullPointerException"));
                }
                frame.stack.push(Value::from_int(
                    obj.ptr
                        .read_i8((object::header_size() as isize + index as isize) as u32)
                        .ok_or_else(|| exception(jvm, "ArrayIndexOutOfBoundsException"))?
                        as i32,
                ));
            }
            CALOAD => {
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() };
                if obj.null() {
                    return Err(exception(jvm, "NullPointerException"));
                }
                frame.stack.push(Value::from_int(
                    obj.ptr
                        .read_i16((object::header_size() as isize + 2 * index as isize) as u32)
                        .ok_or_else(|| exception(jvm, "ArrayIndexOutOfBoundsException"))?
                        as u16 as i32,
                ));
            }
            SALOAD => {
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() };
                if obj.null() {
                    return Err(exception(jvm, "NullPointerException"));
                }
                frame.stack.push(Value::from_int(
                    obj.ptr
                        .read_i16((object::header_size() as isize + 2 * index as isize) as u32)
                        .ok_or_else(|| exception(jvm, "ArrayIndexOutOfBoundsException"))?
                        as i32,
                ));
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
                let obj = unsafe { frame.pop().as_ref() };
                if obj.null() {
                    return Err(exception(jvm, "NullPointerException"));
                }
                obj.ptr
                    .write_i32(
                        (object::header_size() as isize + 4 * index as isize) as u32,
                        value,
                    )
                    .ok_or_else(|| exception(jvm, "ArrayIndexOutOfBoundsException"))?;
            }
            LASTORE => {
                let value = frame.pop_long();
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() };
                if obj.null() {
                    return Err(exception(jvm, "NullPointerException"));
                }
                obj.ptr
                    .write_i64(
                        (object::header_size() as isize + 8 * index as isize) as u32,
                        value,
                    )
                    .ok_or_else(|| exception(jvm, "ArrayIndexOutOfBoundsException"))?;
            }
            FASTORE => {
                let value = frame.pop().as_float();
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() };
                if obj.null() {
                    return Err(exception(jvm, "NullPointerException"));
                }
                obj.ptr
                    .write_f32(
                        (object::header_size() as isize + 4 * index as isize) as u32,
                        value,
                    )
                    .ok_or_else(|| exception(jvm, "ArrayIndexOutOfBoundsException"))?;
            }
            DASTORE => {
                let value = frame.pop_double();
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() };
                if obj.null() {
                    return Err(exception(jvm, "NullPointerException"));
                }
                obj.ptr
                    .write_f64(
                        (object::header_size() as isize + 8 * index as isize) as u32,
                        value,
                    )
                    .ok_or_else(|| exception(jvm, "ArrayIndexOutOfBoundsException"))?;
            }
            AASTORE => {
                let value = unsafe { frame.pop().as_ref() };
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() };
                if obj.null() {
                    return Err(exception(jvm, "NullPointerException"));
                }
                if let Some(Typ::Ref(component)) = obj.class().element_type {
                    if !obj.class().assignable_to(jvm.resolve_class(component)?) {
                        return Err(exception(jvm, "ArrayStoreException"));
                    }
                    obj.ptr
                        .write_ptr(
                            (object::header_size() as isize
                                + size_of::<usize>() as isize * index as isize)
                                as u32,
                            value.ptr.ptr(),
                        )
                        .ok_or_else(|| exception(jvm, "ArrayIndexOutOfBoundsException"))?;
                } else {
                    unreachable!()
                }
            }
            BASTORE => {
                let value = frame.pop().as_int();
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() };
                if obj.null() {
                    return Err(exception(jvm, "NullPointerException"));
                }
                obj.ptr
                    .write_i8(
                        (object::header_size() as isize + index as isize) as u32,
                        value as i8,
                    )
                    .ok_or_else(|| exception(jvm, "ArrayIndexOutOfBoundsException"))?;
            }
            CASTORE => {
                let value = frame.pop().as_int();
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() };
                if obj.null() {
                    return Err(exception(jvm, "NullPointerException"));
                }
                obj.ptr
                    .write_i16(
                        (object::header_size() as isize + 2 * index as isize) as u32,
                        value as u16 as i16,
                    )
                    .ok_or_else(|| exception(jvm, "ArrayIndexOutOfBoundsException"))?;
            }
            SASTORE => {
                let value = frame.pop().as_int();
                let index = frame.pop().as_int();
                let obj = unsafe { frame.pop().as_ref() };
                if obj.null() {
                    return Err(exception(jvm, "NullPointerException"));
                }
                obj.ptr
                    .write_i16(
                        (object::header_size() as isize + 2 * index as isize) as u32,
                        value as i16,
                    )
                    .ok_or_else(|| exception(jvm, "ArrayIndexOutOfBoundsException"))?;
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
                frame.stack.push(Value::from_int(result));
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
                frame.stack.push(Value::from_float(result));
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
                frame.stack.push(Value::from_int(result));
            }
            LNEG => {
                let result = -frame.pop_long();
                frame.push_long(result);
            }
            FNEG => {
                let result = -frame.pop().as_float();
                frame.stack.push(Value::from_float(result));
            }
            DNEG => {
                let result = -frame.pop_double();
                frame.push_double(result);
            }
            IINC => {
                let index = frame.read_code_u8() as u16;
                let constant = frame.read_code_u8() as i32;
                frame.locals[index as usize] =
                    Value::from_int(frame.locals[index as usize].as_int() + constant);
            }
            I2L => {
                let value = frame.pop().as_int();
                frame.push_long(value as i64);
            }
            I2F => {
                let value = frame.pop().as_int();
                frame.stack.push(Value::from_float(value as f32));
            }
            I2D => {
                let value = frame.pop().as_int();
                frame.push_double(value as f64);
            }
            L2I => {
                let value = frame.pop_long();
                frame.stack.push(Value::from_int(value as i32));
            }
            L2F => {
                let value = frame.pop_long();
                frame.stack.push(Value::from_float(value as f32));
            }
            L2D => {
                let value = frame.pop_long();
                frame.push_double(value as f64);
            }
            F2I => {
                let value = frame.pop().as_float();
                frame.stack.push(Value::from_int(value as i32));
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
                let value = frame.pop_double();
                frame.stack.push(Value::from_int(value as i32));
            }
            D2L => {
                let value = frame.pop_double();
                frame.push_long(value as i64);
            }
            D2F => {
                let value = frame.pop_double();
                frame.stack.push(Value::from_float(value as f32));
            }
            I2B => {
                let value = frame.pop().as_int() as i8;
                frame.stack.push(Value::from_int(value as i32));
            }
            I2C => {
                let value = frame.pop().as_int() as u16;
                frame.stack.push(Value::from_int(value as i32));
            }
            I2S => {
                let value = frame.pop().as_int() as i16;
                frame.stack.push(Value::from_int(value as i32));
            }
            LCMP => {
                let b = frame.pop_long();
                let a = frame.pop_long();
                frame.stack.push(Value::from_int(match a - b {
                    x if x > 0 => 1,
                    0 => 0,
                    _ => -1,
                }))
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
                } else if b == a {
                    0
                } else {
                    1
                };
                frame.stack.push(Value::from_int(result))
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
                } else if b == a {
                    0
                } else {
                    1
                };
                frame.stack.push(Value::from_int(result))
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
            IRETURN => return Ok(Some(JVMValue::Int(frame.pop().as_int()))),
            LRETURN => return Ok(Some(JVMValue::Long(frame.pop_long()))),
            FRETURN => return Ok(Some(JVMValue::Float(frame.pop().as_float()))),
            DRETURN => return Ok(Some(JVMValue::Double(frame.pop_double()))),
            ARETURN => return Ok(Some(JVMValue::Ref(unsafe { frame.pop().as_ref() }))),
            RETURN => return Ok(None),
            GETSTATIC => {
                let index = frame.read_code_u16();
                let field = const_pool.get_field(jvm, index).unwrap();
                field.class.load().ensure_init(jvm)?;
                get_field(&mut frame, field, &field.class.load().static_storage);
            }
            PUTSTATIC => {
                let index = frame.read_code_u16();
                let field = const_pool.get_field(jvm, index).unwrap();
                field.class.load().ensure_init(jvm)?;
                put_field(&mut frame, field, &field.class.load().static_storage);
            }
            GETFIELD => {
                let index = frame.read_code_u16();
                let field = const_pool.get_field(jvm, index)?;
                let object = unsafe { frame.pop().as_ref() };
                get_field(&mut frame, field, &object.ptr);
            }
            PUTFIELD => {
                let index = frame.read_code_u16();
                let field = const_pool.get_field(jvm, index)?;
                // Stack has object ref first, then value on top
                // This is ugly
                let object = if matches!(field.descriptor, Typ::Long | Typ::Double) {
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
                };
                put_field(&mut frame, field, &object.ptr);
            }
            INVOKEVIRTUAL => {
                let index = frame.read_code_u16();
                let (_, nat) = const_pool.get_virtual_method(jvm, index)?;
                let obj_index = frame.stack.len() - nat.typ.arg_slots() - 1;
                let obj = unsafe { frame.stack[obj_index].as_ref() };
                if obj.null() {
                    return Err(exception(jvm, "NullPointerException"));
                }
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
                execute_invoke_instr(jvm, &mut frame, invoke_method)?;
            }
            INVOKESPECIAL => {
                let index = frame.read_code_u16();
                let (named_class, nat) = const_pool.get_virtual_method(jvm, index)?;

                let obj_index = frame.stack.len() - nat.typ.arg_slots() - 1;
                let obj = unsafe { frame.stack[obj_index].as_ref() };
                if obj.null() {
                    return Err(exception(jvm, "NullPointerException"));
                }

                let is_instance_init = nat.name.0 == "<init>";

                let class = if !is_instance_init
                    & method.class.load().subclass_of(named_class)
                    & method
                        .class
                        .load()
                        .access_flags
                        .contains(AccessFlags::SUPER)
                {
                    method.class.load().super_class.unwrap()
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
                execute_invoke_instr(jvm, &mut frame, invoke_method)?;
            }
            INVOKESTATIC => {
                let index = frame.read_code_u16();
                let invoke_method = const_pool.get_static_method(jvm, index)?;
                invoke_method.class.load().ensure_init(jvm)?;
                execute_invoke_instr(jvm, &mut frame, invoke_method)?;
            }
            NEW => {
                let index = frame.read_code_u16();
                let class = const_pool.get_class(jvm, index)?;
                class.ensure_init(jvm)?;
                frame.stack.push(Value::from_ref(jvm.create_object(class)))
            }
            NEWARRAY => {
                let length = frame.pop().as_int();
                if length < 0 {
                    return Err(exception(jvm, "NegativeArraySizeException"));
                }
                let typ = match frame.read_code_u8() {
                    4 => "[Z",
                    5 => "[C",
                    6 => "[F",
                    7 => "[D",
                    8 => "[B",
                    9 => "[S",
                    10 => "[I",
                    11 => "[J",
                    _ => unreachable!(),
                };
                let typ = jvm.resolve_class(typ)?;
                frame
                    .stack
                    .push(Value::from_ref(jvm.create_array(typ, length as usize)));
            }
            ANEWARRAY => {
                let length = frame.pop().as_int();
                let index = frame.read_code_u16();
                let component = const_pool.get_class(jvm, index)?;
                // TODO: make it easier to refer to array types
                let typ = jvm.resolve_class(format!("[L{};", component.name))?;
                if length < 0 {
                    return Err(exception(jvm, "NegativeArraySizeException"));
                }
                frame
                    .stack
                    .push(Value::from_ref(jvm.create_array(typ, length as usize)));
            }
            ARRAYLENGTH => {
                let obj = unsafe { frame.pop().as_ref() };
                if obj.null() {
                    return Err(exception(jvm, "NullPointerException"));
                }
                if let Some(base) = obj.class().element_type {
                    let length =
                        ((obj.ptr.size() - object::header_size()) / base.layout().size()) as i32;
                    frame.stack.push(Value::from_int(length));
                } else {
                    unreachable!()
                }
            }
            MULTIANEWARRAY => {
                todo!()
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
        Some(ConstPoolItem::Integer(i)) => frame.stack.push(Value::from_int(*i)),
        Some(ConstPoolItem::Float(f)) => frame.stack.push(Value::from_float(*f)),
        Some(ConstPoolItem::Long(l)) => frame.push_long(*l),
        Some(ConstPoolItem::Double(d)) => frame.push_double(*d),
        Some(ConstPoolItem::RawString(_) | ConstPoolItem::Class(_)) => todo!(),
        _ => unreachable!("Invalid ldc/ldc_w/ldc2_w     index: {}", index,),
    }
}

fn get_field(frame: &mut Frame, field: &Field, storage: &FieldStorage) {
    // Correct allignment guaranteed because it is used to construct the FieldStorage layout and is stored immutably
    match field.descriptor {
        Typ::Bool | Typ::Byte => frame.stack.push(Value::from_int(
            storage.read_i8(field.byte_offset).unwrap() as i32,
        )),
        Typ::Short | Typ::Char => frame.stack.push(Value::from_int(
            storage.read_i16(field.byte_offset).unwrap() as i32,
        )),
        Typ::Int => frame.stack.push(Value::from_int(
            storage.read_i32(field.byte_offset).unwrap(),
        )),
        Typ::Float => frame.stack.push(Value::from_float(
            storage.read_f32(field.byte_offset).unwrap(),
        )),
        Typ::Long => frame.push_long(storage.read_i64(field.byte_offset).unwrap()),
        Typ::Double => frame.push_double(storage.read_f64(field.byte_offset).unwrap()),
        Typ::Ref(..) => frame.stack.push(Value::from_ref(unsafe {
            std::mem::transmute(storage.read_ptr(field.byte_offset).unwrap())
        })),
    }
}

fn put_field(frame: &mut Frame, field: &Field, storage: &FieldStorage) {
    match field.descriptor {
        Typ::Bool | Typ::Byte => storage
            .write_i8(field.byte_offset, frame.pop().as_int() as i8)
            .unwrap(),
        Typ::Short | Typ::Char => storage
            .write_i16(field.byte_offset, frame.pop().as_int() as i16)
            .unwrap(),
        Typ::Int => storage
            .write_i32(field.byte_offset, frame.pop().as_int())
            .unwrap(),
        Typ::Float => storage
            .write_f32(field.byte_offset, frame.pop().as_float())
            .unwrap(),
        Typ::Long => storage
            .write_i64(field.byte_offset, frame.pop_long())
            .unwrap(),
        Typ::Double => storage
            .write_f64(field.byte_offset, frame.pop_double())
            .unwrap(),
        Typ::Ref(..) => storage.write_ptr(field.byte_offset, frame.pop().0).unwrap(),
    }
}

fn execute_invoke_instr<'jvm, 'b>(
    jvm: &'b JVM<'jvm>,
    frame: &'b mut Frame<'jvm>,
    method: &'jvm Method<'jvm>,
) -> JVMResult<'jvm, ()> {
    let mut arg_slots = method.nat.typ.arg_slots();
    if !method.access_flags.contains(AccessFlags::STATIC) {
        // `this` is treated as an argument, but not mentioned in method descriptor
        arg_slots += 1;
    }

    let args = &frame.stack[frame.stack.len() - arg_slots..];
    let result = invoke_initialized(jvm, method, args)?;
    frame.stack.truncate(frame.stack.len() - arg_slots);
    match result {
        None => (),
        Some(JVMValue::Ref(object)) => frame.stack.push(Value::from_ref(object)),
        Some(JVMValue::Int(value)) => frame.stack.push(Value::from_int(value)),
        Some(JVMValue::Float(value)) => frame.stack.push(Value::from_float(value)),
        Some(JVMValue::Long(value)) => frame.push_long(value),
        Some(JVMValue::Double(value)) => frame.push_double(value),
    };
    Ok(())
}

impl Value {
    fn from_int(value: i32) -> Self {
        Self(value as u32 as SlotSize)
    }

    fn from_float(value: f32) -> Self {
        Self(value.to_bits() as SlotSize)
    }

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

    fn from_ref(object: Object) -> Self {
        Self(object.ptr())
    }

    fn as_int(self) -> i32 {
        self.0 as u32 as i32
    }

    fn as_float(self) -> f32 {
        f32::from_bits(self.0 as u32)
    }

    fn as_long(low: Value, high: Value) -> i64 {
        (low.0 as u32 as u64 + ((high.0 as u64) << 32)) as i64
    }

    fn as_double(low: Value, high: Value) -> f64 {
        f64::from_bits(low.0 as u32 as u64 + ((high.0 as u64) << 32))
    }

    unsafe fn as_ref<'jvm>(self) -> Object<'jvm> {
        Object::from_ptr(self.0)
    }
}

struct Frame<'jvm> {
    method: &'jvm Method<'jvm>,
    code_bytes: &'jvm [u8],
    locals: Vec<Value>,
    stack: Vec<Value>,
    pc: u16,
}

impl<'jvm> Frame<'jvm> {
    fn jump_relative(&mut self, base: u16, target_offset: i32) {
        self.pc = (base as i32 + target_offset as i32) as u16;
    }

    fn load_long(&self, index: u16) -> i64 {
        Value::as_long(self.locals[index as usize], self.locals[index as usize + 1])
    }

    fn load_double(&self, index: u16) -> f64 {
        Value::as_double(self.locals[index as usize], self.locals[index as usize + 1])
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("Popped empty stack")
    }

    fn pop_long(&mut self) -> i64 {
        let (high, low) = (self.pop(), self.pop());
        Value::as_long(low, high)
    }

    fn pop_double(&mut self) -> f64 {
        let (high, low) = (self.pop(), self.pop());
        Value::as_double(low, high)
    }

    fn push_long(&mut self, value: i64) {
        let (low, high) = Value::from_long(value);
        self.stack.push(low);
        self.stack.push(high);
    }

    fn push_double(&mut self, value: f64) {
        let (low, high) = Value::from_double(value);
        self.stack.push(low);
        self.stack.push(high);
    }

    /// This always succeeds because bounds were already checked during verification
    fn read_code_u8(&mut self) -> u8 {
        self.pc += 1;
        self.code_bytes[self.pc as usize - 1]
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
