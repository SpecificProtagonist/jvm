use std::{cell::Cell, mem::size_of, usize};

use crate::{
    const_pool::{ConstPool, ConstPoolItem},
    field_storage::FieldStorage,
    instructions::*,
    object::{self, Object},
    AccessFlags, Field, Method, Typ, JVM,
};
use anyhow::{anyhow, bail, Context, Result};

/// A value on the stack or in the local variables
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LocalValue<'jvm> {
    Uninitialized,
    Ref(Object<'jvm>),
    // Only in class version < 50.0:
    // ReturnAddress(u16),
    Int(i32),
    /// Both on stack & locals, the low half is stored first
    LowHalfOfLong(u32),
    HighHalfOfLong(u32),
    Float(f32),
    /// Both on stack & locals, the low half is stored first
    LowHalfOfDouble(u32),
    HighHalfOfDouble(u32),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ReturnValue<'jvm> {
    Void,
    Ref(Object<'jvm>),
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
}

pub fn invoke<'a, 'b>(
    jvm: &'b JVM<'a>,
    method: &'a Method<'a>,
    args: &'b [LocalValue<'a>],
) -> Result<ReturnValue<'a>> {
    method.class.get().ensure_init(jvm)?;
    invoke_initialized(jvm, method, args).with_context(|| format!("Trying to call {}", method.nat))
}

pub(crate) fn invoke_initialized<'a, 'b>(
    jvm: &'b JVM<'a>,
    method: &'a Method<'a>,
    args: &'b [LocalValue<'a>],
) -> Result<ReturnValue<'a>> {
    if method.code.is_none() {
        todo!("Methods without code attribute");
    }

    let mut frame = Frame {
        method,
        code_bytes: &method.code.as_ref().unwrap().bytes,
        locals: {
            let max_locals = method.code.as_ref().unwrap().max_locals as usize;
            let mut locals = Vec::with_capacity(max_locals);
            locals.extend_from_slice(args);
            while locals.len() < max_locals {
                locals.push(LocalValue::Uninitialized);
            }
            locals
        },
        stack: Vec::with_capacity(method.code.as_ref().unwrap().max_stack as usize),
        pc: 0,
    };

    // Todo: bytecode verification, esp. about pc
    loop {
        match frame.read_code_u8() {
            NOP => {}
            ICONST_M1 => frame.push(LocalValue::Int(-1))?,
            ICONST_0 => frame.push(LocalValue::Int(0))?,
            ICONST_1 => frame.push(LocalValue::Int(1))?,
            ICONST_2 => frame.push(LocalValue::Int(2))?,
            ICONST_3 => frame.push(LocalValue::Int(3))?,
            ICONST_4 => frame.push(LocalValue::Int(4))?,
            ICONST_5 => frame.push(LocalValue::Int(5))?,
            LCONST_0 => frame.push_long(0)?,
            LCONST_1 => frame.push_long(1)?,
            FCONST_0 => frame.push(LocalValue::Float(0.0))?,
            FCONST_1 => frame.push(LocalValue::Float(1.0))?,
            FCONST_2 => frame.push(LocalValue::Float(2.0))?,
            DCONST_0 => frame.push_double(0.0)?,
            DCONST_1 => frame.push_double(1.0)?,
            BIPUSH => {
                let value = frame.read_code_u8() as i8;
                frame.push(LocalValue::Int(value as i32))?;
            }
            SIPUSH => {
                let value = frame.read_code_u16() as i16;
                frame.push(LocalValue::Int(value as i32))?;
            }
            LDC => {
                let index = frame.read_code_u8() as u16;
                ldc(&mut frame, &method.class.get().const_pool, index)?;
            }
            LDC_W => {
                let index = frame.read_code_u16();
                ldc(&mut frame, &method.class.get().const_pool, index)?;
            }
            LDC2_W => {
                let index = frame.read_code_u16();
                ldc(&mut frame, &method.class.get().const_pool, index)?;
            }
            ILOAD | FLOAD | ALOAD => {
                let index = frame.read_code_u8() as u16;
                frame.push(frame.load(index)?)?;
            }
            LLOAD => {
                let index = frame.read_code_u8() as u16;
                frame.push_long(frame.load_long(index)?)?;
            }
            DLOAD => {
                let index = frame.read_code_u8() as u16;
                frame.push_double(frame.load_double(index)?)?;
            }
            ILOAD_0 | FLOAD_0 | ALOAD_0 => frame.push(frame.load(0)?)?,
            ILOAD_1 | FLOAD_1 | ALOAD_1 => frame.push(frame.load(1)?)?,
            ILOAD_2 | FLOAD_2 | ALOAD_2 => frame.push(frame.load(2)?)?,
            ILOAD_3 | FLOAD_3 | ALOAD_3 => frame.push(frame.load(3)?)?,
            LLOAD_0 => frame.push_long(frame.load_long(0)?)?,
            LLOAD_1 => frame.push_long(frame.load_long(1)?)?,
            LLOAD_2 => frame.push_long(frame.load_long(2)?)?,
            LLOAD_3 => frame.push_long(frame.load_long(3)?)?,
            DLOAD_0 => frame.push_double(frame.load_double(0)?)?,
            DLOAD_1 => frame.push_double(frame.load_double(1)?)?,
            DLOAD_2 => frame.push_double(frame.load_double(2)?)?,
            DLOAD_3 => frame.push_double(frame.load_double(3)?)?,
            IALOAD => {
                let index = frame.pop_int()?;
                let obj = frame.pop_ref()?;
                if obj.null() {
                    bail!("NullPointerException");
                }
                if obj.class().element_type == Some(Typ::Int) {
                    frame.push(LocalValue::Int(
                        obj.data
                            .read_i32((object::header_size() as isize + 4 * index as isize) as u32)
                            .ok_or_else(|| anyhow!("index out of bounds"))?,
                    ))?;
                } else {
                    bail!("expected int array")
                }
            }
            LALOAD => {
                let index = frame.pop_int()?;
                let obj = frame.pop_ref()?;
                if obj.null() {
                    bail!("NullPointerException");
                }
                if obj.class().element_type == Some(Typ::Long) {
                    frame.push_long(
                        obj.data
                            .read_i64((object::header_size() as isize + 8 * index as isize) as u32)
                            .ok_or_else(|| anyhow!("index out of bounds"))?,
                    )?;
                } else {
                    bail!("expected long array")
                }
            }
            FALOAD => {
                let index = frame.pop_int()?;
                let obj = frame.pop_ref()?;
                if obj.null() {
                    bail!("NullPointerException");
                }
                if obj.class().element_type == Some(Typ::Float) {
                    frame.push(LocalValue::Float(
                        obj.data
                            .read_f32((object::header_size() as isize + 4 * index as isize) as u32)
                            .ok_or_else(|| anyhow!("index out of bounds"))?,
                    ))?;
                } else {
                    bail!("expected float array")
                }
            }
            DALOAD => {
                let index = frame.pop_int()?;
                let obj = frame.pop_ref()?;
                if obj.null() {
                    bail!("NullPointerException");
                }
                if obj.class().element_type == Some(Typ::Double) {
                    frame.push_double(
                        obj.data
                            .read_f64((object::header_size() as isize + 8 * index as isize) as u32)
                            .ok_or_else(|| anyhow!("index out of bounds"))?,
                    )?;
                } else {
                    bail!("expected double array")
                }
            }
            AALOAD => {
                let index = frame.pop_int()?;
                let obj = frame.pop_ref()?;
                if obj.null() {
                    bail!("NullPointerException");
                }
                if matches!(obj.class().element_type, Some(Typ::Ref(_))) {
                    frame.push(LocalValue::Ref(unsafe {
                        std::mem::transmute(
                            obj.data
                                .read_usize(
                                    (object::header_size() as isize
                                        + size_of::<usize>() as isize * index as isize)
                                        as u32,
                                )
                                .ok_or_else(|| anyhow!("index out of bounds"))?,
                        )
                    }))?;
                } else {
                    bail!("expected object array")
                }
            }
            BALOAD => {
                let index = frame.pop_int()?;
                let obj = frame.pop_ref()?;
                if obj.null() {
                    bail!("NullPointerException");
                }
                if matches!(obj.class().element_type, Some(Typ::Byte | Typ::Bool)) {
                    frame.push(LocalValue::Int(
                        obj.data
                            .read_i8((object::header_size() as isize + index as isize) as u32)
                            .ok_or_else(|| anyhow!("index out of bounds"))?
                            as i32,
                    ))?;
                } else {
                    bail!("expected bool or byte array")
                }
            }
            CALOAD => {
                let index = frame.pop_int()?;
                let obj = frame.pop_ref()?;
                if obj.null() {
                    bail!("NullPointerException");
                }
                if obj.class().element_type == Some(Typ::Char) {
                    frame.push(LocalValue::Int(
                        obj.data
                            .read_i16((object::header_size() as isize + 2 * index as isize) as u32)
                            .ok_or_else(|| anyhow!("index out of bounds"))?
                            as u16 as i32,
                    ))?;
                } else {
                    bail!("expected char array")
                }
            }
            SALOAD => {
                let index = frame.pop_int()?;
                let obj = frame.pop_ref()?;
                if obj.null() {
                    bail!("NullPointerException");
                }
                if obj.class().element_type == Some(Typ::Short) {
                    frame.push(LocalValue::Int(
                        obj.data
                            .read_i16((object::header_size() as isize + 2 * index as isize) as u32)
                            .ok_or_else(|| anyhow!("index out of bounds"))?
                            as i32,
                    ))?;
                } else {
                    bail!("expected short array")
                }
            }
            ISTORE => {
                let index = frame.read_code_u8() as u16;
                let value = frame.pop_int()?;
                frame.store(index, LocalValue::Int(value))?;
            }
            LSTORE => {
                let index = frame.read_code_u8() as u16;
                let value = frame.pop_long()?;
                frame.store_long(index, value)?;
            }
            FSTORE => {
                let index = frame.read_code_u8() as u16;
                let value = frame.pop_float()?;
                frame.store(index, LocalValue::Float(value))?;
            }
            DSTORE => {
                let index = frame.read_code_u8() as u16;
                let value = frame.pop_double()?;
                frame.store_double(index, value)?;
            }
            ASTORE => {
                let index = frame.read_code_u8() as u16;
                let value = frame.pop_ref()?;
                frame.store(index, LocalValue::Ref(value))?;
            }
            ISTORE_0 => {
                let value = frame.pop_int()?;
                frame.store(0, LocalValue::Int(value))?;
            }
            ISTORE_1 => {
                let value = frame.pop_int()?;
                frame.store(1, LocalValue::Int(value))?;
            }
            ISTORE_2 => {
                let value = frame.pop_int()?;
                frame.store(2, LocalValue::Int(value))?;
            }
            ISTORE_3 => {
                let value = frame.pop_int()?;
                frame.store(3, LocalValue::Int(value))?;
            }
            LSTORE_0 => {
                let value = frame.pop_long()?;
                frame.store_long(0, value)?;
            }
            LSTORE_1 => {
                let value = frame.pop_long()?;
                frame.store_long(1, value)?;
            }
            LSTORE_2 => {
                let value = frame.pop_long()?;
                frame.store_long(2, value)?;
            }
            LSTORE_3 => {
                let value = frame.pop_long()?;
                frame.store_long(3, value)?;
            }
            FSTORE_0 => {
                let value = frame.pop_float()?;
                frame.store(0, LocalValue::Float(value))?;
            }
            FSTORE_1 => {
                let value = frame.pop_float()?;
                frame.store(1, LocalValue::Float(value))?;
            }
            FSTORE_2 => {
                let value = frame.pop_float()?;
                frame.store(2, LocalValue::Float(value))?;
            }
            FSTORE_3 => {
                let value = frame.pop_float()?;
                frame.store(3, LocalValue::Float(value))?;
            }
            DSTORE_0 => {
                let value = frame.pop_double()?;
                frame.store_double(0, value)?;
            }
            DSTORE_1 => {
                let value = frame.pop_double()?;
                frame.store_double(1, value)?;
            }
            DSTORE_2 => {
                let value = frame.pop_double()?;
                frame.store_double(2, value)?;
            }
            DSTORE_3 => {
                let value = frame.pop_double()?;
                frame.store_double(3, value)?;
            }
            ASTORE_0 => {
                let value = frame.pop_ref()?;
                frame.store(0, LocalValue::Ref(value))?;
            }
            ASTORE_1 => {
                let value = frame.pop_ref()?;
                frame.store(1, LocalValue::Ref(value))?;
            }
            ASTORE_2 => {
                let value = frame.pop_ref()?;
                frame.store(2, LocalValue::Ref(value))?;
            }
            ASTORE_3 => {
                let value = frame.pop_ref()?;
                frame.store(3, LocalValue::Ref(value))?;
            }
            IASTORE => {
                let value = frame.pop_int()?;
                let index = frame.pop_int()?;
                let obj = frame.pop_ref()?;
                if obj.null() {
                    bail!("NullPointerException");
                }
                if obj.class().element_type == Some(Typ::Int) {
                    obj.data
                        .write_i32(
                            (object::header_size() as isize + 4 * index as isize) as u32,
                            value,
                        )
                        .ok_or_else(|| anyhow!("index out of bounds"))?;
                } else {
                    bail!("expected int array")
                }
            }
            LASTORE => {
                let value = frame.pop_long()?;
                let index = frame.pop_int()?;
                let obj = frame.pop_ref()?;
                if obj.null() {
                    bail!("NullPointerException");
                }
                if obj.class().element_type == Some(Typ::Long) {
                    obj.data
                        .write_i64(
                            (object::header_size() as isize + 8 * index as isize) as u32,
                            value,
                        )
                        .ok_or_else(|| anyhow!("index out of bounds"))?;
                } else {
                    bail!("expected long array")
                }
            }
            FASTORE => {
                let value = frame.pop_float()?;
                let index = frame.pop_int()?;
                let obj = frame.pop_ref()?;
                if obj.null() {
                    bail!("NullPointerException");
                }
                if obj.class().element_type == Some(Typ::Float) {
                    obj.data
                        .write_f32(
                            (object::header_size() as isize + 4 * index as isize) as u32,
                            value,
                        )
                        .ok_or_else(|| anyhow!("index out of bounds"))?;
                } else {
                    bail!("expected float array")
                }
            }
            DASTORE => {
                let value = frame.pop_double()?;
                let index = frame.pop_int()?;
                let obj = frame.pop_ref()?;
                if obj.null() {
                    bail!("NullPointerException");
                }
                if obj.class().element_type == Some(Typ::Double) {
                    obj.data
                        .write_f64(
                            (object::header_size() as isize + 8 * index as isize) as u32,
                            value,
                        )
                        .ok_or_else(|| anyhow!("index out of bounds"))?;
                } else {
                    bail!("expected double array")
                }
            }
            AASTORE => {
                let value = frame.pop_ref()?;
                let index = frame.pop_int()?;
                let obj = frame.pop_ref()?;
                if obj.null() {
                    bail!("NullPointerException");
                }
                if let Some(Typ::Ref(component)) = obj.class().element_type {
                    if !obj.class().assignable_to(jvm.resolve_class(component)?) {
                        bail!("ArrayStoreException")
                    }
                    obj.data
                        .write_usize(
                            (object::header_size() as isize
                                + size_of::<usize>() as isize * index as isize)
                                as u32,
                            value.data.addr(),
                        )
                        .ok_or_else(|| anyhow!("index out of bounds"))?;
                } else {
                    bail!("expected object array")
                }
            }
            BASTORE => {
                let value = frame.pop_int()?;
                let index = frame.pop_int()?;
                let obj = frame.pop_ref()?;
                if obj.null() {
                    bail!("NullPointerException");
                }
                if matches!(obj.class().element_type, Some(Typ::Bool | Typ::Byte)) {
                    obj.data
                        .write_i8(
                            (object::header_size() as isize + index as isize) as u32,
                            value as i8,
                        )
                        .ok_or_else(|| anyhow!("index out of bounds"))?;
                } else {
                    bail!("expected boolean or byte array")
                }
            }
            CASTORE => {
                let value = frame.pop_int()?;
                let index = frame.pop_int()?;
                let obj = frame.pop_ref()?;
                if obj.null() {
                    bail!("NullPointerException");
                }
                if obj.class().element_type == Some(Typ::Char) {
                    obj.data
                        .write_i16(
                            (object::header_size() as isize + 2 * index as isize) as u32,
                            value as u16 as i16,
                        )
                        .ok_or_else(|| anyhow!("index out of bounds"))?;
                } else {
                    bail!("expected char array")
                }
            }
            SASTORE => {
                let value = frame.pop_int()?;
                let index = frame.pop_int()?;
                let obj = frame.pop_ref()?;
                if obj.null() {
                    bail!("NullPointerException");
                }
                if obj.class().element_type == Some(Typ::Short) {
                    obj.data
                        .write_i16(
                            (object::header_size() as isize + 2 * index as isize) as u32,
                            value as i16,
                        )
                        .ok_or_else(|| anyhow!("index out of bounds"))?;
                } else {
                    bail!("expected short array")
                }
            }
            POP => {
                if frame.pop()?.category_2() {
                    bail!("Tried to pop category 2 value")
                }
            }
            POP2 => {
                frame.pop()?;
                frame.pop()?;
            }
            DUP => {
                let value = frame.pop()?;
                frame.push(value)?;
                frame.push(value)?;
            }
            DUP_X1 => {
                let first = frame.pop()?;
                let second = frame.pop()?;
                frame.push(first)?;
                frame.push(second)?;
                frame.push(first)?;
            }
            DUP_X2 => {
                let first = frame.pop()?;
                let second = frame.pop()?;
                let third = frame.pop()?;
                frame.push(first)?;
                frame.push(third)?;
                frame.push(second)?;
                frame.push(first)?;
            }
            DUP2 => {
                let first = frame.pop()?;
                let second = frame.pop()?;
                frame.push(second)?;
                frame.push(first)?;
                frame.push(second)?;
                frame.push(first)?;
            }
            DUP2_X1 => {
                let first = frame.pop()?;
                let second = frame.pop()?;
                let third = frame.pop()?;
                frame.push(second)?;
                frame.push(first)?;
                frame.push(third)?;
                frame.push(second)?;
                frame.push(first)?;
            }
            DUP2_X2 => {
                let first = frame.pop()?;
                let second = frame.pop()?;
                let third = frame.pop()?;
                let fourth = frame.pop()?;
                frame.push(second)?;
                frame.push(first)?;
                frame.push(fourth)?;
                frame.push(third)?;
                frame.push(second)?;
                frame.push(first)?;
            }
            SWAP => {
                let first = frame.pop()?;
                let second = frame.pop()?;
                frame.push(first)?;
                frame.push(second)?;
            }
            IADD | ISUB | IMUL | IDIV | IREM | ISHL | ISHR | IUSHR | IAND | IOR | IXOR => {
                let b = frame.pop_int()?;
                let a = frame.pop_int()?;
                let result = match frame.read_previous_byte() {
                    IADD => a + b,
                    ISUB => a - b,
                    IMUL => a * b,
                    IDIV => a.checked_div(b).ok_or_else(|| anyhow!("idiv by 0"))?,
                    IREM => {
                        if b != 0 {
                            a % b
                        } else {
                            bail!("irem by 0")
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
                frame.push(LocalValue::Int(result))?;
            }
            LADD | LSUB | LMUL | LDIV | LREM | LSHL | LSHR | LUSHR | LAND | LOR | LXOR => {
                let b = frame.pop_long()?;
                let a = frame.pop_long()?;
                let result = match frame.read_previous_byte() {
                    LADD => a + b,
                    LSUB => a - b,
                    LMUL => a * b,
                    LDIV => a.checked_div(b).ok_or_else(|| anyhow!("idiv by 0"))?,
                    LREM => {
                        if b != 0 {
                            a % b
                        } else {
                            bail!("irem by 0")
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
                frame.push_long(result)?;
            }
            FADD | FSUB | FMUL | FDIV | FREM => {
                let b = frame.pop_float()?;
                let a = frame.pop_float()?;
                let result = match frame.read_previous_byte() {
                    FADD => a + b,
                    FSUB => a - b,
                    FMUL => a * b,
                    FDIV => a / b,
                    FREM => a % b,
                    _ => unreachable!(),
                };
                frame.push(LocalValue::Float(result))?;
            }
            DADD | DSUB | DMUL | DDIV | DREM => {
                let b = frame.pop_double()?;
                let a = frame.pop_double()?;
                let result = match frame.read_previous_byte() {
                    DADD => a + b,
                    DSUB => a - b,
                    DMUL => a * b,
                    DDIV => a / b,
                    DREM => a % b,
                    _ => unreachable!(),
                };
                frame.push_double(result)?;
            }
            INEG => {
                let result = -frame.pop_int()?;
                frame.push(LocalValue::Int(result))?;
            }
            LNEG => {
                let result = -frame.pop_long()?;
                frame.push_long(result)?;
            }
            FNEG => {
                let result = -frame.pop_float()?;
                frame.push(LocalValue::Float(result))?;
            }
            DNEG => {
                let result = -frame.pop_long()?;
                frame.push_long(result)?;
            }
            IINC => {
                let index = frame.read_code_u8() as u16;
                let constant = frame.read_code_u8() as i32;
                frame.store(index, LocalValue::Int(frame.load_i32(index)? + constant))?;
            }
            I2L => {
                let value = frame.pop_int()?;
                frame.push_long(value as i64)?;
            }
            I2F => {
                let value = frame.pop_int()?;
                frame.push(LocalValue::Float(value as f32))?;
            }
            I2D => {
                let value = frame.pop_int()?;
                frame.push_double(value as f64)?;
            }
            L2I => {
                let value = frame.pop_long()?;
                frame.push(LocalValue::Int(value as i32))?;
            }
            L2F => {
                let value = frame.pop_long()?;
                frame.push(LocalValue::Float(value as f32))?;
            }
            L2D => {
                let value = frame.pop_long()?;
                frame.push_double(value as f64)?;
            }
            F2I => {
                let value = frame.pop_float()?;
                frame.push(LocalValue::Int(value as i32))?;
            }
            F2L => {
                let value = frame.pop_float()?;
                frame.push_long(value as i64)?;
            }
            F2D => {
                let value = frame.pop_float()?;
                frame.push_double(value as f64)?;
            }
            D2I => {
                let value = frame.pop_double()?;
                frame.push(LocalValue::Int(value as i32))?;
            }
            D2L => {
                let value = frame.pop_double()?;
                frame.push_long(value as i64)?;
            }
            D2F => {
                let value = frame.pop_double()?;
                frame.push(LocalValue::Float(value as f32))?;
            }
            I2B => {
                let value = frame.pop_int()? as i8;
                frame.push(LocalValue::Int(value as i32))?;
            }
            I2C => {
                let value = frame.pop_int()? as u16;
                frame.push(LocalValue::Int(value as i32))?;
            }
            I2S => {
                let value = frame.pop_int()? as i16;
                frame.push(LocalValue::Int(value as i32))?;
            }
            LCMP => {
                let b = frame.pop_long()?;
                let a = frame.pop_long()?;
                frame.push(LocalValue::Int(match a - b {
                    x if x > 0 => 1,
                    0 => 0,
                    _ => -1,
                }))?
            }
            FCMPL | FCMPG => {
                let b = frame.pop_float()?;
                let a = frame.pop_float()?;
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
                frame.push(LocalValue::Int(result))?
            }
            DCMPL | DCMPG => {
                let b = frame.pop_double()?;
                let a = frame.pop_double()?;
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
                frame.push(LocalValue::Int(result))?
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
                let b = if two_operand { frame.pop_int()? } else { 0 };
                let a = frame.pop_int()?;
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
            // Only supported in class version < 50.0
            /*JSR => {
                let base = frame.pc - 1;
                let target = frame.read_code_u16()? as i16 as i32;
                frame.push(LocalValue::ReturnAddress(frame.pc))?;
                frame.jump_relative(base, target)
            }
            RET => {
                let index = frame.read_code_u8()?;
                if let LocalValue::ReturnAddress(branch_target) = frame.load(index as u16)? {
                    frame.pc = branch_target;
                } else {
                    bail!("Failed to load return address from slot {}", index);
                }
            }*/
            TABLESWITCH => {
                let base = frame.pc - 1;

                // Skip up to 3 byte padding
                frame.pc += 3;
                frame.pc &= 0xfffc;

                let index = frame.pop_int()?;
                let default = frame.read_code_u32() as i32;
                let low = frame.read_code_u32() as i32;
                let high = frame.read_code_u32() as i32;

                if low > high {
                    bail!("Invalid tableswitch")
                }

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

                let key = frame.pop_int()?;
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
            IRETURN => return Ok(ReturnValue::Int(frame.pop_int()?)),
            LRETURN => return Ok(ReturnValue::Long(frame.pop_long()?)),
            FRETURN => return Ok(ReturnValue::Float(frame.pop_float()?)),
            DRETURN => return Ok(ReturnValue::Double(frame.pop_double()?)),
            ARETURN => return Ok(ReturnValue::Ref(frame.pop_ref()?)),
            RETURN => return Ok(ReturnValue::Void),
            GETSTATIC => {
                let index = frame.read_code_u16();
                let field = method.class.get().const_pool.get_field(index).unwrap();
                field.class.get().ensure_init(jvm)?;
                get_field(&mut frame, field, &field.class.get().static_storage)?;
            }
            PUTSTATIC => {
                let index = frame.read_code_u16();
                let field = method.class.get().const_pool.get_field(index).unwrap();
                field.class.get().ensure_init(jvm)?;
                put_field(&mut frame, field, &field.class.get().static_storage)?;
            }
            GETFIELD => {
                let index = frame.read_code_u16();
                let field = method.class.get().const_pool.get_field(index)?;
                let object = frame.pop_ref()?;
                get_field(&mut frame, field, &object.data)?;
            }
            PUTFIELD => {
                let index = frame.read_code_u16();
                let field = method.class.get().const_pool.get_field(index)?;
                // Stack has object ref first, then value on top
                // This is ugly
                let object = if matches!(field.descriptor, Typ::Long | Typ::Double) {
                    let high = frame.pop()?;
                    let low = frame.pop()?;
                    let obj = frame.pop_ref()?;
                    frame.push(low)?;
                    frame.push(high)?;
                    obj
                } else {
                    let value = frame.pop()?;
                    let obj = frame.pop_ref()?;
                    frame.push(value)?;
                    obj
                };
                put_field(&mut frame, field, &object.data)?;
            }
            INVOKEVIRTUAL => {
                let index = frame.read_code_u16();
                let (_, nat) = method.class.get().const_pool.get_virtual_method(index)?;
                if nat.name.0.starts_with('<') {
                    bail!("Must not invokevirtual class or instance initialization method")
                }
                let obj = *if let Some(LocalValue::Ref(obj)) = frame.stack.last() {
                    obj
                } else {
                    bail!("Failed invoke_special")
                };
                if obj.null() {
                    bail!("NullPointerException")
                }
                let invoke_method = obj
                    .class()
                    .methods
                    .get(&nat)
                    .ok_or_else(|| anyhow!("Method not found"))?;
                execute_invoke_instr(jvm, &mut frame, invoke_method)?;
            }
            INVOKESPECIAL => {
                let index = frame.read_code_u16();
                let (named_class, nat) = method.class.get().const_pool.get_virtual_method(index)?;

                let class = if (nat.name.0 != "<init>")
                    & method.class.get().subclass_of(named_class)
                    & method.class.get().access_flags.contains(AccessFlags::SUPER)
                {
                    method
                        .class
                        .get()
                        .super_class
                        .ok_or_else(|| anyhow!("Invalid invokespecial"))?
                } else {
                    named_class
                };

                let invoke_method = class
                    .methods
                    .get(&nat)
                    .ok_or_else(|| anyhow!("Method not found"))?;
                execute_invoke_instr(jvm, &mut frame, invoke_method)?;
            }
            INVOKESTATIC => {
                let index = frame.read_code_u16();
                let class = method.class.get();
                class.ensure_init(jvm)?;
                let invoke_method = class.const_pool.get_static_method(index)?;
                execute_invoke_instr(jvm, &mut frame, invoke_method)?;
            }
            NEW => {
                let index = frame.read_code_u16();
                let class = method.class.get().const_pool.get_class(index)?;
                class.ensure_init(jvm)?;
                frame.push(LocalValue::Ref(jvm.create_object(class)))?
            }
            NEWARRAY => {
                let length = frame.pop_int()?;
                if length < 0 {
                    bail!("Negative array length")
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
                    _ => bail!("invalid array type"),
                };
                let typ = jvm.resolve_class(typ)?;
                frame.push(LocalValue::Ref(jvm.create_array(typ, length as usize)))?;
            }
            ANEWARRAY => {
                let length = frame.pop_int()?;
                let index = frame.read_code_u16();
                let component = method.class.get().const_pool.get_class(index)?;
                // TODO: make it easier to refer to array types
                let typ = jvm.resolve_class(format!("[L{};", component.name))?;
                if length < 0 {
                    bail!("Negative array length")
                }
                frame.push(LocalValue::Ref(jvm.create_array(typ, length as usize)))?;
            }
            ARRAYLENGTH => {
                let obj = frame.pop_ref()?;
                if obj.null() {
                    bail!("NullPointerException");
                }
                if let Some(base) = obj.class().element_type {
                    let length =
                        ((obj.data.size() - object::header_size()) / base.layout().size()) as i32;
                    frame.push(LocalValue::Int(length))?;
                } else {
                    bail!("object not array")
                }
            }
            MULTIANEWARRAY => {
                todo!()
            }
            IF_NULL => {
                let base = frame.pc - 1;
                let target_offset = frame.read_code_u16() as i16;
                if frame.pop_ref()?.null() {
                    frame.jump_relative(base, target_offset as i32);
                }
            }
            GOTO_W => {
                let base = frame.pc - 1;
                let target_offset = frame.read_code_u32() as i32;
                frame.jump_relative(base, target_offset)
            }
            // Only supported in class version < 50.0
            /*JSR_W => {
                let base = frame.pc - 1;
                let target = frame.read_code_u32()? as i32;
                frame.push(LocalValue::ReturnAddress(frame.pc))?;
                frame.jump_relative(base, target);
            }*/
            other => todo!("Bytecode: {}", other),
        }
    }
}

fn ldc(frame: &mut Frame, const_pool: &ConstPool, index: u16) -> Result<()> {
    match const_pool.items.get(index as usize).map(Cell::get) {
        Some(ConstPoolItem::Integer(i)) => frame.push(LocalValue::Int(i)),
        Some(ConstPoolItem::Float(f)) => frame.push(LocalValue::Float(f)),
        Some(ConstPoolItem::Long(l)) => frame.push_long(l),
        Some(ConstPoolItem::Double(d)) => frame.push_double(d),
        Some(ConstPoolItem::RawString(_) | ConstPoolItem::Class(_)) => todo!(),
        _ => bail!("Invalid ldc/ldc_w/ldc2_w     index: {}", index,),
    }
}

fn get_field(frame: &mut Frame, field: &Field, storage: &FieldStorage) -> Result<()> {
    // Correct allignment guaranteed because it is used to construct the FieldStorage layout and is stored immutably
    match field.descriptor {
        Typ::Bool | Typ::Byte => frame.push(LocalValue::Int(
            storage.read_i8(field.byte_offset).unwrap() as i32,
        ))?,
        Typ::Short | Typ::Char => frame.push(LocalValue::Int(
            storage.read_i16(field.byte_offset).unwrap() as i32,
        ))?,
        Typ::Int => frame.push(LocalValue::Int(
            storage.read_i32(field.byte_offset).unwrap(),
        ))?,
        Typ::Float => frame.push(LocalValue::Float(
            storage.read_f32(field.byte_offset).unwrap(),
        ))?,
        Typ::Long => frame.push_long(storage.read_i64(field.byte_offset).unwrap())?,
        Typ::Double => frame.push_double(storage.read_f64(field.byte_offset).unwrap())?,
        Typ::Ref(..) => frame.push(LocalValue::Ref(unsafe {
            std::mem::transmute(storage.read_usize(field.byte_offset).unwrap())
        }))?,
    }
    Ok(())
}

fn put_field(frame: &mut Frame, field: &Field, storage: &FieldStorage) -> Result<()> {
    match field.descriptor {
        Typ::Bool | Typ::Byte => storage
            .write_i8(field.byte_offset, frame.pop_int()? as i8)
            .unwrap(),
        Typ::Short | Typ::Char => storage
            .write_i16(field.byte_offset, frame.pop_int()? as i16)
            .unwrap(),
        Typ::Int => storage
            .write_i32(field.byte_offset, frame.pop_int()?)
            .unwrap(),
        Typ::Float => storage
            .write_f32(field.byte_offset, frame.pop_float()?)
            .unwrap(),
        Typ::Long => storage
            .write_i64(field.byte_offset, frame.pop_long()?)
            .unwrap(),
        Typ::Double => storage
            .write_f64(field.byte_offset, frame.pop_double()?)
            .unwrap(),
        Typ::Ref(..) => storage
            .write_usize(field.byte_offset, frame.pop_ref()?.data.addr())
            .unwrap(),
    }
    Ok(())
}

fn execute_invoke_instr<'jvm, 'b>(
    jvm: &'b JVM<'jvm>,
    frame: &'b mut Frame<'jvm>,
    method: &'jvm Method<'jvm>,
) -> Result<()> {
    let mut arg_count = 0;
    if !method.access_flags.contains(AccessFlags::STATIC) {
        // `this` is treated as an argument, but not mentioned in method descriptor
        arg_count = 1;
    }
    for typ in &method.nat.typ.0 {
        arg_count += 1;
        // Longs & doubles are double wide...
        if matches!(typ, Typ::Long | Typ::Double) {
            arg_count += 1;
        }
    }
    if arg_count > frame.stack.len() {
        bail!("Invokestatic: not enough elements on stack")
    }
    let args = &frame.stack[frame.stack.len() - arg_count..];
    let result = invoke_initialized(jvm, method, args)
        .with_context(|| format!("Trying to call {}", method.nat.name))?;
    frame.stack.truncate(frame.stack.len() - arg_count);
    match result {
        ReturnValue::Void => Ok(()),
        ReturnValue::Ref(object) => frame.push(LocalValue::Ref(object)),
        ReturnValue::Int(value) => frame.push(LocalValue::Int(value)),
        ReturnValue::Float(value) => frame.push(LocalValue::Float(value)),
        ReturnValue::Long(value) => frame.push_long(value),
        ReturnValue::Double(value) => frame.push_double(value),
    }
}

impl<'jvm> LocalValue<'jvm> {
    fn category_2(self) -> bool {
        matches!(
            self,
            Self::LowHalfOfLong(_)
                | Self::HighHalfOfLong(_)
                | Self::LowHalfOfDouble(_)
                | Self::HighHalfOfDouble(_)
        )
    }
}

impl<'jvm> Default for LocalValue<'jvm> {
    fn default() -> Self {
        LocalValue::Uninitialized
    }
}

impl<'jvm> From<i32> for LocalValue<'jvm> {
    fn from(i: i32) -> Self {
        Self::Int(i)
    }
}

impl<'jvm> From<f32> for LocalValue<'jvm> {
    fn from(f: f32) -> Self {
        Self::Float(f)
    }
}

// I'd use TryFrom, but then type interference fails
impl<'jvm> From<LocalValue<'jvm>> for Result<i32> {
    fn from(value: LocalValue) -> Self {
        if let LocalValue::Int(value) = value {
            Ok(value)
        } else {
            bail!("Value not an int")
        }
    }
}

struct Frame<'jvm> {
    method: &'jvm Method<'jvm>,
    code_bytes: &'jvm [u8],
    locals: Vec<LocalValue<'jvm>>,
    stack: Vec<LocalValue<'jvm>>,
    pc: u16,
}

impl<'jvm> Frame<'jvm> {
    fn jump_relative(&mut self, base: u16, target_offset: i32) {
        self.pc = (base as i32 + target_offset as i32) as u16;
    }

    fn load(&self, index: u16) -> Result<LocalValue<'jvm>> {
        self.locals
            .get(index as usize)
            .copied()
            .ok_or_else(|| anyhow!("Invalid local index"))
    }

    // I would have made get generic but type interference doesn't like its uses
    fn load_i32(&self, index: u16) -> Result<i32> {
        self.load(index)?.into()
    }

    fn store(&mut self, index: u16, value: LocalValue<'jvm>) -> Result<()> {
        *self
            .locals
            .get_mut(index as usize)
            .ok_or_else(|| anyhow!("Invalid locals index"))? = value;
        Ok(())
    }

    fn load_long(&self, index: u16) -> Result<i64> {
        if let (Some(LocalValue::LowHalfOfLong(low)), Some(LocalValue::HighHalfOfLong(high))) = (
            self.locals.get(index as usize),
            self.locals.get(index as usize + 1),
        ) {
            Ok((((*high as u64) << 32) + *low as u64) as i64)
        } else {
            bail!("Failed to load local long from slot {}", index,)
        }
    }

    fn load_double(&self, index: u16) -> Result<f64> {
        if let (Some(LocalValue::LowHalfOfDouble(low)), Some(LocalValue::HighHalfOfDouble(high))) = (
            self.locals.get(index as usize),
            self.locals.get(index as usize + 1),
        ) {
            Ok(f64::from_bits(((*high as u64) << 32) + *low as u64))
        } else {
            bail!("Failed to load local double from slot {}", index)
        }
    }

    fn store_long(&mut self, index: u16, value: i64) -> Result<()> {
        if (index as usize) < self.locals.len() - 1 {
            self.locals[index as usize] = LocalValue::LowHalfOfLong(value as u32);
            self.locals[index as usize + 1] =
                LocalValue::HighHalfOfLong(((value as u64) >> 32) as u32);
            Ok(())
        } else {
            bail!("Failed to store long into slot {}", index)
        }
    }

    fn store_double(&mut self, index: u16, value: f64) -> Result<()> {
        if (index as usize) < self.locals.len() - 1 {
            self.locals[index as usize] = LocalValue::LowHalfOfDouble(value.to_bits() as u32);
            self.locals[index as usize + 1] =
                LocalValue::HighHalfOfDouble((value.to_bits() >> 32) as u32);
            Ok(())
        } else {
            bail!("Failed to store double into slot {}", index)
        }
    }

    fn pop(&mut self) -> Result<LocalValue<'jvm>> {
        self.stack.pop().ok_or_else(|| anyhow!("Pop empty stack"))
    }

    fn pop_int(&mut self) -> Result<i32> {
        match self.pop()? {
            LocalValue::Int(value) => Ok(value),
            other => bail!("Stack value mismatch: not an int ({:?})", other),
        }
    }

    fn pop_float(&mut self) -> Result<f32> {
        match self.pop()? {
            LocalValue::Float(value) => Ok(value),
            other => bail!("Stack value mismatch: not an float ({:?})", other),
        }
    }

    fn pop_long(&mut self) -> Result<i64> {
        match (self.pop()?, self.pop()?) {
            (LocalValue::HighHalfOfLong(high), LocalValue::LowHalfOfLong(low)) => {
                Ok((((high as u64) << 32) + low as u64) as i64)
            }
            other => bail!("Failed to pop long {:?}", other),
        }
    }

    fn pop_double(&mut self) -> Result<f64> {
        match (self.pop()?, self.pop()?) {
            (LocalValue::HighHalfOfDouble(high), LocalValue::LowHalfOfDouble(low)) => {
                Ok(f64::from_bits(((high as u64) << 32) + low as u64))
            }
            other => bail!("Failed to pop double {:?}", other),
        }
    }

    fn pop_ref(&mut self) -> Result<Object<'jvm>> {
        match self.pop()? {
            LocalValue::Ref(value) => Ok(value),
            other => bail!("Stack value mismatch: not an object ({:?})", other),
        }
    }

    fn push(&mut self, value: LocalValue<'jvm>) -> Result<()> {
        if self.stack.len() < self.stack.capacity() {
            self.stack.push(value);
            Ok(())
        } else {
            bail!("Max stack exceeded ({})", self.stack.len(),)
        }
    }

    fn push_long(&mut self, value: i64) -> Result<()> {
        self.push(LocalValue::LowHalfOfLong(value as u32))?;
        self.push(LocalValue::HighHalfOfLong((value as u64 >> 32) as u32))
    }

    fn push_double(&mut self, value: f64) -> Result<()> {
        self.push(LocalValue::LowHalfOfDouble(value.to_bits() as u32))?;
        self.push(LocalValue::HighHalfOfDouble(
            (value.to_bits() as u64 >> 32) as u32,
        ))
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
