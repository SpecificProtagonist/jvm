use std::{cell::Cell, usize};

use crate::{
    const_pool::{ConstPool, ConstPoolItem},
    field_storage::FieldStorage,
    object::{Object, ObjectData},
    AccessFlags, Field, Method, Typ, JVM,
};
use anyhow::{anyhow, bail, Context, Result};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ReturnValue {
    Void,
    Ref(Object),
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
}

/// A value on the stack or in the local variables
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LocalValue {
    Uninitialized,
    Ref(Object),
    ReturnAddress(u16),
    Int(i32),
    /// Both on stack & locals, the low half is stored first
    LowHalfOfLong(u32),
    HighHalfOfLong(u32),
    Float(f32),
    /// Both on stack & locals, the low half is stored first
    LowHalfOfDouble(u32),
    HighHalfOfDouble(u32),
}

impl Default for LocalValue {
    fn default() -> Self {
        LocalValue::Uninitialized
    }
}

// I'd use TryFrom, but then type interference fails
impl From<LocalValue> for Result<i32> {
    fn from(value: LocalValue) -> Self {
        if let LocalValue::Int(value) = value {
            Ok(value)
        } else {
            bail!("Value not an int")
        }
    }
}

struct Frame<'a> {
    // Code has to be accessed via method and unwraped (guaranteed to be Some)
    // to prevent lifetime issues
    // Maybe put Code into a Rc instead of Method?
    method: &'a Method<'a>,
    locals: Vec<LocalValue>,
    stack: Vec<LocalValue>,
    pc: u16,
}

impl<'jvm> Frame<'jvm> {
    fn jump_relative(&mut self, base: u16, target_offset: i32) {
        // TODO: verify the target is an op boundary
        self.pc = (base as i32 + target_offset as i32) as u16;
    }

    fn load(&self, index: u16) -> Result<LocalValue> {
        self.locals
            .get(index as usize)
            .copied()
            .ok_or(anyhow!("Invalid local index"))
    }

    // I would have made get generic but type interference doesn't like its uses
    fn load_i32(&self, index: u16) -> Result<i32> {
        self.load(index)?.into()
    }

    fn store(&mut self, index: u16, value: LocalValue) -> Result<()> {
        *self
            .locals
            .get_mut(index as usize)
            .ok_or(anyhow!("Invalid locals index"))? = value;
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

    fn pop(&mut self) -> Result<LocalValue> {
        self.stack.pop().ok_or(anyhow!("Pop empty stack"))
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

    fn pop_ref(&mut self) -> Result<Object> {
        match self.pop()? {
            LocalValue::Ref(value) => Ok(value),
            other => bail!("Stack value mismatch: not an object ({:?})", other),
        }
    }

    fn push(&mut self, value: LocalValue) -> Result<()> {
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

    fn read_code_u8(&mut self) -> Result<u8> {
        self.pc += 1;
        self.method
            .code
            .as_ref()
            .unwrap()
            .bytes
            .get(self.pc as usize - 1)
            .copied()
            .ok_or(anyhow!("PC out of bounds"))
    }

    fn read_code_u16(&mut self) -> Result<u16> {
        Ok(((self.read_code_u8()? as u16) << 8) + self.read_code_u8()? as u16)
    }

    fn read_code_u32(&mut self) -> Result<u32> {
        Ok(((self.read_code_u16()? as u32) << 16) + self.read_code_u16()? as u32)
    }

    fn read_previous_byte(&mut self) -> u8 {
        self.method.code.as_ref().unwrap().bytes[self.pc as usize - 1]
    }
}

pub fn run<'a, 'b>(
    jvm: &'b JVM<'a>,
    method: &'a Method<'a>,
    args: &'b [LocalValue],
) -> Result<ReturnValue> {
    if method.code.is_none() {
        todo!("Methods without code attribute");
    }

    let mut frame = Frame {
        method,
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
        use instructions::*;
        match frame.read_code_u8()? {
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
                let value = frame.read_code_u8()? as i8;
                frame.push(LocalValue::Int(value as i32))?;
            }
            SIPUSH => {
                let value = frame.read_code_u16()? as i16;
                frame.push(LocalValue::Int(value as i32))?;
            }
            LDC => {
                let index = frame.read_code_u8()? as u16;
                ldc(&mut frame, &method.class.get().const_pool, index)?;
            }
            LDC_W => {
                let index = frame.read_code_u16()?;
                ldc(&mut frame, &method.class.get().const_pool, index)?;
            }
            LDC2_W => {
                let index = frame.read_code_u16()?;
                ldc(&mut frame, &method.class.get().const_pool, index)?;
            }
            ILOAD | FLOAD | ALOAD => {
                let index = frame.read_code_u8()? as u16;
                frame.push(frame.load(index)?)?;
            }
            LLOAD => {
                let index = frame.read_code_u8()? as u16;
                frame.push_long(frame.load_long(index)?)?;
            }
            DLOAD => {
                let index = frame.read_code_u8()? as u16;
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
            ISTORE => {
                let index = frame.read_code_u8()? as u16;
                let value = frame.pop_int()?;
                frame.store(index, LocalValue::Int(value))?;
            }
            LSTORE => {
                let index = frame.read_code_u8()? as u16;
                let value = frame.pop_long()?;
                frame.store_long(index, value)?;
            }
            FSTORE => {
                let index = frame.read_code_u8()? as u16;
                let value = frame.pop_float()?;
                frame.store(index, LocalValue::Float(value))?;
            }
            DSTORE => {
                let index = frame.read_code_u8()? as u16;
                let value = frame.pop_double()?;
                frame.store_double(index, value)?;
            }
            ASTORE => {
                let index = frame.read_code_u8()? as u16;
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
                    IDIV => a.checked_div(b).ok_or(anyhow!("idiv by 0"))?,
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
                    LDIV => a.checked_div(b).ok_or(anyhow!("idiv by 0"))?,
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
                let index = frame.read_code_u8()? as u16;
                let constant = frame.read_code_u8()? as i32;
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
                frame.push(LocalValue::Int(if a > b {
                    1
                } else if a == b {
                    0
                } else {
                    -1
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
                let target_offset = frame.read_code_u16()? as i16;
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
                let target_offset = frame.read_code_u16()? as i16;
                frame.jump_relative(base, target_offset as i32)
            }
            JSR => {
                // This instruction is soft-depretiated since v50?
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
            }
            TABLESWITCH => {
                let base = frame.pc - 1;

                // Skip up to 3 byte padding
                frame.pc += 3;
                frame.pc &= 0xfffc;

                let index = frame.pop_int()?;
                let default = frame.read_code_u32()? as i32;
                let low = frame.read_code_u32()? as i32;
                let high = frame.read_code_u32()? as i32;

                if low > high {
                    bail!("Invalid tableswitch")
                }

                let target = if (index < low) | (index >= high) {
                    default
                } else {
                    frame.pc += (index - low) as u16 * 4;
                    frame.read_code_u32()? as i32
                };
                frame.jump_relative(base, target);
            }
            LOOKUPSWITCH => {
                let base = frame.pc - 1;

                // Skip up to 3 byte padding
                frame.pc += 3;
                frame.pc &= 0xfffc;

                let key = frame.pop_int()?;
                let default = frame.read_code_u32()? as i32;
                let npairs = frame.read_code_u32()?;

                // TODO: binary search
                let mut target = default;
                for _ in 0..npairs {
                    let compare = frame.read_code_u32()? as i32;
                    let value = frame.read_code_u32()? as i32;
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
                let index = frame.read_code_u16()?;
                let field = method.class.get().const_pool.get_field(jvm, index)?;
                get_field(&mut frame, field, &field.class.get().static_storage)?;
            }
            PUTSTATIC => {
                let index = frame.read_code_u16()?;
                let field = method.class.get().const_pool.get_field(jvm, index)?;
                put_field(&mut frame, field, &field.class.get().static_storage)?;
            }
            GETFIELD => {
                let index = frame.read_code_u16()?;
                let field = method.class.get().const_pool.get_field(jvm, index)?;
                let object = frame.pop_ref()?;
                get_field(&mut frame, field, &object.data)?;
            }
            PUTFIELD => {
                let index = frame.read_code_u16()?;
                let field = method.class.get().const_pool.get_field(jvm, index)?;
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
                let index = frame.read_code_u16()?;
                let (_, nat) = method
                    .class
                    .get()
                    .const_pool
                    .get_virtual_method(jvm, index)?;
                if nat.name.0.starts_with('<') {
                    bail!("Must not invokevirtual class or instance initialization method")
                }
                let obj = if let Some(LocalValue::Ref(obj)) = frame.stack.get(frame.stack.len() - 1)
                {
                    obj
                } else {
                    bail!("Failed invoke_special")
                };
                // TODO: check if obj.class() subclasses (or is) the referred class
                let invoke_method = obj
                    .class()
                    .methods
                    .get(&nat)
                    .ok_or_else(|| anyhow!("Method not found"))?;
                invoke(jvm, &mut frame, *invoke_method)?;
            }
            INVOKESPECIAL => {
                let index = frame.read_code_u16()?;
                let (named_class, nat) = method
                    .class
                    .get()
                    .const_pool
                    .get_virtual_method(jvm, index)?;

                let class = if (nat.name.0 != "<init>")
                    & method.class.get().is_subclass_of(named_class)
                    & method.class.get().access_flags.contains(AccessFlags::SUPER)
                {
                    method.class.get().super_class.unwrap()
                } else {
                    named_class
                };

                let invoke_method = class
                    .methods
                    .get(&nat)
                    .ok_or_else(|| anyhow!("Method not found"))?;
                invoke(jvm, &mut frame, *invoke_method)?;
            }
            INVOKESTATIC => {
                let index = frame.read_code_u16()?;
                // TODO: intern MethodRef/use Id<Method> or something
                let invoke_method = method
                    .class
                    .get()
                    .const_pool
                    .get_static_method(jvm, index)?;
                invoke(jvm, &mut frame, invoke_method)?;
            }
            NEW => {
                let index = frame.read_code_u16()?;
                let new_class = method.class.get().const_pool.get_class(index)?;
                let new_class = jvm.resolve_class(new_class)?;
                frame.push(LocalValue::Ref(jvm.create_object(new_class)))?;
            }
            GOTO_W => {
                let base = frame.pc - 1;
                let target_offset = frame.read_code_u32()? as i32;
                frame.jump_relative(base, target_offset)
            }
            JSR_W => {
                // This instruction is soft-depretiated since v50?
                let base = frame.pc - 1;
                let target = frame.read_code_u32()? as i32;
                frame.push(LocalValue::ReturnAddress(frame.pc))?;
                frame.jump_relative(base, target);
            }
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
    unsafe {
        match field.descriptor {
            Typ::Bool | Typ::Byte => frame.push(LocalValue::Int(
                storage.read_u8(field.byte_offset) as i8 as i32,
            ))?,
            Typ::Short | Typ::Char => frame.push(LocalValue::Int(
                storage.read_u16(field.byte_offset) as i16 as i32,
            ))?,
            Typ::Int => frame.push(LocalValue::Int(storage.read_u32(field.byte_offset) as i32))?,
            Typ::Float => frame.push(LocalValue::Float(f32::from_bits(
                storage.read_u32(field.byte_offset),
            )))?,
            Typ::Long => frame.push_long(storage.read_u64(field.byte_offset) as i64)?,
            Typ::Double => {
                frame.push_double(f64::from_bits(storage.read_u64(field.byte_offset)))?
            }
            Typ::Class(..) | Typ::Array { .. } => frame.push(LocalValue::Ref(
                &*(storage.read_usize(field.byte_offset) as *const ObjectData),
            ))?,
        }
    }
    Ok(())
}

fn put_field(frame: &mut Frame, field: &Field, storage: &FieldStorage) -> Result<()> {
    unsafe {
        match field.descriptor {
            Typ::Bool | Typ::Byte => {
                storage.write_u8(field.byte_offset, frame.pop_int()? as i8 as u8)
            }
            Typ::Short | Typ::Char => {
                storage.write_u16(field.byte_offset, frame.pop_int()? as i16 as u16)
            }
            Typ::Int => storage.write_u32(field.byte_offset, frame.pop_int()? as u32),
            Typ::Float => storage.write_u32(field.byte_offset, frame.pop_float()?.to_bits()),
            Typ::Long => storage.write_u64(field.byte_offset, frame.pop_long()? as u64),
            Typ::Double => storage.write_u64(field.byte_offset, frame.pop_double()?.to_bits()),
            Typ::Class(..) | Typ::Array { .. } => storage.write_usize(
                field.byte_offset,
                frame.pop_ref()? as *const ObjectData as usize,
            ),
        }
    }
    Ok(())
}

fn invoke<'a, 'b>(
    jvm: &'b JVM<'a>,
    frame: &'b mut Frame<'a>,
    method: &'a Method<'a>,
) -> Result<()> {
    let mut arg_count = 0;
    if !method.access_flags.contains(AccessFlags::STATIC) {
        arg_count = 1;
    }
    for typ in &method.nat.typ.0 {
        // Longs & doubles are double wide...
        if matches!(typ, Typ::Long | Typ::Double) {
            arg_count += 2;
        } else {
            arg_count += 1;
        }
    }
    if arg_count > frame.stack.len() {
        bail!("Invokestatic: not enough elements on stack")
    }
    let args = &frame.stack[frame.stack.len() - arg_count..];
    let result =
        run(jvm, method, args).with_context(|| format!("Trying to call {}", method.nat.name))?;
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

pub mod instructions {
    pub const NOP: u8 = 0;
    pub const ICONST_M1: u8 = 2;
    pub const ICONST_0: u8 = 3;
    pub const ICONST_1: u8 = 4;
    pub const ICONST_2: u8 = 5;
    pub const ICONST_3: u8 = 6;
    pub const ICONST_4: u8 = 7;
    pub const ICONST_5: u8 = 8;
    pub const LCONST_0: u8 = 9;
    pub const LCONST_1: u8 = 10;
    pub const FCONST_0: u8 = 11;
    pub const FCONST_1: u8 = 12;
    pub const FCONST_2: u8 = 13;
    pub const DCONST_0: u8 = 14;
    pub const DCONST_1: u8 = 15;
    pub const BIPUSH: u8 = 16;
    pub const SIPUSH: u8 = 17;
    pub const LDC: u8 = 18;
    pub const LDC_W: u8 = 19;
    pub const LDC2_W: u8 = 20;
    pub const ILOAD: u8 = 21;
    pub const LLOAD: u8 = 22;
    pub const FLOAD: u8 = 23;
    pub const DLOAD: u8 = 24;
    pub const ALOAD: u8 = 25;
    pub const ILOAD_0: u8 = 26;
    pub const ILOAD_1: u8 = 27;
    pub const ILOAD_2: u8 = 28;
    pub const ILOAD_3: u8 = 29;
    pub const LLOAD_0: u8 = 30;
    pub const LLOAD_1: u8 = 31;
    pub const LLOAD_2: u8 = 32;
    pub const LLOAD_3: u8 = 33;
    pub const FLOAD_0: u8 = 34;
    pub const FLOAD_1: u8 = 35;
    pub const FLOAD_2: u8 = 36;
    pub const FLOAD_3: u8 = 37;
    pub const DLOAD_0: u8 = 38;
    pub const DLOAD_1: u8 = 39;
    pub const DLOAD_2: u8 = 40;
    pub const DLOAD_3: u8 = 41;
    pub const ALOAD_0: u8 = 42;
    pub const ALOAD_1: u8 = 43;
    pub const ALOAD_2: u8 = 44;
    pub const ALOAD_3: u8 = 45;
    pub const ISTORE: u8 = 54;
    pub const LSTORE: u8 = 55;
    pub const FSTORE: u8 = 56;
    pub const DSTORE: u8 = 57;
    pub const ASTORE: u8 = 58;
    pub const ISTORE_0: u8 = 59;
    pub const ISTORE_1: u8 = 60;
    pub const ISTORE_2: u8 = 61;
    pub const ISTORE_3: u8 = 62;
    pub const LSTORE_0: u8 = 63;
    pub const LSTORE_1: u8 = 64;
    pub const LSTORE_2: u8 = 65;
    pub const LSTORE_3: u8 = 66;
    pub const FSTORE_0: u8 = 67;
    pub const FSTORE_1: u8 = 68;
    pub const FSTORE_2: u8 = 69;
    pub const FSTORE_3: u8 = 70;
    pub const DSTORE_0: u8 = 71;
    pub const DSTORE_1: u8 = 72;
    pub const DSTORE_2: u8 = 73;
    pub const DSTORE_3: u8 = 74;
    pub const ASTORE_0: u8 = 75;
    pub const ASTORE_1: u8 = 76;
    pub const ASTORE_2: u8 = 77;
    pub const ASTORE_3: u8 = 78;
    pub const POP: u8 = 87;
    pub const POP2: u8 = 88;
    pub const DUP: u8 = 89;
    pub const DUP_X1: u8 = 90;
    pub const DUP_X2: u8 = 91;
    pub const DUP2: u8 = 92;
    pub const DUP2_X1: u8 = 93;
    pub const DUP2_X2: u8 = 94;
    pub const SWAP: u8 = 95;
    pub const IADD: u8 = 96;
    pub const LADD: u8 = 97;
    pub const FADD: u8 = 98;
    pub const DADD: u8 = 99;
    pub const ISUB: u8 = 100;
    pub const LSUB: u8 = 101;
    pub const FSUB: u8 = 102;
    pub const DSUB: u8 = 103;
    pub const IMUL: u8 = 104;
    pub const LMUL: u8 = 105;
    pub const FMUL: u8 = 106;
    pub const DMUL: u8 = 107;
    pub const IDIV: u8 = 108;
    pub const LDIV: u8 = 109;
    pub const FDIV: u8 = 110;
    pub const DDIV: u8 = 111;
    pub const IREM: u8 = 112;
    pub const LREM: u8 = 113;
    pub const FREM: u8 = 114;
    pub const DREM: u8 = 115;
    pub const INEG: u8 = 116;
    pub const LNEG: u8 = 117;
    pub const FNEG: u8 = 118;
    pub const DNEG: u8 = 119;
    pub const ISHL: u8 = 120;
    pub const LSHL: u8 = 121;
    pub const ISHR: u8 = 122;
    pub const LSHR: u8 = 123;
    pub const IUSHR: u8 = 124;
    pub const LUSHR: u8 = 125;
    pub const IAND: u8 = 126;
    pub const LAND: u8 = 127;
    pub const IOR: u8 = 128;
    pub const LOR: u8 = 129;
    pub const IXOR: u8 = 130;
    pub const LXOR: u8 = 131;
    pub const IINC: u8 = 132;
    pub const I2L: u8 = 133;
    pub const I2F: u8 = 134;
    pub const I2D: u8 = 135;
    pub const L2I: u8 = 136;
    pub const L2F: u8 = 137;
    pub const L2D: u8 = 138;
    pub const F2I: u8 = 139;
    pub const F2L: u8 = 140;
    pub const F2D: u8 = 141;
    pub const D2I: u8 = 142;
    pub const D2L: u8 = 143;
    pub const D2F: u8 = 144;
    pub const I2B: u8 = 145;
    pub const I2C: u8 = 146;
    pub const I2S: u8 = 147;
    pub const LCMP: u8 = 148;
    pub const FCMPL: u8 = 149;
    pub const FCMPG: u8 = 150;
    pub const DCMPL: u8 = 151;
    pub const DCMPG: u8 = 152;
    pub const IFEQ: u8 = 153;
    pub const IFNE: u8 = 154;
    pub const IFLT: u8 = 155;
    pub const IFGE: u8 = 156;
    pub const IFGT: u8 = 157;
    pub const IFLE: u8 = 158;
    pub const IF_ICMPEQ: u8 = 159;
    pub const IF_ICMPNE: u8 = 160;
    pub const IF_ICMPLT: u8 = 161;
    pub const IF_ICMPGE: u8 = 162;
    pub const IF_ICMPGT: u8 = 163;
    pub const IF_ICMPLE: u8 = 164;
    pub const GOTO: u8 = 167;
    pub const JSR: u8 = 168;
    pub const RET: u8 = 169;
    pub const TABLESWITCH: u8 = 170;
    pub const LOOKUPSWITCH: u8 = 171;
    pub const IRETURN: u8 = 172;
    pub const LRETURN: u8 = 173;
    pub const FRETURN: u8 = 174;
    pub const DRETURN: u8 = 175;
    pub const ARETURN: u8 = 176;
    pub const RETURN: u8 = 177;
    pub const GETSTATIC: u8 = 178;
    pub const PUTSTATIC: u8 = 179;
    pub const GETFIELD: u8 = 180;
    pub const PUTFIELD: u8 = 181;
    pub const INVOKEVIRTUAL: u8 = 182;
    pub const INVOKESPECIAL: u8 = 183;
    pub const INVOKESTATIC: u8 = 184;
    pub const NEW: u8 = 187;
    pub const GOTO_W: u8 = 200;
    pub const JSR_W: u8 = 201;
}
