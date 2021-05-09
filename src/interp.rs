use std::{ops::Rem, rc::Rc, usize};

use crate::{instructions, Class, Code, ConstPoolItem, Field, Id, Method, MethodRef, JVM};
use anyhow::{anyhow, bail, Result};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ReturnValue {
    Void,
    Ref(/*TODO*/),
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
}

/// A value on the stack or in the local variables
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LocalValue {
    Uninitialized,
    Ref(/*TODO*/),
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

struct Frame {
    // Code has to be accessed via method and unwraped (guaranteed to be Some)
    // to prevent lifetime issues
    // Maybe put Code into a Rc instead of Method?
    method: Rc<Method>,
    locals: Vec<LocalValue>,
    stack: Vec<LocalValue>,
    pc: u16,
}

impl Frame {
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
            bail!("Failed to load local long from slot {}", index)
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
        if let LocalValue::Int(value) = self.pop()? {
            Ok(value)
        } else {
            bail!("Stack value mismatch: not an int")
        }
    }

    fn pop_float(&mut self) -> Result<f32> {
        if let LocalValue::Float(value) = self.pop()? {
            Ok(value)
        } else {
            bail!("Stack value mismatch: not an float")
        }
    }

    fn pop_long(&mut self) -> Result<i64> {
        if let (LocalValue::HighHalfOfLong(high), LocalValue::LowHalfOfLong(low)) =
            (self.pop()?, self.pop()?)
        {
            Ok((((high as u64) << 32) + low as u64) as i64)
        } else {
            bail!("Failed to pop long")
        }
    }

    fn pop_double(&mut self) -> Result<f64> {
        if let (LocalValue::HighHalfOfDouble(high), LocalValue::LowHalfOfDouble(low)) =
            (self.pop()?, self.pop()?)
        {
            Ok(f64::from_bits(((high as u64) << 32) + low as u64))
        } else {
            bail!("Failed to pop long")
        }
    }

    fn push(&mut self, value: LocalValue) -> Result<()> {
        if self.stack.len() < self.stack.capacity() {
            self.stack.push(value);
            Ok(())
        } else {
            bail!("Max stack exceeded")
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

pub fn run(jvm: &mut JVM, method: MethodRef, args: &[LocalValue]) -> Result<ReturnValue> {
    let class_id = jvm.resolve_class(method.class)?;
    let method = jvm
        .methods
        .get(&method)
        .ok_or(anyhow!("Method not found"))?;

    if method.code.is_none() {
        todo!("Methods without code attribute");
    }

    let mut frame = Frame {
        method: method.clone(),
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
        println!("pc: {}", frame.pc);
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
            ILOAD => {
                let index = frame.read_code_u8()? as u16;
                frame.push(frame.load(index)?)?;
            }
            LLOAD => {
                let index = frame.read_code_u8()? as u16;
                frame.push_long(frame.load_long(index)?)?;
            }
            FLOAD => {
                let index = frame.read_code_u8()? as u16;
                frame.push(frame.load(index)?)?;
            }
            DLOAD => {
                let index = frame.read_code_u8()? as u16;
                frame.push_double(frame.load_double(index)?)?;
            }
            ILOAD_0 => frame.push(frame.load(0)?)?,
            ILOAD_1 => frame.push(frame.load(1)?)?,
            ILOAD_2 => frame.push(frame.load(2)?)?,
            ILOAD_3 => frame.push(frame.load(3)?)?,
            LLOAD_0 => frame.push_long(frame.load_long(0)?)?,
            LLOAD_1 => frame.push_long(frame.load_long(1)?)?,
            LLOAD_2 => frame.push_long(frame.load_long(2)?)?,
            LLOAD_3 => frame.push_long(frame.load_long(3)?)?,
            FLOAD_0 => frame.push(frame.load(0)?)?,
            FLOAD_1 => frame.push(frame.load(1)?)?,
            FLOAD_2 => frame.push(frame.load(2)?)?,
            FLOAD_3 => frame.push(frame.load(3)?)?,
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
            RETURN => return Ok(ReturnValue::Void),
            INVOKESTATIC => {
                let index = frame.read_code_u16()?;
                let method = jvm.classes.get(class_id).const_pool.get_method(index)?;
                // TODO: intern MethodRef/use Id<Method> or something
                let method = method.clone();

                let arg_count = method.typ.0.len();
                if arg_count > frame.stack.len() {
                    bail!("Invokestatic: not enough elements on stack")
                }
                let args = &frame.stack[frame.stack.len() - arg_count..];
                match run(jvm, method, args)? {
                    ReturnValue::Void => {}
                    ReturnValue::Ref() => todo!(),
                    ReturnValue::Int(value) => frame.push(LocalValue::Int(value))?,
                    ReturnValue::Float(value) => frame.push(LocalValue::Float(value))?,
                    ReturnValue::Long(value) => frame.push_long(value)?,
                    ReturnValue::Double(value) => frame.push_double(value)?,
                }
            }
            GOTO_W => {
                let base = frame.pc - 1;
                let target_offset = frame.read_code_u32()? as i32;
                frame.jump_relative(base, target_offset)
            }
            JSR_W => {
                let base = frame.pc - 1;
                let target = frame.read_code_u32()? as i32;
                frame.push(LocalValue::ReturnAddress(frame.pc))?;
                frame.jump_relative(base, target);
            }
            other => todo!("Bytecode: {}", other),
        }
    }
}
