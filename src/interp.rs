use std::rc::Rc;

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
    HighHalfOfLong(u32),
    LowHalfOfLong(u32),
    Float(f32),
    HighHalfOfDouble(u32),
    LowHalfOfDouble(u32),
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
    pc: usize,
}

impl Frame {
    fn get_local(&self, index: u16) -> Result<LocalValue> {
        self.locals
            .get(index as usize)
            .copied()
            .ok_or(anyhow!("Invalid local index"))
    }

    // I would have made get generic but type interference doesn't like its uses
    fn get_i32(&self, index: u16) -> Result<i32> {
        self.get_local(index)?.into()
    }

    fn set_local(&mut self, index: u16, value: LocalValue) -> Result<()> {
        *self
            .locals
            .get_mut(index as usize)
            .ok_or(anyhow!("Invalid locals index"))? = value;
        Ok(())
    }

    fn pop(&mut self) -> Result<LocalValue> {
        self.stack.pop().ok_or(anyhow!("Pop empty stack"))
    }

    fn pop_i32(&mut self) -> Result<i32> {
        if let LocalValue::Int(value) = self.pop()? {
            Ok(value)
        } else {
            bail!("Stack value mismatch: not an int")
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
            .get(self.pc - 1)
            .copied()
            .ok_or(anyhow!("PC out of bounds"))
    }

    fn read_previous_byte(&mut self) -> u8 {
        self.method.code.as_ref().unwrap().bytes[self.pc - 1]
    }

    fn read_code_u16(&mut self) -> Result<u16> {
        Ok(((self.read_code_u8()? as u16) << 8) + self.read_code_u8()? as u16)
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
        match frame.read_code_u8()? {
            NOP => {}
            ICONST_M1 => frame.push(LocalValue::Int(-1))?,
            ICONST_0 => frame.push(LocalValue::Int(0))?,
            ICONST_1 => frame.push(LocalValue::Int(1))?,
            ICONST_2 => frame.push(LocalValue::Int(2))?,
            ICONST_3 => frame.push(LocalValue::Int(3))?,
            ICONST_4 => frame.push(LocalValue::Int(4))?,
            ICONST_5 => frame.push(LocalValue::Int(5))?,
            BIPUSH => {
                let value = frame.read_code_u8()? as i8;
                frame.push(LocalValue::Int(value as i32))?;
            }
            ILOAD => {
                let index = frame.read_code_u8()? as u16;
                frame.push(frame.get_local(index)?)?;
            }
            ILOAD_0 => frame.push(frame.get_local(0)?)?,
            ILOAD_1 => frame.push(frame.get_local(1)?)?,
            ILOAD_2 => frame.push(frame.get_local(2)?)?,
            ILOAD_3 => frame.push(frame.get_local(3)?)?,
            ISTORE => {
                let index = frame.read_code_u8()? as u16;
                let value = frame.pop_i32()?;
                frame.set_local(index, LocalValue::Int(value))?;
            }
            ISTORE_0 => {
                let value = frame.pop_i32()?;
                frame.set_local(0, LocalValue::Int(value))?;
            }
            ISTORE_1 => {
                let value = frame.pop_i32()?;
                frame.set_local(1, LocalValue::Int(value))?;
            }
            ISTORE_2 => {
                let value = frame.pop_i32()?;
                frame.set_local(2, LocalValue::Int(value))?;
            }
            ISTORE_3 => {
                let value = frame.pop_i32()?;
                frame.set_local(3, LocalValue::Int(value))?;
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
            IADD => {
                let result = frame.pop_i32()? + frame.pop_i32()?;
                frame.push(LocalValue::Int(result))?;
            }
            ISUB => {
                let result = frame.pop_i32()? - frame.pop_i32()?;
                frame.push(LocalValue::Int(result))?;
            }
            IMUL => {
                let result = frame.pop_i32()? * frame.pop_i32()?;
                frame.push(LocalValue::Int(result))?;
            }
            IDIV => {
                let result = frame.pop_i32()?.checked_div(frame.pop_i32()?);
                frame.push(LocalValue::Int(result.ok_or(anyhow!("Div by 0"))?))?;
            }
            IREM => {
                let result = frame.pop_i32()?.checked_rem(frame.pop_i32()?);
                frame.push(LocalValue::Int(result.ok_or(anyhow!("Rem by 0"))?))?;
            }
            INEG => {
                let result = -frame.pop_i32()?;
                frame.push(LocalValue::Int(result))?;
            }
            ISHL => {
                let result = frame.pop_i32()? << (frame.pop_i32()? % 0x1f);
                frame.push(LocalValue::Int(result))?;
            }
            ISHR => {
                let result = frame.pop_i32()? >> (frame.pop_i32()? % 0x1f);
                frame.push(LocalValue::Int(result))?;
            }
            IUSHR => {
                let result = (frame.pop_i32()? as u32) >> (frame.pop_i32()? % 0x1f);
                frame.push(LocalValue::Int(result as i32))?;
            }
            IAND => {
                let result = frame.pop_i32()? & (frame.pop_i32()? % 0x1f);
                frame.push(LocalValue::Int(result))?;
            }
            IOR => {
                let result = frame.pop_i32()? | (frame.pop_i32()? % 0x1f);
                frame.push(LocalValue::Int(result))?;
            }
            IXOR => {
                let result = frame.pop_i32()? ^ (frame.pop_i32()? % 0x1f);
                frame.push(LocalValue::Int(result))?;
            }
            IINC => {
                let index = frame.read_code_u8()? as u16;
                let constant = frame.read_code_u8()? as i32;
                frame.set_local(index, LocalValue::Int(frame.get_i32(index)? + constant))?;
            }
            IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE | IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT
            | IF_ICMPGE | IF_ICMPGT | IF_ICMPLE => {
                let (op, two_operand) = {
                    let instr = frame.read_previous_byte();
                    if instr > IFLE {
                        (instr - 6, true)
                    } else {
                        (instr, false)
                    }
                };
                let a = frame.pop_i32()?;
                let b = if two_operand { frame.pop_i32()? } else { 0 };
                let branch_target = frame.read_code_u16()?;
                if match op {
                    IFEQ => a == b,
                    IFNE => a != b,
                    IFLT => a < b,
                    IFGE => a >= b,
                    IFGT => a > b,
                    IFLE => a <= b,
                    _ => unreachable!(),
                } {
                    frame.pc = (frame.pc as isize + branch_target as isize) as usize;
                }
            }
            IRETURN => return Ok(ReturnValue::Int(frame.pop_i32()?)),
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
            other => todo!("Bytecode: {}", other),
        }
    }
}
