use crate::{instructions, Code, ConstPoolItem, Field, Method, Value, JVM};
use anyhow::{anyhow, bail, Result};

struct Locals(Vec<Value>);

impl Locals {
    fn get(&self, index: u16) -> Result<Value> {
        self.0
            .get(index as usize)
            .copied()
            .ok_or(anyhow!("Invalid local index"))
    }

    // I would have made get generic but type interference doesn't like its uses
    fn get_i32(&self, index: u16) -> Result<i32> {
        self.get(index)?.into()
    }

    fn set(&mut self, index: u16, value: Value) -> Result<()> {
        *self
            .0
            .get_mut(index as usize)
            .ok_or(anyhow!("Invalid locals index"))? = value;
        Ok(())
    }
}

pub fn run(jvm: &JVM, method: &Method, args: &[Value]) -> Result<Option<Value>> {
    let code = method
        .code
        .as_ref()
        .expect("TODO: Methods without code attribute");

    // TODO: check args descriptor match
    let mut locals = Locals({
        let mut locals = Vec::with_capacity(code.max_locals as usize);
        locals.extend_from_slice(args);
        while locals.len() < code.max_locals as usize {
            locals.push(Value::Uninitialized);
        }
        locals
    });

    let mut stack = Vec::with_capacity(code.max_stack as usize);

    let mut pc = 0;

    fn pop<T>(stack: &mut Vec<Value>) -> Result<T>
    where
        Result<T>: From<Value>,
    {
        stack.pop().ok_or(anyhow!("Pop empty stack"))?.into()
    }

    // Todo: bytecode verification
    // (incl running of the end)
    loop {
        use instructions::*;
        match code.bytes[pc] {
            ICONST_M1 => stack.push(Value::Int(-1)),
            ICONST_0 => stack.push(Value::Int(0)),
            ICONST_1 => stack.push(Value::Int(1)),
            ICONST_2 => stack.push(Value::Int(2)),
            ICONST_3 => stack.push(Value::Int(3)),
            ICONST_4 => stack.push(Value::Int(4)),
            ICONST_5 => stack.push(Value::Int(5)),
            ILOAD => {
                pc += 1;
                let index = code.bytes[pc] as u16;
                stack.push(locals.get(index)?)
            }
            ILOAD_0 => stack.push(locals.get(0)?),
            ILOAD_1 => stack.push(locals.get(1)?),
            ILOAD_2 => stack.push(locals.get(2)?),
            ILOAD_3 => stack.push(locals.get(3)?),
            ISTORE => {
                pc += 1;
                let index = code.bytes[pc] as u16;
                locals.set(index, Value::Int(pop(&mut stack)?))?;
            }
            ISTORE_0 => locals.set(0, Value::Int(pop(&mut stack)?))?,
            ISTORE_1 => locals.set(0, Value::Int(pop(&mut stack)?))?,
            ISTORE_2 => locals.set(0, Value::Int(pop(&mut stack)?))?,
            ISTORE_3 => locals.set(0, Value::Int(pop(&mut stack)?))?,
            DUP => stack.push(*stack.last().ok_or(anyhow!("Empty stack"))?),
            DUP_X1 => {
                let first = stack.pop().ok_or(anyhow!("Empty stack"))?;
                let second = stack.pop().ok_or(anyhow!("Empty stack"))?;
                stack.push(first);
                stack.push(second);
                stack.push(first);
            }
            DUP_X2 => {
                let first = stack.pop().ok_or(anyhow!("Empty stack"))?;
                let second = stack.pop().ok_or(anyhow!("Empty stack"))?;
                let third = stack.pop().ok_or(anyhow!("Empty stack"))?;
                stack.push(first);
                stack.push(third);
                stack.push(second);
                stack.push(first);
            }
            DUP2 => {
                let first = stack.pop().ok_or(anyhow!("Empty stack"))?;
                let second = stack.pop().ok_or(anyhow!("Empty stack"))?;
                stack.push(second);
                stack.push(first);
                stack.push(second);
                stack.push(first);
            }
            DUP2_X1 => {
                let first = stack.pop().ok_or(anyhow!("Empty stack"))?;
                let second = stack.pop().ok_or(anyhow!("Empty stack"))?;
                let third = stack.pop().ok_or(anyhow!("Empty stack"))?;
                stack.push(second);
                stack.push(first);
                stack.push(third);
                stack.push(second);
                stack.push(first);
            }
            DUP2_X2 => {
                let first = stack.pop().ok_or(anyhow!("Empty stack"))?;
                let second = stack.pop().ok_or(anyhow!("Empty stack"))?;
                let third = stack.pop().ok_or(anyhow!("Empty stack"))?;
                let fourth = stack.pop().ok_or(anyhow!("Empty stack"))?;
                stack.push(second);
                stack.push(first);
                stack.push(fourth);
                stack.push(third);
                stack.push(second);
                stack.push(first);
            }
            IADD => {
                let result = pop(&mut stack)? + pop(&mut stack)?;
                stack.push(Value::Int(result));
            }
            ISUB => {
                let result = pop(&mut stack)? - pop(&mut stack)?;
                stack.push(Value::Int(result));
            }
            IMUL => {
                let result = pop(&mut stack)? * pop(&mut stack)?;
                stack.push(Value::Int(result));
            }
            IDIV => {
                let result = pop::<i32>(&mut stack)?.checked_div(pop(&mut stack)?);
                stack.push(Value::Int(result.ok_or(anyhow!("Div by 0"))?));
            }
            IREM => {
                let result = pop::<i32>(&mut stack)?.checked_rem(pop(&mut stack)?);
                stack.push(Value::Int(result.ok_or(anyhow!("Rem by 0"))?));
            }
            INEG => {
                let result = -pop(&mut stack)?;
                stack.push(Value::Int(result));
            }
            ISHL => {
                let result = pop(&mut stack)? << (pop(&mut stack)? % 0x1f);
                stack.push(Value::Int(result));
            }
            ISHR => {
                let result = pop(&mut stack)? >> (pop(&mut stack)? % 0x1f);
                stack.push(Value::Int(result));
            }
            IUSHR => {
                let result = (pop(&mut stack)? as u32) >> (pop(&mut stack)? % 0x1f);
                stack.push(Value::Int(result as i32));
            }
            IAND => {
                let result = pop(&mut stack)? & pop(&mut stack)?;
                stack.push(Value::Int(result));
            }
            IOR => {
                let result = pop(&mut stack)? | pop(&mut stack)?;
                stack.push(Value::Int(result));
            }
            IXOR => {
                let result = pop(&mut stack)? ^ pop(&mut stack)?;
                stack.push(Value::Int(result));
            }
            IINC => {
                pc += 1;
                let index = code.bytes[pc] as u16;
                pc += 1;
                let constant = code.bytes[pc] as i32;
                locals.set(index, Value::Int(locals.get_i32(index)? + constant))?;
            }
            IRETURN => return Ok(Some(stack.pop().unwrap())),
            _ => todo!("bytecode verification"),
        }
        pc += 1;
    }
}
