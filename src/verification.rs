use anyhow::{anyhow, bail, Context, Result};

use crate::{instructions::*, parse, AccessFlags, Class, Method, RefType, Typ, JVM};

#[derive(Clone, Default)]
pub(crate) struct StackMapFrame<'a> {
    pub locals: Vec<VerificationType<'a>>,
    pub stack: Vec<VerificationType<'a>>,
}

impl<'a> StackMapFrame<'a> {
    fn is_assignable_to(&self, other: &Self, jvm: &JVM<'a>) -> bool {
        (self.locals.len() == other.locals.len())
            & (self.stack.len() == other.stack.len())
            & self
                .locals
                .iter()
                .zip(other.locals.iter())
                .all(|(s, o)| s.is_assignable_to(*o, jvm))
            & self
                .stack
                .iter()
                .zip(other.stack.iter())
                .all(|(s, o)| s.is_assignable_to(*o, jvm))
    }
}

// Note: Two ObjectVariables with different indexes may still refer to same class
#[derive(Copy, Clone, PartialEq, Eq)]
pub(crate) enum VerificationType<'a> {
    Top,
    Integer,
    Float,
    Null,
    ObjectVariable(&'a RefType<'a>),
    UninitializedThis,
    UninitializedVariable { offset: u16 },
    Long,
    Double,
}

impl<'a> VerificationType<'a> {
    fn is_assignable_to(self, other: Self, jvm: &JVM<'a>) -> bool {
        if (self == other) | (other == Self::Top) {
            true
        } else if let Self::ObjectVariable(other_class) = other {
            if let Self::Null = self {
                true
            } else if let Self::ObjectVariable(self_class) = self {
                self_class.assignable_to(other_class)
            } else {
                false
            }
        } else {
            false
        }
    }
}

pub(crate) fn push_type<'a, 'b>(
    jvm: &'b JVM<'a>,
    types: &'b mut Vec<VerificationType<'a>>,
    typ: Typ<'a>,
) -> Result<()> {
    match typ {
        Typ::Bool | Typ::Char | Typ::Byte | Typ::Short | Typ::Int => {
            types.push(VerificationType::Integer)
        }
        Typ::Float => types.push(VerificationType::Float),
        Typ::Double => {
            types.push(VerificationType::Double);
            types.push(VerificationType::Top);
        }
        Typ::Long => {
            types.push(VerificationType::Long);
            types.push(VerificationType::Top);
        }
        Typ::Ref(class) => types.push(VerificationType::ObjectVariable(jvm.resolve_class(class)?)),
    }
    Ok(())
}

// TODO: use this to construct a more efficient bytecode format
/// Verification by type checking
/// (Verification by type inference is not planned)
pub(crate) fn verify<'a: 'b, 'b>(jvm: &'b JVM<'a>, class: &'b Class<'a>) -> Result<()> {
    if let Some(RefType::Class(super_class)) = class.super_class {
        verify(jvm, super_class)?;
        // Check whether the super class is final is redundant (has to already be checked during loading)
    } else if class.name.0 != "java/lang/Object" {
        bail!("Class has no superclass")
    }

    for method in class.methods.values() {
        if method.class.get() == class {
            verify_method(jvm, method).with_context(|| format!("in method {}", method.nat))?;
        }
    }

    Ok(())
}

// TODO: do this lazily
fn verify_method<'a, 'b>(jvm: &'b JVM<'a>, method: &'b Method<'a>) -> Result<()> {
    if let (false, Some(super_class)) = (
        method.access_flags.contains(AccessFlags::STATIC),
        method.class.get().super_class,
    ) {
        if let Some(super_method) = super_class.method(method.nat.name, method.nat.typ) {
            if super_method.access_flags.contains(AccessFlags::FINAL) {
                bail!("Tried to overwrite final method {}", method.nat);
            }
        }
    }

    if method.code.is_some() {
        verify_bytecode(jvm, method)?
    } else if !method
        .access_flags
        .intersects(AccessFlags::NATIVE | AccessFlags::ABSTRACT)
    {
        bail!("Missing code attribute")
    }

    Ok(())
}

fn verify_bytecode<'a, 'b>(jvm: &'b JVM<'a>, method: &'b Method<'a>) -> Result<()> {
    let initial_locals = {
        let mut locals = Vec::new();
        if !method.access_flags.contains(AccessFlags::STATIC) {
            locals.push(
                if (method.nat.name.0 != "<init>")
                    || (method.class.get().name.0 == "java/lang/Object")
                {
                    VerificationType::ObjectVariable(todo!() /*method.class.get()*/)
                } else {
                    VerificationType::UninitializedThis
                },
            );
        }
        for arg in &method.nat.typ.0 {
            push_type(jvm, &mut locals, *arg)?
        }
        locals.into()
    };

    let code = method.code.as_ref().unwrap();
    let stack_map_table = parse::read_stack_map_table(
        &code.stack_map_table.bytes,
        &method.class.get().const_pool,
        initial_locals,
        code.stack_map_table.max_stack,
        code.stack_map_table.max_locals,
    )?;
    let mut type_state = stack_map_table[&0].clone();
    let mut type_states = stack_map_table.iter();
    let next_explicit_type_state = type_states.next();
    let mut connected_to_previous_block = true;
    let mut pc = 0;

    let bytes = &code.bytes;
    let pc = &mut pc;

    // TODO: check that jumps target instruction boundaries

    while (*pc as usize) < code.bytes.len() {
        if type_state.stack.len() > code.max_stack as usize {
            bail!("Max stack exceeded")
        }
        if let Some((type_state_pc, explicit_type_state)) = next_explicit_type_state {
            if *pc as u32 == *type_state_pc {
                if !connected_to_previous_block
                    | type_state.is_assignable_to(explicit_type_state, jvm)
                {
                    type_state = explicit_type_state.clone();
                } else {
                    bail!("Typestates not assignable")
                }
            }
        }
        connected_to_previous_block = true;

        match read_code_u8(bytes, pc)? {
            ILOAD_0 => type_state.stack.push({
                *type_state
                    .locals
                    .get(0)
                    .ok_or_else(|| anyhow!("Index out of bounds"))?
            }),
            ILOAD_1 => type_state.stack.push({
                *type_state
                    .locals
                    .get(1)
                    .ok_or_else(|| anyhow!("Index out of bounds"))?
            }),
            IADD => {
                if let (Some(VerificationType::Integer), Some(VerificationType::Integer)) =
                    (type_state.stack.pop(), type_state.stack.pop())
                {
                    type_state.stack.push(VerificationType::Integer)
                } else {
                    bail!("failed IADD")
                }
            }
            IRETURN => {
                if !matches!(
                    (type_state.stack.pop(), method.nat.typ.1),
                    (
                        Some(VerificationType::Integer),
                        Some(Typ::Bool | Typ::Byte | Typ::Char | Typ::Short | Typ::Int)
                    )
                ) {
                    bail!("failed IRETURN")
                }
                connected_to_previous_block = false
            }
            RETURN => {
                if !method.nat.typ.1.is_none() {
                    bail!("failed RETURN")
                }
                connected_to_previous_block = false
            }
            GETSTATIC => {
                let index = read_code_u16(bytes, pc)?;
                push_type(
                    jvm,
                    &mut type_state.stack,
                    method.class.get().const_pool.get_field(index)?.descriptor,
                );
            }
            PUTSTATIC => {
                let index = read_code_u16(bytes, pc)?;
                let typ = method.class.get().const_pool.get_field(index)?.descriptor;
            }
            _ => todo!(),
        }
    }
    Ok(())
}

fn pop_type(stack: &mut Vec<VerificationType>, typ: Typ) -> Result<()> {
    if match (typ, stack.pop()) {
        (
            Typ::Bool | Typ::Byte | Typ::Short | Typ::Char | Typ::Int,
            Some(VerificationType::Integer),
        ) => true,
        (Typ::Long, Some(VerificationType::Top)) => {
            matches!(stack.pop(), Some(VerificationType::Long))
        }
        (Typ::Float, Some(VerificationType::Float)) => true,
        (Typ::Double, Some(VerificationType::Top)) => {
            matches!(stack.pop(), Some(VerificationType::Double))
        }
        (Typ::Ref(ref_type), Some(VerificationType::ObjectVariable(obj))) => {
            let ref_type = todo!();
        }
        _ => false,
    } {
        Ok(())
    } else {
        Err(anyhow!("Failed to pop {:?}", typ))
    }
}

fn read_code_u8(code: &[u8], pc: &mut u16) -> Result<u8> {
    *pc += 1;
    code.get(*pc as usize - 1)
        .copied()
        .ok_or(anyhow!("Incomplete Instruction"))
}

fn read_code_u16(code: &[u8], pc: &mut u16) -> Result<u16> {
    Ok(((read_code_u8(code, pc)? as u16) << 8) + read_code_u8(code, pc)? as u16)
}

fn read_code_u32(code: &[u8], pc: &mut u16) -> Result<u32> {
    Ok(((read_code_u16(code, pc)? as u32) << 16) + read_code_u16(code, pc)? as u32)
}
