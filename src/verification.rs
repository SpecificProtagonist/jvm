use std::{collections::BTreeMap, sync::atomic::Ordering};

use crate::{
    const_pool::ConstPoolItem, exception, instructions::*, object::Object, parse, AccessFlags,
    Class, JVMResult, Method, Typ, JVM,
};

fn ve<'a>(jvm: &JVM<'a>, _message: &str) -> Object<'a> {
    exception(jvm, "VerifyError")
}

#[derive(Clone, Default, Debug)]
pub(crate) struct StackMapFrame<'a> {
    pub locals: Vec<VerificationType<'a>>,
    pub stack: Vec<VerificationType<'a>>,
}

impl<'a> StackMapFrame<'a> {
    fn is_assignable_to(&self, other: &Self) -> bool {
        (self.locals.len() == other.locals.len())
            & (self.stack.len() == other.stack.len())
            & self
                .locals
                .iter()
                .zip(other.locals.iter())
                .all(|(s, o)| s.is_assignable_to(*o))
            & self
                .stack
                .iter()
                .zip(other.stack.iter())
                .all(|(s, o)| s.is_assignable_to(*o))
    }

    /// In case of category 2 type, pops both the top type and the actual type
    fn pop(&mut self, jvm: &JVM<'a>) -> JVMResult<'a, VerificationType<'a>> {
        let typ = self
            .stack
            .pop()
            .ok_or_else(|| ve(jvm, "popped empty stack"))?;
        if typ == VerificationType::Top {
            self.stack
                .pop()
                .ok_or_else(|| ve(jvm, "popped empty stack"))
        } else if typ.category_2() {
            Err(ve(jvm, "stack missing top"))
        } else {
            Ok(typ)
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub(crate) enum VerificationType<'a> {
    Top,
    Integer,
    Float,
    Null,
    ObjectVariable(&'a Class<'a>),
    UninitializedThis,
    UninitializedVariable { offset: u16 },
    Long,
    Double,
}

impl<'a> Default for VerificationType<'a> {
    fn default() -> Self {
        Self::Top
    }
}

impl<'a> VerificationType<'a> {
    fn is_assignable_to(self, other: Self) -> bool {
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

    pub fn category_2(self) -> bool {
        matches!(self, VerificationType::Long | VerificationType::Double)
    }
}

pub(crate) fn push_type<'a, 'b>(
    jvm: &'b JVM<'a>,
    types: &'b mut Vec<VerificationType<'a>>,
    typ: Typ<'a>,
) -> JVMResult<'a, ()> {
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

// TODO: use this to construct a more efficient bytecode format?
// TODO: if this throws a linkage error, it must always throw the same instance for the same class
/// Verification by type checking
/// (Verification by type inference is not planned)
pub(crate) fn verify<'a: 'b, 'b>(jvm: &'b JVM<'a>, class: &'b Class<'a>) -> JVMResult<'a, ()> {
    if let Some(super_class) = class.super_class {
        verify(jvm, super_class)?;
        // Check whether the super class is final (as described in spec) is redundant (has to already be checked during loading)
    } else if class.name.0 != "java/lang/Object" {
        return Err(ve(jvm, "class has no superclass"));
    }

    for method in class.methods.values() {
        if method.class.load() == class {
            verify_method(jvm, method)?;
        }
    }

    Ok(())
}

// TODO: do this lazily
fn verify_method<'a, 'b>(jvm: &'b JVM<'a>, method: &'a Method<'a>) -> JVMResult<'a, ()> {
    if let (false, Some(super_class)) = (
        method.access_flags.contains(AccessFlags::STATIC),
        method.class.load().super_class,
    ) {
        if let Some(super_method) = super_class.methods.get(&method.nat) {
            if super_method.access_flags.contains(AccessFlags::FINAL) {
                return Err(ve(jvm, "tried to overwrite final method"));
            }
        }
    }

    if method.code.is_some() {
        if !jvm
            .verification_type_checking_disabled
            .load(Ordering::SeqCst)
        {
            verify_bytecode(jvm, method)?
        }
    } else if !method
        .access_flags
        .intersects(AccessFlags::NATIVE | AccessFlags::ABSTRACT)
    {
        return Err(ve(jvm, "missing code attribute"));
    }

    Ok(())
}

fn verify_bytecode<'a, 'b>(jvm: &'b JVM<'a>, method: &'a Method<'a>) -> JVMResult<'a, ()> {
    println!(
        "verifying {}.{} ({})",
        method.class().name,
        method.nat.name,
        jvm.verification_type_checking_disabled
            .load(Ordering::SeqCst)
    );
    ///// for testing, TODO: remove this! /////
    if method.nat.name.0 == "<init>" {
        return Ok(());
    }
    ///////////////////////////////////////////

    let code = method.code.as_ref().unwrap();
    let const_pool = method.class.load().const_pool.read();

    // The initial typestate is not stored in the class file but derive from the method signature
    let initial_locals = {
        let mut locals = Vec::new();
        if !method.access_flags.contains(AccessFlags::STATIC) {
            locals.push(
                if (method.nat.name.0 != "<init>")
                    || (method.class.load().name.0 == "java/lang/Object")
                {
                    VerificationType::ObjectVariable(method.class.load())
                } else {
                    VerificationType::UninitializedThis
                },
            );
        }
        for arg in &method.nat.typ.0 {
            push_type(jvm, &mut locals, *arg)?
        }
        if locals.len() > code.max_locals as usize {
            return Err(ve(jvm, "max locals too low"));
        }
        locals
    };

    let bytes = &code.bytes;
    let mut stack_map_table = parse::read_stack_map_table(
        jvm,
        code.stack_map_table.as_deref(),
        &const_pool,
        initial_locals,
    )?;

    // Verify max locals & stack length
    for frame in stack_map_table.values_mut() {
        if (frame.locals.len() > code.max_locals as usize)
            | (frame.stack.len() > code.max_stack as usize)
        {
            return Err(ve(jvm, "invalid stack map table"));
        }
        frame
            .locals
            .resize(code.max_locals as usize, VerificationType::Top);
    }

    if let Some((last_pc, _)) = stack_map_table.last_key_value() {
        if *last_pc as usize > bytes.len() {
            return Err(ve(jvm, "stack map frame out of bounds"));
        }
    }

    let mut type_states = stack_map_table.iter();
    let mut type_state = type_states.next().unwrap().1.clone();
    let mut next_explicit_type_state = type_states.next();
    // Whether the type state depends on the type state of the previous instruction (false after goto & returns)
    let mut connected_to_previous_block = true;
    let mut pc = 0;

    let pc = &mut pc;

    let any_object = VerificationType::ObjectVariable(jvm.resolve_class("java/lang/Object")?);

    // Iterate over all instructions
    // TODO: switch result to bool
    while (*pc as usize) < code.bytes.len() {
        match read_code_u8(jvm, bytes, pc)? {
            NOP => {}
            ICONST_0 | ICONST_1 | ICONST_2 | ICONST_3 | ICONST_4 | ICONST_5 | ICONST_M1 => {
                type_state.stack.push(VerificationType::Integer)
            }
            LCONST_0 | LCONST_1 => {
                type_state.stack.push(VerificationType::Long);
                type_state.stack.push(VerificationType::Top);
            }
            FCONST_0 | FCONST_1 | FCONST_2 => type_state.stack.push(VerificationType::Float),
            DCONST_0 | DCONST_1 => {
                type_state.stack.push(VerificationType::Double);
                type_state.stack.push(VerificationType::Top);
            }
            BIPUSH => {
                *pc += 1;
                type_state.stack.push(VerificationType::Integer)
            }
            SIPUSH => {
                *pc += 2;
                type_state.stack.push(VerificationType::Integer)
            }
            LDC => {
                let index = read_code_u8(jvm, bytes, pc)? as u16;
                match const_pool.items.get(index as usize) {
                    Some(ConstPoolItem::Integer(_)) => {
                        type_state.stack.push(VerificationType::Integer)
                    }
                    Some(ConstPoolItem::Float(_)) => type_state.stack.push(VerificationType::Float),
                    Some(ConstPoolItem::RawString(_)) => type_state.stack.push(
                        VerificationType::ObjectVariable(jvm.resolve_class("java/lang/String")?),
                    ),
                    Some(ConstPoolItem::Class(class)) => type_state
                        .stack
                        .push(VerificationType::ObjectVariable(class)),
                    _ => return Err(ve(jvm, "invalid ldc")),
                }
            }
            LDC_W => {
                let index = read_code_u16(jvm, bytes, pc)?;
                match const_pool.items.get(index as usize) {
                    Some(ConstPoolItem::Integer(_)) => {
                        type_state.stack.push(VerificationType::Integer)
                    }
                    Some(ConstPoolItem::Float(_)) => type_state.stack.push(VerificationType::Float),
                    Some(ConstPoolItem::RawString(_)) => type_state.stack.push(
                        VerificationType::ObjectVariable(jvm.resolve_class("java/lang/String")?),
                    ),
                    Some(ConstPoolItem::Class(class)) => type_state
                        .stack
                        .push(VerificationType::ObjectVariable(class)),
                    _ => return Err(ve(jvm, "invalid ldc_w")),
                }
            }
            LDC2_W => {
                let index = read_code_u16(jvm, bytes, pc)?;
                match const_pool.items.get(index as usize) {
                    Some(ConstPoolItem::Double(_)) => {
                        type_state.stack.push(VerificationType::Double);
                        type_state.stack.push(VerificationType::Top)
                    }
                    Some(ConstPoolItem::Long(_)) => {
                        type_state.stack.push(VerificationType::Long);
                        type_state.stack.push(VerificationType::Top)
                    }
                    _ => return Err(ve(jvm, "invalid ldc2_w")),
                }
            }
            ILOAD => {
                let index = read_code_u8(jvm, bytes, pc)? as u16;
                load(jvm, &mut type_state, index, VerificationType::Integer)?
            }
            LLOAD => {
                let index = read_code_u8(jvm, bytes, pc)? as u16;
                load(jvm, &mut type_state, index, VerificationType::Long)?;
                type_state.stack.push(VerificationType::Top);
            }
            FLOAD => {
                let index = read_code_u8(jvm, bytes, pc)? as u16;
                load(jvm, &mut type_state, index, VerificationType::Float)?
            }
            DLOAD => {
                let index = read_code_u8(jvm, bytes, pc)? as u16;
                load(jvm, &mut type_state, index, VerificationType::Double)?;
                type_state.stack.push(VerificationType::Top);
            }
            ALOAD => {
                let index = read_code_u8(jvm, bytes, pc)? as u16;
                load(jvm, &mut type_state, index, any_object)?
            }
            ILOAD_0 => load(jvm, &mut type_state, 0, VerificationType::Integer)?,
            ILOAD_1 => load(jvm, &mut type_state, 1, VerificationType::Integer)?,
            ILOAD_2 => load(jvm, &mut type_state, 2, VerificationType::Integer)?,
            ILOAD_3 => load(jvm, &mut type_state, 3, VerificationType::Integer)?,
            LLOAD_0 => {
                load(jvm, &mut type_state, 0, VerificationType::Long)?;
                type_state.stack.push(VerificationType::Top);
            }
            LLOAD_1 => {
                load(jvm, &mut type_state, 1, VerificationType::Long)?;
                type_state.stack.push(VerificationType::Top);
            }
            LLOAD_2 => {
                load(jvm, &mut type_state, 2, VerificationType::Long)?;
                type_state.stack.push(VerificationType::Top);
            }
            LLOAD_3 => {
                load(jvm, &mut type_state, 3, VerificationType::Long)?;
                type_state.stack.push(VerificationType::Top);
            }
            FLOAD_0 => load(jvm, &mut type_state, 0, VerificationType::Float)?,
            FLOAD_1 => load(jvm, &mut type_state, 1, VerificationType::Float)?,
            FLOAD_2 => load(jvm, &mut type_state, 2, VerificationType::Float)?,
            FLOAD_3 => load(jvm, &mut type_state, 3, VerificationType::Float)?,
            DLOAD_0 => {
                load(jvm, &mut type_state, 0, VerificationType::Double)?;
                type_state.stack.push(VerificationType::Top);
            }
            DLOAD_1 => {
                load(jvm, &mut type_state, 1, VerificationType::Double)?;
                type_state.stack.push(VerificationType::Top);
            }
            DLOAD_2 => {
                load(jvm, &mut type_state, 2, VerificationType::Double)?;
                type_state.stack.push(VerificationType::Top);
            }
            DLOAD_3 => {
                load(jvm, &mut type_state, 3, VerificationType::Double)?;
                type_state.stack.push(VerificationType::Top);
            }
            ALOAD_0 => load(jvm, &mut type_state, 0, any_object)?,
            ALOAD_1 => load(jvm, &mut type_state, 1, any_object)?,
            ALOAD_2 => load(jvm, &mut type_state, 2, any_object)?,
            ALOAD_3 => load(jvm, &mut type_state, 3, any_object)?,
            BALOAD => {
                todo!()
            }
            ISTORE => {
                let index = read_code_u8(jvm, bytes, pc)? as u16;
                store(jvm, &mut type_state, index, VerificationType::Integer)?
            }
            LSTORE => {
                let index = read_code_u8(jvm, bytes, pc)? as u16;
                store(jvm, &mut type_state, index, VerificationType::Long)?
            }
            FSTORE => {
                let index = read_code_u8(jvm, bytes, pc)? as u16;
                store(jvm, &mut type_state, index, VerificationType::Float)?
            }
            DSTORE => {
                let index = read_code_u8(jvm, bytes, pc)? as u16;
                store(jvm, &mut type_state, index, VerificationType::Double)?
            }
            ASTORE => {
                let index = read_code_u8(jvm, bytes, pc)? as u16;
                store(jvm, &mut type_state, index, any_object)?
            }
            ISTORE_0 => store(jvm, &mut type_state, 0, VerificationType::Integer)?,
            ISTORE_1 => store(jvm, &mut type_state, 1, VerificationType::Integer)?,
            ISTORE_2 => store(jvm, &mut type_state, 2, VerificationType::Integer)?,
            ISTORE_3 => store(jvm, &mut type_state, 3, VerificationType::Integer)?,
            LSTORE_0 => store(jvm, &mut type_state, 0, VerificationType::Long)?,
            LSTORE_1 => store(jvm, &mut type_state, 1, VerificationType::Long)?,
            LSTORE_2 => store(jvm, &mut type_state, 2, VerificationType::Long)?,
            LSTORE_3 => store(jvm, &mut type_state, 3, VerificationType::Long)?,
            FSTORE_0 => store(jvm, &mut type_state, 0, VerificationType::Float)?,
            FSTORE_1 => store(jvm, &mut type_state, 1, VerificationType::Float)?,
            FSTORE_2 => store(jvm, &mut type_state, 2, VerificationType::Float)?,
            FSTORE_3 => store(jvm, &mut type_state, 3, VerificationType::Float)?,
            DSTORE_0 => store(jvm, &mut type_state, 0, VerificationType::Double)?,
            DSTORE_1 => store(jvm, &mut type_state, 1, VerificationType::Double)?,
            DSTORE_2 => store(jvm, &mut type_state, 2, VerificationType::Double)?,
            DSTORE_3 => store(jvm, &mut type_state, 3, VerificationType::Double)?,
            ASTORE_0 => store(jvm, &mut type_state, 0, any_object)?,
            ASTORE_1 => store(jvm, &mut type_state, 1, any_object)?,
            ASTORE_2 => store(jvm, &mut type_state, 2, any_object)?,
            ASTORE_3 => store(jvm, &mut type_state, 3, any_object)?,
            AASTORE => {
                if !matches!(
                    (
                        type_state.stack.pop(),
                        type_state.stack.pop(),
                        type_state.stack.pop()
                    ),
                    (
                        Some(VerificationType::ObjectVariable(_)),
                        Some(VerificationType::Integer),
                        Some(VerificationType::ObjectVariable(Class {
                            element_type: Some(Typ::Ref(_)),
                            ..
                        }))
                    )
                ) {
                    return Err(ve(jvm, "invalid aastore"));
                }
            }
            BASTORE => {
                if !((type_state.stack.pop() == Some(VerificationType::Integer))
                    & (type_state.stack.pop() == Some(VerificationType::Integer))
                    & small_array_type(type_state.pop(jvm)?))
                {
                    return Err(ve(jvm, "invalid bastore"));
                }
            }
            POP => {
                if type_state.pop(jvm)?.category_2() {
                    return Err(ve(jvm, "invalid pop"));
                }
            }
            POP2 => {
                if !type_state.pop(jvm)?.category_2() && type_state.pop(jvm)?.category_2() {
                    return Err(ve(jvm, "invalid pop2"));
                }
            }
            DUP => {
                let typ = type_state.pop(jvm)?;
                if typ.category_2() {
                    return Err(ve(jvm, "invalid dup"));
                }
                type_state.stack.push(typ);
                type_state.stack.push(typ);
            }
            DUP_X1 => {
                let first = type_state.pop(jvm)?;
                let second = type_state.pop(jvm)?;
                if first.category_2() | second.category_2() {
                    return Err(ve(jvm, "invalid dup_x1"));
                }
                type_state.stack.push(first);
                type_state.stack.push(second);
                type_state.stack.push(first);
            }
            DUP_X2 => {
                let first = type_state.pop(jvm)?;
                if let (Some(second), Some(third)) =
                    (type_state.stack.pop(), type_state.stack.pop())
                {
                    type_state.stack.push(first);
                    type_state.stack.push(third);
                    type_state.stack.push(second);
                    type_state.stack.push(first);
                } else {
                    return Err(ve(jvm, "invalid dup_x2"));
                }
            }
            DUP2 => {
                if let (Some(first), Some(second)) =
                    (type_state.stack.pop(), type_state.stack.pop())
                {
                    if second == VerificationType::Top {
                        return Err(ve(jvm, "invalid dup2"));
                    }
                    type_state.stack.push(second);
                    type_state.stack.push(first);
                    type_state.stack.push(second);
                    type_state.stack.push(first);
                } else {
                    return Err(ve(jvm, "invalid dup2"));
                }
            }
            DUP2_X1 => {
                if let (Some(first), Some(second), Some(third)) = (
                    type_state.stack.pop(),
                    type_state.stack.pop(),
                    type_state.stack.pop(),
                ) {
                    if (second == VerificationType::Top) | (third == VerificationType::Top) {
                        return Err(ve(jvm, "invalid dup2_x1"));
                    }
                    type_state.stack.push(second);
                    type_state.stack.push(first);
                    type_state.stack.push(third);
                    type_state.stack.push(second);
                    type_state.stack.push(first);
                } else {
                    return Err(ve(jvm, "invalid dup2_x1"));
                }
            }
            DUP2_X2 => {
                if let (Some(first), Some(second), Some(third), Some(fourth)) = (
                    type_state.stack.pop(),
                    type_state.stack.pop(),
                    type_state.stack.pop(),
                    type_state.stack.pop(),
                ) {
                    if (second == VerificationType::Top) | (fourth == VerificationType::Top) {
                        return Err(ve(jvm, "invalid dup2_x2"));
                    }
                    type_state.stack.push(second);
                    type_state.stack.push(first);
                    type_state.stack.push(fourth);
                    type_state.stack.push(third);
                    type_state.stack.push(second);
                    type_state.stack.push(first);
                } else {
                    return Err(ve(jvm, "invalid dup2_x2"));
                }
            }
            SWAP => {
                let first = type_state.pop(jvm)?;
                let second = type_state.pop(jvm)?;
                if first.category_2() | second.category_2() {
                    return Err(ve(jvm, "invalid swap"));
                }
                type_state.stack.push(first);
                type_state.stack.push(second);
            }
            IADD | ISUB | IMUL | IDIV | IREM | ISHL | ISHR | IUSHR | IAND | IOR | IXOR => {
                if let (Some(VerificationType::Integer), Some(VerificationType::Integer)) =
                    (type_state.stack.pop(), type_state.stack.pop())
                {
                    type_state.stack.push(VerificationType::Integer)
                } else {
                    return Err(ve(jvm, "invalid integer arithmetic"));
                }
            }
            DADD | DSUB | DMUL | DDIV => {
                if let (Ok(VerificationType::Double), Ok(VerificationType::Double)) =
                    (type_state.pop(jvm), type_state.pop(jvm))
                {
                    type_state.stack.push(VerificationType::Double);
                    type_state.stack.push(VerificationType::Top);
                } else {
                    return Err(ve(jvm, "invalid double arithmetic"));
                }
            }
            INEG | I2B | I2C | I2S => {
                top_of_stack(jvm, &mut type_state, VerificationType::Integer)?
            }
            IINC => {
                let index = read_code_u8(jvm, bytes, pc)?;
                read_code_u8(jvm, bytes, pc)?;
                if type_state.locals.get(index as usize) != Some(&VerificationType::Integer) {
                    return Err(ve(jvm, "invalid iinc or equivalent"));
                }
            }
            I2F => {
                if type_state.pop(jvm)? == VerificationType::Integer {
                    type_state.stack.push(VerificationType::Float)
                } else {
                    return Err(ve(jvm, "invalid i2f"));
                }
            }
            I2D => {
                if type_state.pop(jvm)? == VerificationType::Integer {
                    type_state.stack.push(VerificationType::Double);
                    type_state.stack.push(VerificationType::Top)
                } else {
                    return Err(ve(jvm, "invalid i2d"));
                }
            }
            I2L => {
                if type_state.pop(jvm)? == VerificationType::Integer {
                    type_state.stack.push(VerificationType::Long);
                    type_state.stack.push(VerificationType::Top)
                } else {
                    return Err(ve(jvm, "invalid i2l"));
                }
            }
            D2I => {
                if type_state.pop(jvm)? == VerificationType::Double {
                    type_state.stack.push(VerificationType::Integer);
                } else {
                    return Err(ve(jvm, "invalid d2i"));
                }
            }
            IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE => {
                if type_state.pop(jvm)? != VerificationType::Integer {
                    return Err(ve(jvm, "invalid if"));
                }
                relative_jump(jvm, &stack_map_table, &mut type_state, bytes, pc)?;
                connected_to_previous_block = false;
            }
            IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT | IF_ICMPGE | IF_ICMPGT | IF_ICMPLE => {
                if (type_state.pop(jvm)? != VerificationType::Integer)
                    | (type_state.pop(jvm)? != VerificationType::Integer)
                {
                    return Err(ve(jvm, "invalid if_cmp"));
                }
                relative_jump(jvm, &stack_map_table, &mut type_state, bytes, pc)?;
                connected_to_previous_block = false;
            }
            GOTO => {
                relative_jump(jvm, &stack_map_table, &mut type_state, bytes, pc)?;
                connected_to_previous_block = false;
            }
            // TODO: check for uninitialized this on return
            IRETURN => {
                if !(matches!(
                    method.nat.typ.1,
                    Some(Typ::Bool | Typ::Byte | Typ::Char | Typ::Short | Typ::Int)
                ) & (type_state.stack.pop() == Some(VerificationType::Integer)))
                {
                    return Err(ve(jvm, "invalid ireturn"));
                }
                connected_to_previous_block = false
            }
            LRETURN => {
                if !((method.nat.typ.1 == Some(Typ::Long))
                    & (type_state.pop(jvm)? == VerificationType::Long))
                {
                    return Err(ve(jvm, "invalid lreturn"));
                }
                connected_to_previous_block = false
            }
            FRETURN => {
                if !((method.nat.typ.1 == Some(Typ::Float))
                    & (type_state.pop(jvm)? == VerificationType::Float))
                {
                    return Err(ve(jvm, "invalid freturn"));
                }
                connected_to_previous_block = false
            }
            DRETURN => {
                if !((method.nat.typ.1 == Some(Typ::Double))
                    & (type_state.pop(jvm)? == VerificationType::Double))
                {
                    return Err(ve(jvm, "invalid dreturn"));
                }
                connected_to_previous_block = false
            }
            RETURN => {
                if method.nat.typ.1 != None {
                    return Err(ve(jvm, "invalid return"));
                }
                connected_to_previous_block = false
            }
            GETSTATIC => {
                let index = read_code_u16(jvm, bytes, pc)?;
                push_type(
                    jvm,
                    &mut type_state.stack,
                    const_pool.get_field(jvm, index)?.descriptor,
                )?;
            }
            PUTSTATIC => {
                let index = read_code_u16(jvm, bytes, pc)?;
                let typ = const_pool.get_field(jvm, index)?.descriptor;
                pop_type(jvm, &mut type_state.stack, typ)?;
            }
            NEW => {
                let uninit = VerificationType::UninitializedVariable { offset: *pc - 1 };
                let index = read_code_u16(jvm, bytes, pc)?;
                let class = const_pool.get_class(jvm, index)?;
                if class.element_type.is_some() | type_state.stack.contains(&uninit) {
                    return Err(ve(jvm, "invalid new"));
                }
                type_state.stack.push(uninit);
            }
            NEWARRAY => {
                pop_type(jvm, &mut type_state.stack, Typ::Int)?;
                let typ = match read_code_u8(jvm, bytes, pc)? {
                    4 => "[Z",
                    5 => "[C",
                    6 => "[F",
                    7 => "[D",
                    8 => "[B",
                    9 => "[S",
                    10 => "[I",
                    11 => "[J",
                    _ => return Err(ve(jvm, "invalid array type")),
                };
                type_state
                    .stack
                    .push(VerificationType::ObjectVariable(jvm.resolve_class(typ)?));
            }
            ANEWARRAY => {
                if type_state.stack.pop() != Some(VerificationType::Integer) {
                    return Err(ve(jvm, "invalid anewarray"));
                }
                let index = read_code_u16(jvm, bytes, pc)?;
                let component = const_pool.get_class(jvm, index)?;
                let typ = jvm.resolve_class(format!("[L{};", component.name))?;
                type_state.stack.push(VerificationType::ObjectVariable(typ));
            }
            ARRAYLENGTH => {
                if !matches!(
                    type_state.stack.pop(),
                    Some(VerificationType::ObjectVariable(Class {
                        element_type: Some(_),
                        ..
                    })),
                ) {
                    return Err(ve(jvm, "invalid arraylength"));
                }
                type_state.stack.push(VerificationType::Integer);
            }
            _ => todo!(),
        }

        if type_state.stack.len() > code.max_stack as usize {
            return Err(ve(jvm, "max stack exceeded"));
        }
        // If the next specified stack map frame is reached, check if it matches the inferred one
        if let Some((type_state_pc, explicit_type_state)) = next_explicit_type_state {
            match (*pc).cmp(type_state_pc) {
                std::cmp::Ordering::Less => {} // Not at frame yet
                std::cmp::Ordering::Equal => {
                    if !connected_to_previous_block
                        | type_state.is_assignable_to(explicit_type_state)
                    {
                        type_state = explicit_type_state.clone();
                        next_explicit_type_state = type_states.next();
                    } else {
                        return Err(
                            ve(jvm,&format!("typestates not assignable: {type_state:?} to {explicit_type_state:?}"))
                        );
                    }
                }
                std::cmp::Ordering::Greater => {
                    return Err(ve(
                        jvm,
                        &format!(
                        "Stack map frame within instruction (pc: {pc}, frame at {type_state_pc})",
                    ),
                    ))
                }
            }
        }
        connected_to_previous_block = true;
    }
    Ok(())
}

fn pop_type<'a, 'b>(
    jvm: &'b JVM<'a>,
    stack: &'b mut Vec<VerificationType<'a>>,
    typ: Typ<'a>,
) -> JVMResult<'a, ()> {
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
            obj.assignable_to(jvm.resolve_class(ref_type)?)
        }
        _ => false,
    } {
        Ok(())
    } else {
        Err(ve(jvm, &format!("Failed to pop {:?}", typ)))
    }
}

fn load<'a, 'b>(
    jvm: &'b JVM<'a>,
    type_state: &'b mut StackMapFrame<'a>,
    index: u16,
    typ: VerificationType<'a>,
) -> JVMResult<'a, ()> {
    let var = type_state
        .locals
        .get(index as usize)
        .ok_or_else(|| ve(jvm, "Index out of bounds"))?;
    if !var.is_assignable_to(typ) {
        return Err(ve(jvm, "load type mismatch"));
    }
    type_state.stack.push(*var);
    Ok(())
}

fn store<'a, 'b>(
    jvm: &'b JVM<'a>,
    type_state: &'b mut StackMapFrame<'a>,
    index: u16,
    typ: VerificationType<'a>,
) -> JVMResult<'a, ()> {
    let on_stack = type_state.pop(jvm)?;

    let mut cat2_success = true;
    if on_stack.category_2() {
        if index + 1 < type_state.locals.len() as u16 {
            type_state.locals[index as usize + 1] = VerificationType::Top;
        } else {
            cat2_success = false;
        }
    }
    if cat2_success && on_stack.is_assignable_to(typ) & (index < type_state.locals.len() as u16) {
        type_state.locals[index as usize] = on_stack;
        Ok(())
    } else {
        Err(ve(
            jvm,
            &format!("invalid store of {on_stack:?} as {typ:?} ({type_state:?})",),
        ))
    }
}

fn top_of_stack<'a, 'b>(
    jvm: &'b JVM<'a>,
    type_state: &'b mut StackMapFrame<'a>,
    typ: VerificationType<'a>,
) -> JVMResult<'a, ()> {
    if typ.category_2() {
        if (type_state.stack.len() > 1)
            && type_state.stack[type_state.stack.len() - 1] == VerificationType::Top
            && type_state.stack[type_state.stack.len() - 1].is_assignable_to(typ)
        {
            Ok(())
        } else {
            Err(ve(jvm, "top of stack mismatch"))
        }
    } else if !type_state.stack.is_empty()
        && type_state.stack[type_state.stack.len() - 1].is_assignable_to(typ)
    {
        Ok(())
    } else {
        Err(ve(jvm, "top of stack mismatch"))
    }
}

fn relative_jump<'a, 'b>(
    jvm: &'b JVM<'a>,
    stack_map_table: &BTreeMap<u16, StackMapFrame<'a>>,
    type_state: &'b mut StackMapFrame<'a>,
    bytes: &'b [u8],
    pc: &'b mut u16,
) -> JVMResult<'a, ()> {
    let base = *pc - 1;
    let target_offset = read_code_u16(jvm, bytes, pc)? as i16;
    let target = (base as i32 + target_offset as i32) as u16;
    if let Some(target_state) = stack_map_table.get(&target) {
        if type_state.is_assignable_to(target_state) {
            Ok(())
        } else {
            Err(ve(jvm, "invalid jump target typestate"))
        }
    } else {
        Err(ve(jvm, "missing stack map frame"))
    }
}

fn small_array_type(typ: VerificationType) -> bool {
    match typ {
        VerificationType::ObjectVariable(class) => {
            matches!(class.element_type, Some(Typ::Byte | Typ::Bool))
        }
        VerificationType::Null => true,
        _ => false,
    }
}

fn read_code_u8<'a>(jvm: &JVM<'a>, bytes: &[u8], pc: &mut u16) -> JVMResult<'a, u8> {
    *pc += 1;
    bytes
        .get(*pc as usize - 1)
        .copied()
        .ok_or_else(|| ve(jvm, "incomplete instruction"))
}

fn read_code_u16<'a>(jvm: &JVM<'a>, bytes: &[u8], pc: &mut u16) -> JVMResult<'a, u16> {
    Ok(((read_code_u8(jvm, bytes, pc)? as u16) << 8) + read_code_u8(jvm, bytes, pc)? as u16)
}
