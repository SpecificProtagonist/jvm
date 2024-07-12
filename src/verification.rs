use std::{collections::BTreeMap, ops::Deref, sync::atomic::Ordering};

use crate::{
    class::{Class, ClassPtr},
    const_pool::{ConstPool, ConstPoolItem},
    instructions::*,
    jvm::{error, Jvm, JvmResult},
    method::{Bytecode, Code, Method},
    object::Object,
    parse,
    typ::Typ,
    AccessFlags,
};

fn ve(jvm: &Jvm, message: &str) -> Object {
    error(jvm, "VerifyError", message)
}

#[derive(Clone, Default, Debug)]
pub(crate) struct StackMapFrame {
    pub locals: Vec<VerificationType>,
    pub stack: Vec<VerificationType>,
}

impl StackMapFrame {
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
    fn pop(&mut self, jvm: &Jvm) -> JvmResult<VerificationType> {
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

    fn push(&mut self, typ: VerificationType) {
        self.stack.push(typ);
        if typ.category_2() {
            self.stack.push(VerificationType::Top);
        }
    }

    fn pop_type(&mut self, jvm: &Jvm, typ: &Typ) -> JvmResult<()> {
        if match (typ, self.stack.pop()) {
            (
                Typ::Boolean | Typ::Byte | Typ::Short | Typ::Char | Typ::Int,
                Some(VerificationType::Integer),
            ) => true,
            (Typ::Long, Some(VerificationType::Top)) => {
                matches!(self.stack.pop(), Some(VerificationType::Long))
            }
            (Typ::Float, Some(VerificationType::Float)) => true,
            (Typ::Double, Some(VerificationType::Top)) => {
                matches!(self.stack.pop(), Some(VerificationType::Double))
            }
            (Typ::Ref(ref_type), Some(VerificationType::ObjectVariable(obj))) => {
                obj.assignable_to(ref_type)
            }
            _ => false,
        } {
            Ok(())
        } else {
            Err(ve(jvm, &format!("Failed to pop {:?}", typ)))
        }
    }

    fn load(&mut self, jvm: &Jvm, index: u16, typ: VerificationType) -> JvmResult<()> {
        let var = self
            .locals
            .get(index as usize)
            .ok_or_else(|| ve(jvm, "Index out of bounds"))?;
        if !var.is_assignable_to(typ)
            | (var.category_2()
                & (self.locals.get(index as usize + 1) != Some(&VerificationType::Top)))
        {
            return Err(ve(jvm, "load type mismatch"));
        }
        self.push(*var);
        Ok(())
    }

    fn store(&mut self, jvm: &Jvm, index: u16, typ: VerificationType) -> JvmResult<()> {
        let on_stack = self.pop(jvm)?;

        let mut cat2_success = true;
        if on_stack.category_2() {
            if index + 1 < self.locals.len() as u16 {
                self.locals[index as usize + 1] = VerificationType::Top;
            } else {
                cat2_success = false;
            }
        }
        if cat2_success && on_stack.is_assignable_to(typ) & (index < self.locals.len() as u16) {
            self.locals[index as usize] = on_stack;
            Ok(())
        } else {
            Err(ve(
                jvm,
                &format!("invalid store of {on_stack:?} as {typ:?} ({self:?})",),
            ))
        }
    }

    fn top_of_stack(&mut self, jvm: &Jvm, typ: VerificationType) -> JvmResult<()> {
        if typ.category_2() {
            if (self.stack.len() > 1)
                && self.stack[self.stack.len() - 1] == VerificationType::Top
                && self.stack[self.stack.len() - 2].is_assignable_to(typ)
            {
                Ok(())
            } else {
                Err(ve(jvm, "top of stack mismatch"))
            }
        } else if !self.stack.is_empty() && self.stack[self.stack.len() - 1].is_assignable_to(typ) {
            Ok(())
        } else {
            Err(ve(jvm, "top of stack mismatch"))
        }
    }

    fn relative_jump(
        &mut self,
        jvm: &Jvm,
        stack_map_table: &BTreeMap<u16, StackMapFrame>,
        bytes: &[u8],
        pc: &mut u16,
        wide: bool,
    ) -> JvmResult<()> {
        let base = *pc - 1;
        let target_offset = if wide {
            read_code_u32(jvm, bytes, pc)? as i32
        } else {
            read_code_u16(jvm, bytes, pc)? as i16 as i32
        };
        let target = (base as i32 + target_offset as i32) as u16;
        if let Some(target_state) = stack_map_table.get(&target) {
            if self.is_assignable_to(target_state) {
                Ok(())
            } else {
                Err(ve(
                    jvm,
                    &format!("invalid jump target typestate (pc: {base}): typestate {self:?}, target state {target_state:?}"),
                ))
            }
        } else {
            Err(ve(jvm, "missing stack map frame"))
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub(crate) enum VerificationType {
    Top,
    Integer,
    Float,
    Null,
    Reference,
    ObjectVariable(ClassPtr),
    UninitializedThis,
    UninitializedVariable { offset: u16 },
    Long,
    Double,
}

impl Default for VerificationType {
    fn default() -> Self {
        Self::Top
    }
}

impl VerificationType {
    fn is_assignable_to(self, other: Self) -> bool {
        use VerificationType::*;
        match (self, other) {
            (a, b) if a == b => true,
            (_, Top) => true,
            (
                Null | ObjectVariable(_) | UninitializedThis | UninitializedVariable { .. },
                Reference,
            ) => true,
            (ObjectVariable(a), ObjectVariable(b)) => a.assignable_to(&b.name),
            (Null, ObjectVariable(_)) => true,
            _ => false,
        }
    }

    pub fn category_2(self) -> bool {
        matches!(self, VerificationType::Long | VerificationType::Double)
    }

    fn matches(self, typ: &Typ) -> bool {
        matches!(
            (self, typ),
            (
                Self::Integer,
                Typ::Boolean | Typ::Byte | Typ::Char | Typ::Short | Typ::Int
            ) | (Self::Long, Typ::Long)
                | (Self::Float, Typ::Float)
                | (Self::Double, Typ::Double)
                | (Self::Null, Typ::Ref(_))
        ) | matches!((self,typ), (Self::ObjectVariable(class), Typ::Ref(classname)) if &class.name == classname
        )
    }
}

pub(crate) fn push_type(jvm: &Jvm, types: &mut Vec<VerificationType>, typ: &Typ) -> JvmResult<()> {
    match typ {
        Typ::Boolean | Typ::Char | Typ::Byte | Typ::Short | Typ::Int => {
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
        Typ::Ref(class) => types.push(VerificationType::ObjectVariable(
            jvm.resolve_class_internal(class)?,
        )),
    }
    Ok(())
}

// TODO: Exception handlers
/// Verification by type checking (verification by type inference is not planned)
/// Requires class to be resolved (→ only called from Class::verify)
pub(crate) fn verify<'a: 'b, 'b>(jvm: &'b Jvm, class: ClassPtr) -> JvmResult<()> {
    if let Some(super_class) = class.super_class {
        super_class.verify(jvm)?;
        // Check whether the super class is final (as described in spec) is redundant (has to already be checked during loading)
    } else if class.name.as_ref() != "java.lang.Object" {
        return Err(ve(jvm, "class has no superclass"));
    }

    let const_pool = class.const_pool.read();

    for method in class.methods.values() {
        if method.class() == class {
            verify_method(jvm, &const_pool, method)?;
        }
    }

    Ok(())
}

pub(crate) fn verify_method(jvm: &Jvm, const_pool: &ConstPool, method: &Method) -> JvmResult<()> {
    if let (false, Some(super_class)) = (
        method.access_flags.contains(AccessFlags::STATIC),
        method.class().super_class,
    ) {
        if let Some(super_method) = super_class.methods.get(&method.nat) {
            if super_method.access_flags.contains(AccessFlags::FINAL) {
                return Err(ve(jvm, "tried to overwrite final method"));
            }
        }
    }

    match &method.code {
        Code::None => {
            if !method.access_flags.intersects(AccessFlags::ABSTRACT) {
                return Err(ve(jvm, "missing code attribute"));
            }
        }
        Code::Bytecode(code) => {
            if !jvm
                .verification_type_checking_disabled
                .load(Ordering::SeqCst)
            {
                verify_bytecode(jvm, const_pool, method, &code.read())?
            }
        }
        Code::Native(_) => assert!(method.access_flags.contains(AccessFlags::NATIVE)),
    }

    Ok(())
}

fn verify_bytecode(
    jvm: &Jvm,
    const_pool: &ConstPool,
    method: &Method,
    code: &Bytecode,
) -> JvmResult<()> {
    // println!(
    //     "verifying method {}.{}",
    //     method.class().name,
    //     method.nat.name,
    // );

    // The initial typestate is not stored in the class file but derive from the method signature
    let initial_locals = {
        let mut locals = Vec::new();
        if !method.access_flags.contains(AccessFlags::STATIC) {
            locals.push(
                if (method.nat.name.as_ref() != "<init>")
                    || (method.class().name.as_ref() == "java.lang.Object")
                {
                    VerificationType::ObjectVariable(method.class())
                } else {
                    VerificationType::UninitializedThis
                },
            );
        }
        for arg in &method.nat.typ.args {
            push_type(jvm, &mut locals, arg)?
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
        const_pool,
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

    // Iterate over all instructions
    // TODO: switch result to bool
    while (*pc as usize) < code.bytes.len() {
        connected_to_previous_block = true;
        match read_code_u8(jvm, bytes, pc)? {
            NOP => {}
            ACONST_NULL => type_state.push(VerificationType::Null),
            ICONST_0 | ICONST_1 | ICONST_2 | ICONST_3 | ICONST_4 | ICONST_5 | ICONST_M1 => {
                type_state.push(VerificationType::Integer)
            }
            LCONST_0 | LCONST_1 => {
                type_state.push(VerificationType::Long);
            }
            FCONST_0 | FCONST_1 | FCONST_2 => type_state.stack.push(VerificationType::Float),
            DCONST_0 | DCONST_1 => {
                type_state.push(VerificationType::Double);
            }
            BIPUSH => {
                *pc += 1;
                type_state.push(VerificationType::Integer)
            }
            SIPUSH => {
                *pc += 2;
                type_state.push(VerificationType::Integer)
            }
            LDC => {
                let index = read_code_u8(jvm, bytes, pc)? as u16;
                match const_pool.items.get(index as usize) {
                    Some(ConstPoolItem::Integer(_)) => type_state.push(VerificationType::Integer),
                    Some(ConstPoolItem::Float(_)) => type_state.push(VerificationType::Float),
                    Some(ConstPoolItem::RawString(_)) => {
                        type_state.push(VerificationType::ObjectVariable(
                            jvm.resolve_class_internal("java.lang.String")?,
                        ))
                    }
                    Some(ConstPoolItem::Class(class)) => {
                        type_state.push(VerificationType::ObjectVariable(class))
                    }
                    _ => return Err(ve(jvm, "invalid ldc")),
                }
            }
            LDC_W => {
                let index = read_code_u16(jvm, bytes, pc)?;
                match const_pool.items.get(index as usize) {
                    Some(ConstPoolItem::Integer(_)) => type_state.push(VerificationType::Integer),
                    Some(ConstPoolItem::Float(_)) => type_state.push(VerificationType::Float),
                    Some(ConstPoolItem::RawString(_)) => {
                        type_state.push(VerificationType::ObjectVariable(
                            jvm.resolve_class_internal("java.lang.String")?,
                        ))
                    }
                    Some(ConstPoolItem::Class(class)) => {
                        type_state.push(VerificationType::ObjectVariable(class))
                    }
                    _ => return Err(ve(jvm, "invalid ldc_w")),
                }
            }
            LDC2_W => {
                let index = read_code_u16(jvm, bytes, pc)?;
                match const_pool.items.get(index as usize) {
                    Some(ConstPoolItem::Double(_)) => {
                        type_state.push(VerificationType::Double);
                    }
                    Some(ConstPoolItem::Long(_)) => {
                        type_state.push(VerificationType::Long);
                    }
                    _ => return Err(ve(jvm, "invalid ldc2_w")),
                }
            }
            ILOAD => {
                let index = read_code_u8(jvm, bytes, pc)? as u16;
                type_state.load(jvm, index, VerificationType::Integer)?
            }
            LLOAD => {
                let index = read_code_u8(jvm, bytes, pc)? as u16;
                type_state.load(jvm, index, VerificationType::Long)?;
            }
            FLOAD => {
                let index = read_code_u8(jvm, bytes, pc)? as u16;
                type_state.load(jvm, index, VerificationType::Float)?
            }
            DLOAD => {
                let index = read_code_u8(jvm, bytes, pc)? as u16;
                type_state.load(jvm, index, VerificationType::Double)?;
            }
            ALOAD => {
                let index = read_code_u8(jvm, bytes, pc)? as u16;
                type_state.load(jvm, index, VerificationType::Reference)?
            }
            ILOAD_0 => type_state.load(jvm, 0, VerificationType::Integer)?,
            ILOAD_1 => type_state.load(jvm, 1, VerificationType::Integer)?,
            ILOAD_2 => type_state.load(jvm, 2, VerificationType::Integer)?,
            ILOAD_3 => type_state.load(jvm, 3, VerificationType::Integer)?,
            LLOAD_0 => {
                type_state.load(jvm, 0, VerificationType::Long)?;
            }
            LLOAD_1 => {
                type_state.load(jvm, 1, VerificationType::Long)?;
            }
            LLOAD_2 => {
                type_state.load(jvm, 2, VerificationType::Long)?;
            }
            LLOAD_3 => {
                type_state.load(jvm, 3, VerificationType::Long)?;
            }
            FLOAD_0 => type_state.load(jvm, 0, VerificationType::Float)?,
            FLOAD_1 => type_state.load(jvm, 1, VerificationType::Float)?,
            FLOAD_2 => type_state.load(jvm, 2, VerificationType::Float)?,
            FLOAD_3 => type_state.load(jvm, 3, VerificationType::Float)?,
            DLOAD_0 => {
                type_state.load(jvm, 0, VerificationType::Double)?;
            }
            DLOAD_1 => {
                type_state.load(jvm, 1, VerificationType::Double)?;
            }
            DLOAD_2 => {
                type_state.load(jvm, 2, VerificationType::Double)?;
            }
            DLOAD_3 => {
                type_state.load(jvm, 3, VerificationType::Double)?;
            }
            ALOAD_0 => type_state.load(jvm, 0, VerificationType::Reference)?,
            ALOAD_1 => type_state.load(jvm, 1, VerificationType::Reference)?,
            ALOAD_2 => type_state.load(jvm, 2, VerificationType::Reference)?,
            ALOAD_3 => type_state.load(jvm, 3, VerificationType::Reference)?,
            IALOAD => {
                if (type_state.pop(jvm)? != VerificationType::Integer)
                    | !matches!(
                        type_state.pop(jvm)?,
                        VerificationType::ObjectVariable(Class {
                            element_type: Some(Typ::Int),
                            ..
                        })
                    )
                {
                    return Err(ve(jvm, "invalid iaload"));
                }
                type_state.push(VerificationType::Integer);
            }
            LALOAD => {
                if (type_state.pop(jvm)? != VerificationType::Integer)
                    | !matches!(
                        type_state.pop(jvm)?,
                        VerificationType::ObjectVariable(Class {
                            element_type: Some(Typ::Long),
                            ..
                        })
                    )
                {
                    return Err(ve(jvm, "invalid laload"));
                }
                type_state.push(VerificationType::Long);
            }
            FALOAD => {
                if (type_state.pop(jvm)? != VerificationType::Integer)
                    | !matches!(
                        type_state.pop(jvm)?,
                        VerificationType::ObjectVariable(Class {
                            element_type: Some(Typ::Float),
                            ..
                        })
                    )
                {
                    return Err(ve(jvm, "invalid faload"));
                }
                type_state.push(VerificationType::Float);
            }
            DALOAD => {
                if (type_state.pop(jvm)? != VerificationType::Integer)
                    | !matches!(
                        type_state.pop(jvm)?,
                        VerificationType::ObjectVariable(Class {
                            element_type: Some(Typ::Double),
                            ..
                        })
                    )
                {
                    return Err(ve(jvm, "invalid daload"));
                }
                type_state.push(VerificationType::Double);
            }
            AALOAD => {
                if type_state.pop(jvm)? != VerificationType::Integer {
                    return Err(ve(jvm, "invalid aaload"));
                }
                let component = match type_state.pop(jvm)? {
                    VerificationType::Null => VerificationType::Null,
                    VerificationType::ObjectVariable(class) => {
                        if let Some(Typ::Ref(component)) = &class.element_type {
                            let class = jvm.resolve_class_internal(component)?;
                            VerificationType::ObjectVariable(class)
                        } else {
                            return Err(ve(jvm, "invalid aaload"));
                        }
                    }
                    _ => {
                        return Err(ve(jvm, "invalid aaload"));
                    }
                };
                type_state.push(component);
            }
            BALOAD => {
                if (type_state.pop(jvm)? != VerificationType::Integer)
                    | !small_array_type(type_state.pop(jvm)?)
                {
                    return Err(ve(jvm, "invalid baload"));
                }
                type_state.push(VerificationType::Integer);
            }
            CALOAD => {
                if (type_state.pop(jvm)? != VerificationType::Integer)
                    | !matches!(
                        type_state.pop(jvm)?,
                        VerificationType::ObjectVariable(Class {
                            element_type: Some(Typ::Char),
                            ..
                        })
                    )
                {
                    return Err(ve(jvm, "invalid caload"));
                }
                type_state.push(VerificationType::Integer);
            }
            SALOAD => {
                if (type_state.pop(jvm)? != VerificationType::Integer)
                    | !matches!(
                        type_state.pop(jvm)?,
                        VerificationType::ObjectVariable(Class {
                            element_type: Some(Typ::Short),
                            ..
                        })
                    )
                {
                    return Err(ve(jvm, "invalid saload"));
                }
                type_state.push(VerificationType::Integer);
            }
            ISTORE => {
                let index = read_code_u8(jvm, bytes, pc)? as u16;
                type_state.store(jvm, index, VerificationType::Integer)?
            }
            LSTORE => {
                let index = read_code_u8(jvm, bytes, pc)? as u16;
                type_state.store(jvm, index, VerificationType::Long)?
            }
            FSTORE => {
                let index = read_code_u8(jvm, bytes, pc)? as u16;
                type_state.store(jvm, index, VerificationType::Float)?
            }
            DSTORE => {
                let index = read_code_u8(jvm, bytes, pc)? as u16;
                type_state.store(jvm, index, VerificationType::Double)?
            }
            ASTORE => {
                let index = read_code_u8(jvm, bytes, pc)? as u16;
                type_state.store(jvm, index, VerificationType::Reference)?
            }
            ISTORE_0 => type_state.store(jvm, 0, VerificationType::Integer)?,
            ISTORE_1 => type_state.store(jvm, 1, VerificationType::Integer)?,
            ISTORE_2 => type_state.store(jvm, 2, VerificationType::Integer)?,
            ISTORE_3 => type_state.store(jvm, 3, VerificationType::Integer)?,
            LSTORE_0 => type_state.store(jvm, 0, VerificationType::Long)?,
            LSTORE_1 => type_state.store(jvm, 1, VerificationType::Long)?,
            LSTORE_2 => type_state.store(jvm, 2, VerificationType::Long)?,
            LSTORE_3 => type_state.store(jvm, 3, VerificationType::Long)?,
            FSTORE_0 => type_state.store(jvm, 0, VerificationType::Float)?,
            FSTORE_1 => type_state.store(jvm, 1, VerificationType::Float)?,
            FSTORE_2 => type_state.store(jvm, 2, VerificationType::Float)?,
            FSTORE_3 => type_state.store(jvm, 3, VerificationType::Float)?,
            DSTORE_0 => type_state.store(jvm, 0, VerificationType::Double)?,
            DSTORE_1 => type_state.store(jvm, 1, VerificationType::Double)?,
            DSTORE_2 => type_state.store(jvm, 2, VerificationType::Double)?,
            DSTORE_3 => type_state.store(jvm, 3, VerificationType::Double)?,
            ASTORE_0 => type_state.store(jvm, 0, VerificationType::Reference)?,
            ASTORE_1 => type_state.store(jvm, 1, VerificationType::Reference)?,
            ASTORE_2 => type_state.store(jvm, 2, VerificationType::Reference)?,
            ASTORE_3 => type_state.store(jvm, 3, VerificationType::Reference)?,
            IASTORE => {
                if !((type_state.pop(jvm)? == VerificationType::Integer)
                    & (type_state.pop(jvm)? == VerificationType::Integer)
                    & matches!(
                        type_state.pop(jvm)?,
                        VerificationType::ObjectVariable(Class {
                            element_type: Some(Typ::Int),
                            ..
                        })
                    ))
                {
                    return Err(ve(jvm, "invalid iastore"));
                }
            }
            LASTORE => {
                if !((type_state.pop(jvm)? == VerificationType::Long)
                    & (type_state.pop(jvm)? == VerificationType::Integer)
                    & matches!(
                        type_state.pop(jvm)?,
                        VerificationType::ObjectVariable(Class {
                            element_type: Some(Typ::Long),
                            ..
                        })
                    ))
                {
                    return Err(ve(jvm, "invalid lastore"));
                }
            }
            FASTORE => {
                if !((type_state.pop(jvm)? == VerificationType::Float)
                    & (type_state.pop(jvm)? == VerificationType::Integer)
                    & matches!(
                        type_state.pop(jvm)?,
                        VerificationType::ObjectVariable(Class {
                            element_type: Some(Typ::Float),
                            ..
                        })
                    ))
                {
                    return Err(ve(jvm, "invalid fastore"));
                }
            }
            DASTORE => {
                if !((type_state.pop(jvm)? == VerificationType::Double)
                    & (type_state.pop(jvm)? == VerificationType::Integer)
                    & matches!(
                        type_state.pop(jvm)?,
                        VerificationType::ObjectVariable(Class {
                            element_type: Some(Typ::Double),
                            ..
                        })
                    ))
                {
                    return Err(ve(jvm, "invalid dastore"));
                }
            }
            AASTORE => {
                if !matches!(
                    (
                        type_state.pop(jvm)?,
                        type_state.pop(jvm)?,
                        type_state.pop(jvm)?
                    ),
                    (
                        VerificationType::ObjectVariable(_),
                        VerificationType::Integer,
                        VerificationType::ObjectVariable(Class {
                            element_type: Some(Typ::Ref(_)),
                            ..
                        })
                    )
                ) {
                    return Err(ve(jvm, "invalid aastore"));
                }
            }
            BASTORE => {
                if !((type_state.pop(jvm)? == VerificationType::Integer)
                    & (type_state.pop(jvm)? == VerificationType::Integer)
                    & small_array_type(type_state.pop(jvm)?))
                {
                    return Err(ve(jvm, "invalid bastore"));
                }
            }
            CASTORE => {
                if !((type_state.pop(jvm)? == VerificationType::Integer)
                    & (type_state.pop(jvm)? == VerificationType::Integer)
                    & matches!(
                        type_state.pop(jvm)?,
                        VerificationType::ObjectVariable(Class {
                            element_type: Some(Typ::Char),
                            ..
                        })
                    ))
                {
                    return Err(ve(jvm, "invalid castore"));
                }
            }
            SASTORE => {
                if !((type_state.pop(jvm)? == VerificationType::Integer)
                    & (type_state.pop(jvm)? == VerificationType::Integer)
                    & matches!(
                        type_state.pop(jvm)?,
                        VerificationType::ObjectVariable(Class {
                            element_type: Some(Typ::Short),
                            ..
                        })
                    ))
                {
                    return Err(ve(jvm, "invalid sastore"));
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
                type_state.push(typ);
                type_state.push(typ);
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
                let (Some(second), Some(third)) = (type_state.stack.pop(), type_state.stack.pop())
                else {
                    return Err(ve(jvm, "invalid dup_x2"));
                };
                if third == VerificationType::Top {
                    return Err(ve(jvm, "invalid dup_x2"));
                }
                type_state.stack.push(first);
                type_state.stack.push(third);
                type_state.stack.push(second);
                type_state.stack.push(first);
            }
            DUP2 => {
                let (Some(first), Some(second)) = (type_state.stack.pop(), type_state.stack.pop())
                else {
                    return Err(ve(jvm, "invalid dup2"));
                };
                if second == VerificationType::Top {
                    return Err(ve(jvm, "invalid dup2"));
                }
                type_state.stack.push(second);
                type_state.stack.push(first);
                type_state.stack.push(second);
                type_state.stack.push(first);
            }
            DUP2_X1 => {
                let (Some(first), Some(second), Some(third)) = (
                    type_state.stack.pop(),
                    type_state.stack.pop(),
                    type_state.stack.pop(),
                ) else {
                    return Err(ve(jvm, "invalid dup2_x1"));
                };
                if (second == VerificationType::Top) | (third == VerificationType::Top) {
                    return Err(ve(jvm, "invalid dup2_x1"));
                }
                type_state.stack.push(second);
                type_state.stack.push(first);
                type_state.stack.push(third);
                type_state.stack.push(second);
                type_state.stack.push(first);
            }
            DUP2_X2 => {
                let (Some(first), Some(second), Some(third), Some(fourth)) = (
                    type_state.stack.pop(),
                    type_state.stack.pop(),
                    type_state.stack.pop(),
                    type_state.stack.pop(),
                ) else {
                    return Err(ve(jvm, "invalid dup2_x2"));
                };
                if (second == VerificationType::Top) | (fourth == VerificationType::Top) {
                    return Err(ve(jvm, "invalid dup2_x2"));
                }
                type_state.stack.push(second);
                type_state.stack.push(first);
                type_state.stack.push(fourth);
                type_state.stack.push(third);
                type_state.stack.push(second);
                type_state.stack.push(first);
            }
            SWAP => {
                let first = type_state.pop(jvm)?;
                let second = type_state.pop(jvm)?;
                if first.category_2() | second.category_2() {
                    return Err(ve(jvm, "invalid swap"));
                }
                type_state.push(first);
                type_state.push(second);
            }
            IADD | ISUB | IMUL | IDIV | IREM | ISHL | ISHR | IUSHR | IAND | IOR | IXOR => {
                if (type_state.pop(jvm)?, type_state.pop(jvm)?)
                    != (VerificationType::Integer, VerificationType::Integer)
                {
                    return Err(ve(jvm, "invalid integer arithmetic"));
                };
                type_state.push(VerificationType::Integer)
            }
            LADD | LSUB | LMUL | LDIV | LREM | LSHL | LSHR | LUSHR | LAND | LOR | LXOR => {
                if (type_state.pop(jvm)?, type_state.pop(jvm)?)
                    != (VerificationType::Long, VerificationType::Long)
                {
                    return Err(ve(jvm, "invalid long arithmetic"));
                };
                type_state.push(VerificationType::Long)
            }
            FADD | FSUB | FMUL | FDIV | FREM => {
                if (type_state.pop(jvm)?, type_state.pop(jvm)?)
                    != (VerificationType::Float, VerificationType::Float)
                {
                    return Err(ve(jvm, "invalid float arithmetic"));
                };
                type_state.push(VerificationType::Float)
            }
            DADD | DSUB | DMUL | DDIV | DREM => {
                if (type_state.pop(jvm)?, type_state.pop(jvm)?)
                    != (VerificationType::Double, VerificationType::Double)
                {
                    return Err(ve(jvm, "invalid double arithmetic"));
                };
                type_state.push(VerificationType::Double);
            }
            INEG | I2B | I2C | I2S => type_state.top_of_stack(jvm, VerificationType::Integer)?,
            LNEG => type_state.top_of_stack(jvm, VerificationType::Long)?,
            FNEG => type_state.top_of_stack(jvm, VerificationType::Float)?,
            DNEG => type_state.top_of_stack(jvm, VerificationType::Double)?,
            IINC => {
                let index = read_code_u8(jvm, bytes, pc)?;
                read_code_u8(jvm, bytes, pc)?;
                if type_state.locals.get(index as usize) != Some(&VerificationType::Integer) {
                    return Err(ve(jvm, "invalid iinc or equivalent"));
                }
            }
            I2F => {
                if type_state.pop(jvm)? != VerificationType::Integer {
                    return Err(ve(jvm, "invalid i2f"));
                }
                type_state.push(VerificationType::Float)
            }
            I2D => {
                if type_state.pop(jvm)? != VerificationType::Integer {
                    return Err(ve(jvm, "invalid i2d"));
                }
                type_state.push(VerificationType::Double);
            }
            I2L => {
                if type_state.pop(jvm)? != VerificationType::Integer {
                    return Err(ve(jvm, "invalid i2l"));
                }
                type_state.push(VerificationType::Long);
            }
            L2I => {
                if type_state.pop(jvm)? != VerificationType::Long {
                    return Err(ve(jvm, "invalid l2i"));
                }
                type_state.push(VerificationType::Integer);
            }
            L2F => {
                if type_state.pop(jvm)? != VerificationType::Long {
                    return Err(ve(jvm, "invalid l2f"));
                }
                type_state.push(VerificationType::Float);
            }
            L2D => {
                if type_state.pop(jvm)? != VerificationType::Long {
                    return Err(ve(jvm, "invalid l2d"));
                }
                type_state.push(VerificationType::Double);
            }
            F2I => {
                if type_state.pop(jvm)? != VerificationType::Float {
                    return Err(ve(jvm, "invalid f2i"));
                }
                type_state.push(VerificationType::Integer);
            }
            F2L => {
                if type_state.pop(jvm)? != VerificationType::Float {
                    return Err(ve(jvm, "invalid f2l"));
                }
                type_state.push(VerificationType::Long);
            }
            F2D => {
                if type_state.pop(jvm)? != VerificationType::Float {
                    return Err(ve(jvm, "invalid f2d"));
                }
                type_state.push(VerificationType::Double);
            }
            D2I => {
                if type_state.pop(jvm)? != VerificationType::Double {
                    return Err(ve(jvm, "invalid d2i"));
                }
                type_state.push(VerificationType::Integer);
            }
            D2L => {
                if type_state.pop(jvm)? != VerificationType::Double {
                    return Err(ve(jvm, "invalid d2l"));
                }
                type_state.push(VerificationType::Long);
            }
            D2F => {
                if type_state.pop(jvm)? != VerificationType::Double {
                    return Err(ve(jvm, "invalid d2f"));
                }
                type_state.push(VerificationType::Float);
            }
            LCMP => {
                if (type_state.pop(jvm)?, type_state.pop(jvm)?)
                    != (VerificationType::Long, VerificationType::Long)
                {
                    return Err(ve(jvm, "invalid lcmp"));
                }
                type_state.push(VerificationType::Integer);
            }
            FCMPL | FCMPG => {
                if (type_state.pop(jvm)?, type_state.pop(jvm)?)
                    != (VerificationType::Float, VerificationType::Float)
                {
                    return Err(ve(jvm, "invalid fcmp"));
                }
                type_state.push(VerificationType::Integer);
            }
            DCMPL | DCMPG => {
                if (type_state.pop(jvm)?, type_state.pop(jvm)?)
                    != (VerificationType::Double, VerificationType::Double)
                {
                    return Err(ve(jvm, "invalid dcmp"));
                }
                type_state.push(VerificationType::Integer);
            }
            IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE => {
                if type_state.pop(jvm)? != VerificationType::Integer {
                    return Err(ve(jvm, "invalid if"));
                }
                type_state.relative_jump(jvm, &stack_map_table, bytes, pc, false)?;
                connected_to_previous_block = false;
            }
            IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT | IF_ICMPGE | IF_ICMPGT | IF_ICMPLE => {
                if (type_state.pop(jvm)? != VerificationType::Integer)
                    | (type_state.pop(jvm)? != VerificationType::Integer)
                {
                    return Err(ve(jvm, "invalid if_cmp"));
                }
                type_state.relative_jump(jvm, &stack_map_table, bytes, pc, false)?;
                connected_to_previous_block = false;
            }
            GOTO => {
                type_state.relative_jump(jvm, &stack_map_table, bytes, pc, false)?;
                connected_to_previous_block = false;
            }
            // TODO: check for uninitialized this on return
            IRETURN => {
                if !matches!(
                    method.nat.typ.returns,
                    Some(Typ::Boolean | Typ::Byte | Typ::Char | Typ::Short | Typ::Int)
                ) | (type_state.pop(jvm)? != VerificationType::Integer)
                {
                    return Err(ve(jvm, "invalid ireturn"));
                }
                connected_to_previous_block = false
            }
            LRETURN => {
                if (method.nat.typ.returns != Some(Typ::Long))
                    | (type_state.pop(jvm)? != VerificationType::Long)
                {
                    return Err(ve(jvm, "invalid lreturn"));
                }
                connected_to_previous_block = false
            }
            FRETURN => {
                if (method.nat.typ.returns != Some(Typ::Float))
                    | (type_state.pop(jvm)? != VerificationType::Float)
                {
                    return Err(ve(jvm, "invalid freturn"));
                }
                connected_to_previous_block = false
            }
            DRETURN => {
                if (method.nat.typ.returns != Some(Typ::Double))
                    | (type_state.pop(jvm)? != VerificationType::Double)
                {
                    return Err(ve(jvm, "invalid dreturn"));
                }
                connected_to_previous_block = false
            }
            RETURN => {
                if method.nat.typ.returns.is_some() {
                    return Err(ve(jvm, "invalid return"));
                }
                connected_to_previous_block = false
            }
            GETSTATIC => {
                let index = read_code_u16(jvm, bytes, pc)?;
                push_type(
                    jvm,
                    &mut type_state.stack,
                    &const_pool.get_field(jvm, index)?.nat.typ,
                )?;
            }
            PUTSTATIC => {
                let index = read_code_u16(jvm, bytes, pc)?;
                let typ = &const_pool.get_field(jvm, index)?.nat.typ;
                type_state.pop_type(jvm, typ)?;
            }
            GETFIELD => {
                let index = read_code_u16(jvm, bytes, pc)?;
                let field = const_pool.get_field(jvm, index)?;
                if match type_state.pop(jvm)? {
                    VerificationType::ObjectVariable(class) => {
                        !class.assignable_to(&field.class().name)
                    }
                    _ => true,
                } {
                    return Err(ve(jvm, "invalid getfield"));
                }
                push_type(jvm, &mut type_state.stack, &field.nat.typ)?;
            }
            PUTFIELD => {
                let index = read_code_u16(jvm, bytes, pc)?;
                let field = const_pool.get_field(jvm, index)?;
                type_state.pop_type(jvm, &field.nat.typ)?;
                if match type_state.pop(jvm)? {
                    VerificationType::ObjectVariable(class) => {
                        !class.assignable_to(&field.class().name)
                    }
                    VerificationType::UninitializedThis => {
                        (method.nat.name.as_ref() == "<init>")
                            & method.class().assignable_to(&field.class().name)
                    }
                    _ => true,
                } {
                    return Err(ve(jvm, "invalid putfield"));
                }
            }
            INVOKEVIRTUAL => {
                let index = read_code_u16(jvm, bytes, pc)?;
                let (invoke_class, invoke_nat) = const_pool.get_virtual_method(jvm, index)?;
                let mut valid = !matches!(invoke_nat.name.as_ref(), "<init>" | "<clinit>");
                for arg in invoke_nat.typ.args.iter().rev() {
                    valid &= type_state.pop(jvm)?.matches(arg);
                }
                valid &= type_state.pop(jvm)? == VerificationType::ObjectVariable(invoke_class);
                if let Some(return_type) = &invoke_nat.typ.returns {
                    push_type(jvm, &mut type_state.stack, return_type)?
                }
                if !valid {
                    return Err(ve(jvm, "invalid invokevirtual"));
                }
            }
            INVOKESPECIAL => {
                let index = read_code_u16(jvm, bytes, pc)?;
                let (invoke_class, invoke_nat) = const_pool.get_virtual_method(jvm, index)?;
                let mut valid = invoke_nat.name.deref() != "<clinit>";
                if invoke_nat.name.deref() == "<init>" {
                    valid &= invoke_nat.typ.returns.is_none();
                    for arg in invoke_nat.typ.args.iter().rev() {
                        valid &= type_state.pop(jvm)?.matches(arg);
                    }
                    let uninit_type = type_state.pop(jvm)?;
                    let init_type = match uninit_type {
                        VerificationType::UninitializedThis => {
                            VerificationType::ObjectVariable(method.class())
                        }
                        VerificationType::UninitializedVariable { offset } => {
                            valid &= code.bytes[offset as usize] == NEW;
                            VerificationType::ObjectVariable(invoke_class)
                        }
                        _ => return Err(ve(jvm, "invalid invokespecial")),
                    };
                    for typ in type_state
                        .locals
                        .iter_mut()
                        .chain(type_state.stack.iter_mut())
                    {
                        if *typ == uninit_type {
                            *typ = init_type
                        }
                    }
                } else {
                    for arg in invoke_nat.typ.args.iter().rev() {
                        valid &= type_state.pop(jvm)?.matches(arg);
                    }
                    let object = type_state.pop(jvm)?;
                    valid &= invoke_class == method.class();
                    valid &= object.matches(&Typ::Ref(invoke_class.name.clone()));
                    if let Some(return_type) = &invoke_nat.typ.returns {
                        push_type(jvm, &mut type_state.stack, return_type)?
                    }
                }
                if !valid {
                    return Err(ve(jvm, "invalid invokespecial"));
                }
            }
            INVOKESTATIC => {
                let index = read_code_u16(jvm, bytes, pc)?;
                let invoke_method = const_pool.get_static_method(jvm, index)?;
                let mut valid = !matches!(invoke_method.nat.name.as_ref(), "<init>" | "<clinit>");
                for arg in invoke_method.nat.typ.args.iter().rev() {
                    valid &= type_state.pop(jvm)?.matches(arg);
                }
                if let Some(return_type) = &invoke_method.nat.typ.returns {
                    push_type(jvm, &mut type_state.stack, return_type)?
                }
                if !valid {
                    return Err(ve(jvm, "invalid invokestatic"));
                }
            }
            NEW => {
                let uninit = VerificationType::UninitializedVariable { offset: *pc - 1 };
                let index = read_code_u16(jvm, bytes, pc)?;
                let class = const_pool.get_class(jvm, index)?;
                if class.element_type.is_some() | type_state.stack.contains(&uninit) {
                    return Err(ve(jvm, "invalid new"));
                }
                type_state.push(uninit);
            }
            NEWARRAY => {
                type_state.pop_type(jvm, &Typ::Int)?;
                let component = match read_code_u8(jvm, bytes, pc)? {
                    4 => Typ::Boolean,
                    5 => Typ::Byte,
                    6 => Typ::Char,
                    7 => Typ::Short,
                    8 => Typ::Double,
                    9 => Typ::Float,
                    10 => Typ::Int,
                    11 => Typ::Long,
                    _ => return Err(ve(jvm, "invalid array type")),
                };
                type_state.push(VerificationType::ObjectVariable(
                    jvm.resolve_array_of(&component)?,
                ));
            }
            ANEWARRAY => {
                if type_state.pop(jvm)? != VerificationType::Integer {
                    return Err(ve(jvm, "invalid anewarray"));
                }
                let index = read_code_u16(jvm, bytes, pc)?;
                let component = const_pool.get_class(jvm, index)?;
                let typ = jvm.resolve_array_of(&component.typ())?;
                type_state.push(VerificationType::ObjectVariable(typ));
            }
            ARRAYLENGTH => {
                if !matches!(
                    type_state.pop(jvm)?,
                    VerificationType::ObjectVariable(Class {
                        element_type: Some(_),
                        ..
                    }),
                ) {
                    return Err(ve(jvm, "invalid arraylength"));
                }
                type_state.push(VerificationType::Integer);
            }
            IFNULL | IFNONNULL => {
                if !matches!(type_state.pop(jvm)?, VerificationType::ObjectVariable(_)) {
                    return Err(ve(jvm, "invalid if"));
                }
                type_state.relative_jump(jvm, &stack_map_table, bytes, pc, false)?;
                connected_to_previous_block = false;
            }
            GOTO_W => {
                type_state.relative_jump(jvm, &stack_map_table, bytes, pc, true)?;
                connected_to_previous_block = false;
            }
            other => todo!("bytecode {other}"),
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
    }
    // TODO: check specification
    if connected_to_previous_block {
        return Err(ve(jvm, "final block open"));
    }
    Ok(())
}

fn small_array_type(typ: VerificationType) -> bool {
    match typ {
        VerificationType::ObjectVariable(class) => {
            matches!(class.element_type, Some(Typ::Byte | Typ::Boolean))
        }
        VerificationType::Null => true,
        _ => false,
    }
}

fn read_code_u8(jvm: &Jvm, bytes: &[u8], pc: &mut u16) -> JvmResult<u8> {
    *pc += 1;
    bytes
        .get(*pc as usize - 1)
        .copied()
        .ok_or_else(|| ve(jvm, "incomplete instruction"))
}

fn read_code_u16(jvm: &Jvm, bytes: &[u8], pc: &mut u16) -> JvmResult<u16> {
    Ok(((read_code_u8(jvm, bytes, pc)? as u16) << 8) + read_code_u8(jvm, bytes, pc)? as u16)
}

fn read_code_u32(jvm: &Jvm, bytes: &[u8], pc: &mut u16) -> JvmResult<u32> {
    Ok(((read_code_u16(jvm, bytes, pc)? as u32) << 8) + read_code_u16(jvm, bytes, pc)? as u32)
}
