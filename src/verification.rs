use std::{cell::SyncUnsafeCell, collections::BTreeMap, ops::Deref};

use crate::{
    class::{Class, ClassRef},
    const_pool::{ConstPool, ConstPoolItem},
    jvm::{error, str_to_java, Jvm, JvmResult},
    method::{Bytecode, Code, Method},
    opcodes::*,
    parse,
    typ::Type,
    value::Value,
    AccessFlags,
};

macro_rules! error {
    ($jvm:expr) => {
        error($jvm, "VerifyError", &format!("verify error {}", line!()))
    };
}

macro_rules! verify {
    ($jvm:expr, $expr:expr) => {
        if !$expr {
            return Err(error!($jvm));
        }
    };
}

macro_rules! verify_failed {
    ($jvm:expr) => {
        return Err(error!($jvm))
    };
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub(crate) enum VerificationType {
    Top,
    Integer,
    Float,
    Null,
    Reference,
    ObjectVariable(ClassRef),
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
    fn is_assignable_to(self, to: Self) -> bool {
        pub fn is_java_assignable(from: ClassRef, to: ClassRef) -> bool {
            // Interfaces are to be treated as java.lang.Object
            to.is_interface()
                || std::ptr::eq(from, to)
                || from.super_class.is_some_and(|s| is_java_assignable(s, to))
        }

        use VerificationType::*;
        match (self, to) {
            (from, to) if from == to => true,
            (_, Top) => true,
            (
                Null | ObjectVariable(_) | UninitializedThis | UninitializedVariable { .. },
                Reference,
            ) => true,
            (ObjectVariable(from), ObjectVariable(to)) => is_java_assignable(from, to),
            (Null, ObjectVariable(_)) => true,
            _ => false,
        }
    }

    pub fn category_2(self) -> bool {
        matches!(self, VerificationType::Long | VerificationType::Double)
    }

    // TODO: Does this need to do subtyping?
    fn matches(self, typ: &Type) -> bool {
        matches!(
            (self, typ),
            (
                Self::Integer,
                Type::Boolean | Type::Byte | Type::Char | Type::Short | Type::Int
            ) | (Self::Long, Type::Long)
                | (Self::Float, Type::Float)
                | (Self::Double, Type::Double)
                | (Self::Null, Type::Ref(_))
        ) | matches!((self,typ), (Self::ObjectVariable(class), Type::Ref(classname)) if &class.name == classname
        )
    }
}

#[derive(Clone, Default, Debug)]
pub(crate) struct StackMapFrame {
    pub locals: Vec<VerificationType>,
    pub stack: Vec<VerificationType>,
}

impl StackMapFrame {
    fn is_assignable_to(&self, to: &Self) -> bool {
        (self.locals.len() == to.locals.len())
            & (self.stack.len() == to.stack.len())
            & self
                .locals
                .iter()
                .zip(to.locals.iter())
                .all(|(s, o)| s.is_assignable_to(*o))
            & self
                .stack
                .iter()
                .zip(to.stack.iter())
                .all(|(s, o)| s.is_assignable_to(*o))
    }

    /// In case of category 2 type, pops both the top type and the actual type
    fn pop(&mut self, jvm: &Jvm) -> JvmResult<VerificationType> {
        let typ = self.stack.pop().ok_or_else(|| error!(jvm))?;
        if typ == VerificationType::Top {
            self.stack.pop().ok_or_else(|| error!(jvm))
        } else {
            verify!(jvm, !typ.category_2());
            Ok(typ)
        }
    }

    fn push(&mut self, typ: VerificationType) {
        self.stack.push(typ);
        if typ.category_2() {
            self.stack.push(VerificationType::Top);
        }
    }

    fn pop_type(&mut self, jvm: &Jvm, typ: &Type) -> JvmResult<()> {
        verify!(
            jvm,
            match (typ, self.stack.pop()) {
                (
                    Type::Boolean | Type::Byte | Type::Short | Type::Char | Type::Int,
                    Some(VerificationType::Integer),
                ) => true,
                (Type::Long, Some(VerificationType::Top)) => {
                    matches!(self.stack.pop(), Some(VerificationType::Long))
                }
                (Type::Float, Some(VerificationType::Float)) => true,
                (Type::Double, Some(VerificationType::Top)) => {
                    matches!(self.stack.pop(), Some(VerificationType::Double))
                }
                (Type::Ref(ref_type), Some(VerificationType::ObjectVariable(obj))) => {
                    obj.assignable_to(ref_type)
                }
                _ => false,
            }
        );
        Ok(())
    }

    fn load(&mut self, jvm: &Jvm, index: u16, typ: VerificationType) -> JvmResult<()> {
        let var = self.locals.get(index as usize).ok_or_else(|| error!(jvm))?;
        verify!(
            jvm,
            var.is_assignable_to(typ)
                & !(var.category_2()
                    & (self.locals.get(index as usize + 1) != Some(&VerificationType::Top)))
        );
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
        verify!(
            jvm,
            cat2_success && on_stack.is_assignable_to(typ) & (index < self.locals.len() as u16)
        );
        self.locals[index as usize] = on_stack;
        Ok(())
    }

    fn top_of_stack(&mut self, jvm: &Jvm, typ: VerificationType) -> JvmResult<()> {
        if typ.category_2() {
            verify!(
                jvm,
                (self.stack.len() > 1)
                    && self.stack[self.stack.len() - 1] == VerificationType::Top
                    && self.stack[self.stack.len() - 2].is_assignable_to(typ)
            );
        } else {
            verify!(
                jvm,
                !self.stack.is_empty() && self.stack[self.stack.len() - 1].is_assignable_to(typ)
            );
        }
        Ok(())
    }

    fn relative_jump(
        &mut self,
        jvm: &Jvm,
        stack_map_table: &BTreeMap<u16, StackMapFrame>,
        bytes: &[SyncUnsafeCell<u8>],
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
        self.jump(jvm, stack_map_table, target)
    }

    fn jump(
        &mut self,
        jvm: &Jvm,
        stack_map_table: &BTreeMap<u16, StackMapFrame>,
        target: u16,
    ) -> JvmResult<()> {
        if let Some(target_state) = stack_map_table.get(&target) {
            verify!(jvm, self.is_assignable_to(target_state));
            Ok(())
        } else {
            verify_failed!(jvm)
        }
    }
}

/// Requires class to be resolved (→ only called from Class::verify)
pub(crate) fn verify<'a: 'b, 'b>(jvm: &'b Jvm, class: ClassRef) -> JvmResult<()> {
    if let Some(super_class) = class.super_class {
        super_class.verify(jvm)?;
        // Check whether the super class is final (as described in spec) is redundant (has to already be checked during loading)
    } else {
        verify!(jvm, class.name.as_ref() == "java.lang.Object");
    }

    let const_pool = class.const_pool.read().unwrap();

    for method in class.methods.values() {
        if method.class() == class {
            verify_method(jvm, &const_pool, method)?;
        }
    }

    Ok(())
}

pub(crate) fn verify_method(jvm: &Jvm, const_pool: &ConstPool, method: &Method) -> JvmResult<()> {
    if !method
        .access_flags
        .intersects(AccessFlags::STATIC | AccessFlags::PRIVATE)
    {
        let mut class = method.class();
        while let Some(super_class) = class.super_class {
            class = super_class;
            if let Some(super_method) = super_class.methods.get(&method.nat) {
                if !super_method.access_flags.contains(AccessFlags::FINAL)
                    | method
                        .access_flags
                        .intersects(AccessFlags::STATIC | AccessFlags::PRIVATE)
                {
                    break;
                } else {
                    verify_failed!(jvm);
                }
            }
        }
    }

    let ptrs = jvm.heap.ptrs();
    match &*method.code.read().unwrap() {
        Code::None => {
            verify!(
                jvm,
                method
                    .access_flags
                    .intersects(AccessFlags::ABSTRACT | AccessFlags::NATIVE)
            )
        }
        Code::Bytecode(code) => {
            verify_bytecode(jvm, const_pool, method, code).inspect_err(|err| {
                // Include method in error
                if err.class(ptrs).name.deref() == "java.lang.VerifyError" {
                    let field = err
                        .class(ptrs)
                        .field("message", Type::Ref("java.lang.String".into()))
                        .unwrap();
                    let Value::Ref(Some(message)) = field.instance_get(ptrs, *err) else {
                        panic!()
                    };
                    let message = format!("{method}: {message:?}");
                    field.instance_set(ptrs, *err, Value::Ref(Some(str_to_java(jvm, &message))));
                }
            })?
        }
        Code::Native(_) => assert!(method.access_flags.contains(AccessFlags::NATIVE)),
    }

    Ok(())
}

/// Verification by type checking (verification by type inference is not planned)
fn verify_bytecode(
    jvm: &Jvm,
    const_pool: &ConstPool,
    method: &Method,
    code: &Bytecode,
) -> JvmResult<()> {
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
        verify!(jvm, locals.len() <= code.max_locals as usize);
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
        verify!(
            jvm,
            (frame.locals.len() <= code.max_locals as usize)
                & (frame.stack.len() <= code.max_stack as usize)
        );
        frame
            .locals
            .resize(code.max_locals as usize, VerificationType::Top);
    }

    if let Some((last_pc, _)) = stack_map_table.last_key_value() {
        verify!(jvm, *last_pc as usize <= bytes.len());
    }

    let mut type_states = stack_map_table.iter();
    let mut type_state = type_states.next().unwrap().1.clone();
    let mut next_explicit_type_state = type_states.next();
    // Whether the type state depends on the type state of the previous instruction (false after goto & returns)
    let mut connected_to_previous_block = true;
    let mut pc = 0;

    let pc = &mut pc;

    // Iterate over all instructions
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
                    Some(ConstPoolItem::String(_)) => {
                        type_state.push(VerificationType::ObjectVariable(
                            jvm.resolve_class_internal("java.lang.String")?,
                        ))
                    }
                    Some(ConstPoolItem::Class(class)) => {
                        type_state.push(VerificationType::ObjectVariable(class))
                    }
                    _ => verify_failed!(jvm),
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
                    _ => verify_failed!(jvm),
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
                    _ => verify_failed!(jvm),
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
                            element_type: Some(Type::Int),
                            ..
                        })
                    )
                {
                    verify_failed!(jvm);
                }
                type_state.push(VerificationType::Integer);
            }
            LALOAD => {
                if (type_state.pop(jvm)? != VerificationType::Integer)
                    | !matches!(
                        type_state.pop(jvm)?,
                        VerificationType::ObjectVariable(Class {
                            element_type: Some(Type::Long),
                            ..
                        })
                    )
                {
                    verify_failed!(jvm);
                }
                type_state.push(VerificationType::Long);
            }
            FALOAD => {
                if (type_state.pop(jvm)? != VerificationType::Integer)
                    | !matches!(
                        type_state.pop(jvm)?,
                        VerificationType::ObjectVariable(Class {
                            element_type: Some(Type::Float),
                            ..
                        })
                    )
                {
                    verify_failed!(jvm);
                }
                type_state.push(VerificationType::Float);
            }
            DALOAD => {
                if (type_state.pop(jvm)? != VerificationType::Integer)
                    | !matches!(
                        type_state.pop(jvm)?,
                        VerificationType::ObjectVariable(Class {
                            element_type: Some(Type::Double),
                            ..
                        })
                    )
                {
                    verify_failed!(jvm);
                }
                type_state.push(VerificationType::Double);
            }
            AALOAD => {
                if type_state.pop(jvm)? != VerificationType::Integer {
                    verify_failed!(jvm);
                }
                let component = match type_state.pop(jvm)? {
                    VerificationType::Null => VerificationType::Null,
                    VerificationType::ObjectVariable(class) => {
                        if let Some(Type::Ref(component)) = &class.element_type {
                            let class = jvm.resolve_class_internal(component)?;
                            VerificationType::ObjectVariable(class)
                        } else {
                            verify_failed!(jvm);
                        }
                    }
                    _ => {
                        verify_failed!(jvm);
                    }
                };
                type_state.push(component);
            }
            BALOAD => {
                verify!(
                    jvm,
                    (type_state.pop(jvm)? == VerificationType::Integer)
                        & small_array_type(type_state.pop(jvm)?)
                );
                type_state.push(VerificationType::Integer);
            }
            CALOAD => {
                verify!(
                    jvm,
                    (type_state.pop(jvm)? == VerificationType::Integer)
                        & matches!(
                            type_state.pop(jvm)?,
                            VerificationType::ObjectVariable(Class {
                                element_type: Some(Type::Char),
                                ..
                            })
                        )
                );
                type_state.push(VerificationType::Integer);
            }
            SALOAD => {
                verify!(
                    jvm,
                    (type_state.pop(jvm)? == VerificationType::Integer)
                        & matches!(
                            type_state.pop(jvm)?,
                            VerificationType::ObjectVariable(Class {
                                element_type: Some(Type::Short),
                                ..
                            })
                        )
                );
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
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Integer);
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Integer);
                verify!(
                    jvm,
                    matches!(
                        type_state.pop(jvm)?,
                        VerificationType::ObjectVariable(Class {
                            element_type: Some(Type::Int),
                            ..
                        })
                    )
                )
            }
            LASTORE => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Long);
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Integer);
                verify!(
                    jvm,
                    matches!(
                        type_state.pop(jvm)?,
                        VerificationType::ObjectVariable(Class {
                            element_type: Some(Type::Long),
                            ..
                        })
                    )
                );
            }
            FASTORE => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Float);
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Integer);
                verify!(
                    jvm,
                    matches!(
                        type_state.pop(jvm)?,
                        VerificationType::ObjectVariable(Class {
                            element_type: Some(Type::Float),
                            ..
                        })
                    )
                );
            }
            DASTORE => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Double);
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Integer);
                verify!(
                    jvm,
                    matches!(
                        type_state.pop(jvm)?,
                        VerificationType::ObjectVariable(Class {
                            element_type: Some(Type::Double),
                            ..
                        })
                    )
                );
            }
            AASTORE => {
                verify!(
                    jvm,
                    matches!(
                        (
                            type_state.pop(jvm)?,
                            type_state.pop(jvm)?,
                            type_state.pop(jvm)?
                        ),
                        (
                            VerificationType::ObjectVariable(_),
                            VerificationType::Integer,
                            VerificationType::ObjectVariable(Class {
                                element_type: Some(Type::Ref(_)),
                                ..
                            })
                        )
                    )
                );
            }
            BASTORE => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Integer);
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Integer);
                verify!(jvm, small_array_type(type_state.pop(jvm)?));
            }
            CASTORE => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Integer);
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Integer);
                verify!(
                    jvm,
                    matches!(
                        type_state.pop(jvm)?,
                        VerificationType::ObjectVariable(Class {
                            element_type: Some(Type::Char),
                            ..
                        })
                    )
                );
            }
            SASTORE => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Integer);
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Integer);
                verify!(
                    jvm,
                    matches!(
                        type_state.pop(jvm)?,
                        VerificationType::ObjectVariable(Class {
                            element_type: Some(Type::Short),
                            ..
                        })
                    )
                );
            }
            POP => {
                verify!(jvm, !type_state.pop(jvm)?.category_2());
            }
            POP2 => {
                verify!(
                    jvm,
                    type_state.pop(jvm)?.category_2() || !type_state.pop(jvm)?.category_2()
                );
            }
            DUP => {
                let typ = type_state.pop(jvm)?;
                verify!(jvm, !typ.category_2());
                type_state.push(typ);
                type_state.push(typ);
            }
            DUP_X1 => {
                let first = type_state.pop(jvm)?;
                let second = type_state.pop(jvm)?;
                verify!(jvm, !first.category_2());
                verify!(jvm, !second.category_2());
                type_state.stack.push(first);
                type_state.stack.push(second);
                type_state.stack.push(first);
            }
            DUP_X2 => {
                let first = type_state.pop(jvm)?;
                let (Some(second), Some(third)) = (type_state.stack.pop(), type_state.stack.pop())
                else {
                    verify_failed!(jvm);
                };
                verify!(jvm, third != VerificationType::Top);
                type_state.stack.push(first);
                type_state.stack.push(third);
                type_state.stack.push(second);
                type_state.stack.push(first);
            }
            DUP2 => {
                let (Some(first), Some(second)) = (type_state.stack.pop(), type_state.stack.pop())
                else {
                    verify_failed!(jvm);
                };
                verify!(jvm, second != VerificationType::Top);
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
                    verify_failed!(jvm);
                };
                verify!(jvm, second != VerificationType::Top);
                verify!(jvm, third != VerificationType::Top);
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
                    verify_failed!(jvm);
                };
                verify!(jvm, second != VerificationType::Top);
                verify!(jvm, fourth != VerificationType::Top);
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
                verify!(jvm, !first.category_2());
                verify!(jvm, !second.category_2());
                type_state.push(first);
                type_state.push(second);
            }
            IADD | ISUB | IMUL | IDIV | IREM | ISHL | ISHR | IUSHR | IAND | IOR | IXOR => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Integer);
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Integer);
                type_state.push(VerificationType::Integer)
            }
            LADD | LSUB | LMUL | LDIV | LREM | LSHL | LSHR | LUSHR | LAND | LOR | LXOR => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Long);
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Long);
                type_state.push(VerificationType::Long)
            }
            FADD | FSUB | FMUL | FDIV | FREM => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Float);
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Float);
                type_state.push(VerificationType::Float)
            }
            DADD | DSUB | DMUL | DDIV | DREM => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Double);
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Double);
                type_state.push(VerificationType::Double);
            }
            INEG | I2B | I2C | I2S => type_state.top_of_stack(jvm, VerificationType::Integer)?,
            LNEG => type_state.top_of_stack(jvm, VerificationType::Long)?,
            FNEG => type_state.top_of_stack(jvm, VerificationType::Float)?,
            DNEG => type_state.top_of_stack(jvm, VerificationType::Double)?,
            IINC => {
                let index = read_code_u8(jvm, bytes, pc)?;
                read_code_u8(jvm, bytes, pc)?;
                verify!(
                    jvm,
                    type_state.locals.get(index as usize) == Some(&VerificationType::Integer)
                );
            }
            I2F => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Integer);
                type_state.push(VerificationType::Float)
            }
            I2D => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Integer);
                type_state.push(VerificationType::Double);
            }
            I2L => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Integer);
                type_state.push(VerificationType::Long);
            }
            L2I => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Long);
                type_state.push(VerificationType::Integer);
            }
            L2F => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Long);
                type_state.push(VerificationType::Float);
            }
            L2D => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Long);
                type_state.push(VerificationType::Double);
            }
            F2I => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Float);
                type_state.push(VerificationType::Integer);
            }
            F2L => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Float);
                type_state.push(VerificationType::Long);
            }
            F2D => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Float);
                type_state.push(VerificationType::Double);
            }
            D2I => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Double);
                type_state.push(VerificationType::Integer);
            }
            D2L => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Double);
                type_state.push(VerificationType::Long);
            }
            D2F => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Double);
                type_state.push(VerificationType::Float);
            }
            LCMP => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Long);
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Long);
                type_state.push(VerificationType::Integer);
            }
            FCMPL | FCMPG => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Float);
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Float);
                type_state.push(VerificationType::Integer);
            }
            DCMPL | DCMPG => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Double);
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Double);
                type_state.push(VerificationType::Integer);
            }
            IFEQ | IFNE | IFLT | IFGE | IFGT | IFLE => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Integer);
                type_state.relative_jump(jvm, &stack_map_table, bytes, pc, false)?;
                connected_to_previous_block = false;
            }
            IF_ICMPEQ | IF_ICMPNE | IF_ICMPLT | IF_ICMPGE | IF_ICMPGT | IF_ICMPLE => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Integer);
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Integer);
                type_state.relative_jump(jvm, &stack_map_table, bytes, pc, false)?;
                connected_to_previous_block = false;
            }
            IF_ACMPEQ | IF_ACMPNE => {
                verify!(
                    jvm,
                    type_state
                        .pop(jvm)?
                        .is_assignable_to(VerificationType::Reference)
                );
                verify!(
                    jvm,
                    type_state
                        .pop(jvm)?
                        .is_assignable_to(VerificationType::Reference)
                );
                type_state.relative_jump(jvm, &stack_map_table, bytes, pc, false)?;
                connected_to_previous_block = false;
            }
            GOTO => {
                type_state.relative_jump(jvm, &stack_map_table, bytes, pc, false)?;
                connected_to_previous_block = false;
            }
            TABLESWITCH => {
                let base = *pc - 1;

                // Skip up to 3 byte padding
                *pc += 3;
                *pc &= 0b1111111111111000;

                let default = read_code_u32(jvm, bytes, pc)? as i32;
                let low = read_code_u32(jvm, bytes, pc)? as i32;
                let high = read_code_u32(jvm, bytes, pc)? as i32;
                verify!(jvm, low <= high);

                type_state.pop_type(jvm, &Type::Int)?;

                type_state.jump(jvm, &stack_map_table, (base as i32 + default) as u16)?;
                let mut prev = i32::MIN;
                for _ in low..=high {
                    let offset = read_code_u32(jvm, bytes, pc)? as i32;
                    type_state.jump(jvm, &stack_map_table, (base as i32 + offset) as u16)?;
                    verify!(jvm, offset > prev);
                    prev = offset;
                }
                connected_to_previous_block = false;
            }
            LOOKUPSWITCH => {
                let base = *pc - 1;

                // Skip up to 3 byte padding
                *pc += 3;
                *pc &= 0b1111111111111000;

                let default = read_code_u32(jvm, bytes, pc)? as i32;
                let npairs = read_code_u32(jvm, bytes, pc)? as i32;
                verify!(jvm, npairs >= 0);

                type_state.pop_type(jvm, &Type::Int)?;

                type_state.jump(jvm, &stack_map_table, (base as i32 + default) as u16)?;
                let mut prev = i32::MIN;
                for _ in 0..npairs {
                    let i_match = read_code_u32(jvm, bytes, pc)? as i32;
                    let offset = read_code_u32(jvm, bytes, pc)? as i32;
                    type_state.jump(jvm, &stack_map_table, (base as i32 + offset) as u16)?;
                    verify!(jvm, i_match >= prev);
                    prev = i_match;
                }
                connected_to_previous_block = false;
            }
            IRETURN => {
                verify!(
                    jvm,
                    matches!(
                        method.nat.typ.returns,
                        Some(Type::Boolean | Type::Byte | Type::Char | Type::Short | Type::Int)
                    )
                );
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Integer);
                connected_to_previous_block = false
            }
            LRETURN => {
                verify!(jvm, method.nat.typ.returns == Some(Type::Long));
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Long);
                connected_to_previous_block = false
            }
            FRETURN => {
                verify!(jvm, method.nat.typ.returns == Some(Type::Float));
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Float);
                connected_to_previous_block = false
            }
            DRETURN => {
                verify!(jvm, method.nat.typ.returns == Some(Type::Double));
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Double);
                connected_to_previous_block = false
            }
            RETURN => {
                verify!(jvm, method.nat.typ.returns.is_none());
                verify!(
                    jvm,
                    (method.nat.name.deref() != "<init>")
                        || !type_state
                            .locals
                            .contains(&VerificationType::UninitializedThis)
                );
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
                verify!(
                    jvm,
                    match type_state.pop(jvm)? {
                        VerificationType::ObjectVariable(class) => {
                            class.assignable_to(&field.class().name)
                        }
                        _ => false,
                    }
                );
                push_type(jvm, &mut type_state.stack, &field.nat.typ)?;
            }
            PUTFIELD => {
                let index = read_code_u16(jvm, bytes, pc)?;
                let field = const_pool.get_field(jvm, index)?;
                type_state.pop_type(jvm, &field.nat.typ)?;
                verify!(
                    jvm,
                    match type_state.pop(jvm)? {
                        VerificationType::ObjectVariable(class) => {
                            class.assignable_to(&field.class().name)
                        }
                        VerificationType::UninitializedThis => {
                            !((method.nat.name.as_ref() == "<init>")
                                & method.class().assignable_to(&field.class().name))
                        }
                        _ => false,
                    }
                );
            }
            INVOKEVIRTUAL => {
                let index = read_code_u16(jvm, bytes, pc)?;
                let (invoke_class, invoke_nat) = const_pool.get_virtual_method(jvm, index)?;
                verify!(
                    jvm,
                    !matches!(invoke_nat.name.as_ref(), "<init>" | "<clinit>")
                );
                for arg in invoke_nat.typ.args.iter().rev() {
                    verify!(jvm, type_state.pop(jvm)?.matches(arg));
                }
                verify!(
                    jvm,
                    type_state.pop(jvm)? == VerificationType::ObjectVariable(invoke_class)
                );
                if let Some(return_type) = &invoke_nat.typ.returns {
                    push_type(jvm, &mut type_state.stack, return_type)?
                }
            }
            INVOKESPECIAL => {
                let index = read_code_u16(jvm, bytes, pc)?;
                let (invoke_class, invoke_nat) = const_pool.get_virtual_method(jvm, index)?;
                verify!(jvm, invoke_nat.name.deref() != "<clinit>");
                if invoke_nat.name.deref() == "<init>" {
                    verify!(jvm, invoke_nat.typ.returns.is_none());
                    for arg in invoke_nat.typ.args.iter().rev() {
                        verify!(jvm, type_state.pop(jvm)?.matches(arg));
                    }
                    let uninit_type = type_state.pop(jvm)?;
                    let init_type = match uninit_type {
                        VerificationType::UninitializedThis => {
                            VerificationType::ObjectVariable(method.class())
                        }
                        VerificationType::UninitializedVariable { offset } => {
                            verify!(
                                jvm,
                                unsafe {
                                    // SAFETY: Code does not get modified during verification
                                    *code.bytes[offset as usize].get()
                                } == NEW
                            );
                            VerificationType::ObjectVariable(invoke_class)
                        }
                        _ => verify_failed!(jvm),
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
                        verify!(jvm, type_state.pop(jvm)?.matches(arg));
                    }
                    let object = type_state.pop(jvm)?;
                    verify!(jvm, invoke_class == method.class());
                    verify!(jvm, object.matches(&Type::Ref(invoke_class.name.clone())));
                    if let Some(return_type) = &invoke_nat.typ.returns {
                        push_type(jvm, &mut type_state.stack, return_type)?
                    }
                }
            }
            INVOKESTATIC => {
                let index = read_code_u16(jvm, bytes, pc)?;
                let invoke_method = const_pool.get_static_method(jvm, index)?;
                verify!(
                    jvm,
                    !matches!(invoke_method.nat.name.as_ref(), "<init>" | "<clinit>")
                );
                for arg in invoke_method.nat.typ.args.iter().rev() {
                    verify!(jvm, type_state.pop(jvm)?.matches(arg));
                }
                if let Some(return_type) = &invoke_method.nat.typ.returns {
                    push_type(jvm, &mut type_state.stack, return_type)?
                }
            }
            INVOKEINTERFACE => {
                let index = read_code_u16(jvm, bytes, pc)?;
                let (interface, invoke_nat) = const_pool.get_interface_method(jvm, index)?;
                let count = read_code_u8(jvm, bytes, pc)?;
                let zero = read_code_u8(jvm, bytes, pc)?;
                verify!(
                    jvm,
                    (zero == 0) & !matches!(invoke_nat.name.as_ref(), "<init>" | "<clinit>")
                );
                let stack_in = type_state.stack.len();
                for arg in invoke_nat.typ.args.iter().rev() {
                    verify!(jvm, type_state.pop(jvm)?.matches(arg));
                }
                verify!(jvm, type_state.stack.len() + count as usize == stack_in);
                if let VerificationType::ObjectVariable(invoke_class) = type_state.pop(jvm)? {
                    verify!(jvm, invoke_class.assignable_to(&interface.name));
                } else {
                    verify_failed!(jvm)
                }
                if let Some(return_type) = &invoke_nat.typ.returns {
                    push_type(jvm, &mut type_state.stack, return_type)?
                }
            }
            NEW => {
                let uninit = VerificationType::UninitializedVariable { offset: *pc - 1 };
                let index = read_code_u16(jvm, bytes, pc)?;
                let class = const_pool.get_class(jvm, index)?;
                verify!(jvm, class.element_type.is_none());
                verify!(jvm, !type_state.stack.contains(&uninit));
                type_state.push(uninit);
            }
            NEWARRAY => {
                type_state.pop_type(jvm, &Type::Int)?;
                let read = read_code_u8(jvm, bytes, pc)?;
                let component = match read {
                    4 => Type::Boolean,
                    5 => Type::Char,
                    6 => Type::Float,
                    7 => Type::Double,
                    8 => Type::Byte,
                    9 => Type::Short,
                    10 => Type::Int,
                    11 => Type::Long,
                    _ => verify_failed!(jvm),
                };
                let class = jvm.resolve_array_of(&component)?;
                type_state.push(VerificationType::ObjectVariable(class));
            }
            ANEWARRAY => {
                verify!(jvm, type_state.pop(jvm)? == VerificationType::Integer);
                let index = read_code_u16(jvm, bytes, pc)?;
                let component = const_pool.get_class(jvm, index)?;
                let typ = jvm.resolve_array_of(&component.typ())?;
                type_state.push(VerificationType::ObjectVariable(typ));
            }
            ARRAYLENGTH => {
                verify!(
                    jvm,
                    matches!(
                        type_state.pop(jvm)?,
                        VerificationType::ObjectVariable(Class {
                            element_type: Some(_),
                            ..
                        }),
                    )
                );
                type_state.push(VerificationType::Integer);
            }
            ATHROW => {
                match type_state.pop(jvm)? {
                    VerificationType::ObjectVariable(thrown)
                        if thrown.assignable_to("java.lang.Throwable") => {}
                    _ => verify_failed!(jvm),
                }
                connected_to_previous_block = false
            }
            CHECKCAST => {
                let index = read_code_u16(jvm, bytes, pc)?;
                let casted = const_pool.get_class(jvm, index)?;
                match type_state.pop(jvm)? {
                    VerificationType::ObjectVariable(_) => {}
                    _ => verify_failed!(jvm),
                }
                type_state.push(VerificationType::ObjectVariable(casted));
            }
            INSTANCEOF => {
                let index = read_code_u16(jvm, bytes, pc)?;
                const_pool.get_class(jvm, index)?;
                match type_state.pop(jvm)? {
                    // Spec bug: Interfaces in reality allowed
                    VerificationType::ObjectVariable(_) => {}
                    _ => verify_failed!(jvm),
                }
                type_state.push(VerificationType::Integer);
            }
            WIDE => {
                let op = read_code_u8(jvm, bytes, pc)?;
                let index = read_code_u16(jvm, bytes, pc)?;
                match op {
                    ILOAD => type_state.load(jvm, index, VerificationType::Integer)?,
                    LLOAD => {
                        type_state.load(jvm, index, VerificationType::Long)?;
                    }
                    FLOAD => type_state.load(jvm, index, VerificationType::Float)?,
                    DLOAD => {
                        type_state.load(jvm, index, VerificationType::Double)?;
                    }
                    ALOAD => type_state.load(jvm, index, VerificationType::Reference)?,
                    ISTORE => type_state.store(jvm, index, VerificationType::Integer)?,
                    LSTORE => type_state.store(jvm, index, VerificationType::Long)?,
                    FSTORE => type_state.store(jvm, index, VerificationType::Float)?,
                    DSTORE => type_state.store(jvm, index, VerificationType::Double)?,
                    ASTORE => type_state.store(jvm, index, VerificationType::Reference)?,
                    IINC => {
                        read_code_u16(jvm, bytes, pc)?;
                        verify!(
                            jvm,
                            type_state.locals.get(index as usize)
                                == Some(&VerificationType::Integer)
                        );
                    }
                    _ => verify_failed!(jvm),
                }
            }
            MONITORENTER | MONITOREXIT => {
                verify!(
                    jvm,
                    type_state
                        .pop(jvm)?
                        .is_assignable_to(VerificationType::Reference)
                );
            }
            MULTIANEWARRAY => {
                let index = read_code_u16(jvm, bytes, pc)?;
                let dimensions = read_code_u8(jvm, bytes, pc)?;
                let class = const_pool.get_class(jvm, index)?;
                let mut iter_class = class;
                let mut class_dimensions = 0;
                while let Some(elem) = &iter_class.element_type {
                    class_dimensions += 1;
                    if let Type::Ref(elem) = elem {
                        iter_class = jvm.resolve_class_internal(elem)?;
                    } else {
                        break;
                    }
                }
                verify!(jvm, dimensions > 0);
                verify!(jvm, dimensions <= class_dimensions);
                for _ in 0..dimensions {
                    type_state.pop_type(jvm, &Type::Int)?;
                }
                type_state.push(VerificationType::ObjectVariable(class));
            }
            IFNULL | IFNONNULL => {
                verify!(
                    jvm,
                    matches!(type_state.pop(jvm)?, VerificationType::ObjectVariable(_))
                );
                type_state.relative_jump(jvm, &stack_map_table, bytes, pc, false)?;
                connected_to_previous_block = false;
            }
            GOTO_W => {
                type_state.relative_jump(jvm, &stack_map_table, bytes, pc, true)?;
                connected_to_previous_block = false;
            }
            other => todo!("bytecode {other}"),
        }

        verify!(jvm, type_state.stack.len() <= code.max_stack as usize);
        // If the next specified stack map frame is reached, check if it matches the inferred one
        if let Some((type_state_pc, explicit_type_state)) = next_explicit_type_state {
            match (*pc).cmp(type_state_pc) {
                std::cmp::Ordering::Less => {} // Not at frame yet
                std::cmp::Ordering::Equal => {
                    verify!(
                        jvm,
                        !connected_to_previous_block
                            | type_state.is_assignable_to(explicit_type_state)
                    );
                    type_state = explicit_type_state.clone();
                    next_explicit_type_state = type_states.next();
                }
                std::cmp::Ordering::Greater => verify_failed!(jvm),
            }
        }
    }
    verify!(jvm, !connected_to_previous_block);
    Ok(())
}

pub(crate) fn push_type(jvm: &Jvm, types: &mut Vec<VerificationType>, typ: &Type) -> JvmResult<()> {
    match typ {
        Type::Boolean | Type::Char | Type::Byte | Type::Short | Type::Int => {
            types.push(VerificationType::Integer)
        }
        Type::Float => types.push(VerificationType::Float),
        Type::Double => {
            types.push(VerificationType::Double);
            types.push(VerificationType::Top);
        }
        Type::Long => {
            types.push(VerificationType::Long);
            types.push(VerificationType::Top);
        }
        Type::Ref(class) => types.push(VerificationType::ObjectVariable(
            jvm.resolve_class_internal(class)?,
        )),
    }
    Ok(())
}

fn small_array_type(typ: VerificationType) -> bool {
    match typ {
        VerificationType::ObjectVariable(class) => {
            matches!(class.element_type, Some(Type::Byte | Type::Boolean))
        }
        VerificationType::Null => true,
        _ => false,
    }
}

fn read_code_u8(jvm: &Jvm, bytes: &[SyncUnsafeCell<u8>], pc: &mut u16) -> JvmResult<u8> {
    *pc += 1;
    bytes
        .get(*pc as usize - 1)
        .map(|b| unsafe {
            // SAFETY: Code does not get modified during verification
            *b.get()
        })
        .ok_or_else(|| error!(jvm))
}

fn read_code_u16(jvm: &Jvm, bytes: &[SyncUnsafeCell<u8>], pc: &mut u16) -> JvmResult<u16> {
    Ok(((read_code_u8(jvm, bytes, pc)? as u16) << 8) + read_code_u8(jvm, bytes, pc)? as u16)
}

fn read_code_u32(jvm: &Jvm, bytes: &[SyncUnsafeCell<u8>], pc: &mut u16) -> JvmResult<u32> {
    Ok(((read_code_u16(jvm, bytes, pc)? as u32) << 8) + read_code_u16(jvm, bytes, pc)? as u32)
}
