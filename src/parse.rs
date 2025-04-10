use alloc::collections::BTreeMap;
use alloc::sync::Arc;
use core::cell::SyncUnsafeCell;
use hashbrown::HashMap;

use crate::{
    const_pool::{ConstPool, ConstPoolItem},
    default,
    field::{Field, FieldNaT},
    jvm::{throwable, Jvm, JvmResult},
    method::{
        Bytecode, Code, ExceptionHandler, LocalVariable, LocalVariableType, Method,
        MethodDescriptor, MethodNaT,
    },
    typ::Type,
    verification::{StackMapFrame, VerificationType},
    AccessFlags,
};

macro_rules! error {
    ($jvm:expr) => {
        throwable(
            $jvm,
            "ClassFormatError",
            &*format!("class format error {}", line!()),
        )
    };
}

macro_rules! enforce {
    ($jvm:expr, $expr:expr) => {
        if !$expr {
            return Err(error!($jvm));
        }
    };
}

#[inline]
fn read_u8(jvm: &Jvm, input: &mut &[u8]) -> JvmResult<u8> {
    if let Some(u8) = input.first() {
        *input = &input[1..];
        Ok(*u8)
    } else {
        Err(error!(jvm))
    }
}

fn read_u16(jvm: &Jvm, input: &mut &[u8]) -> JvmResult<u16> {
    let high = read_u8(jvm, input)?;
    let low = read_u8(jvm, input)?;
    Ok(((high as u16) << 8) + low as u16)
}

fn read_u32(jvm: &Jvm, input: &mut &[u8]) -> JvmResult<u32> {
    let high = read_u16(jvm, input)?;
    let low = read_u16(jvm, input)?;
    Ok(((high as u32) << 16) + low as u32)
}

fn read_u64(jvm: &Jvm, input: &mut &[u8]) -> JvmResult<u64> {
    let high = read_u32(jvm, input)?;
    let low = read_u32(jvm, input)?;
    Ok(((high as u64) << 32) + low as u64)
}

fn read_magic(jvm: &Jvm, input: &mut &[u8]) -> JvmResult<()> {
    enforce!(jvm, read_u32(jvm, input)? == 0xCAFEBABE);
    Ok(())
}

fn read_const_pool_item(input: &mut &[u8], jvm: &Jvm) -> JvmResult<ConstPoolItem> {
    use ConstPoolItem::*;
    match read_u8(jvm, input)? {
        1 => {
            let length = read_u16(jvm, input)?;
            let mut bytes = Vec::with_capacity(length as usize);
            for _ in 0..length {
                bytes.push(read_u8(jvm, input)?);
            }
            let string = cesu8::from_java_cesu8(&bytes).map_err(|_| error!(jvm))?;
            Ok(Utf8(string.into()))
        }
        3 => Ok(Integer(read_u32(jvm, input)? as i32)),
        4 => Ok(Float(f32::from_bits(read_u32(jvm, input)?))),
        5 => Ok(Long(read_u64(jvm, input)? as i64)),
        6 => Ok(Double(f64::from_bits(read_u64(jvm, input)?))),
        7 => Ok(RawClass(read_u16(jvm, input)?)),
        8 => Ok(RawString(read_u16(jvm, input)?)),
        9 => Ok(FieldRef {
            class: read_u16(jvm, input)?,
            nat: read_u16(jvm, input)?,
        }),
        10 => Ok(MethodRef {
            class: read_u16(jvm, input)?,
            nat: read_u16(jvm, input)?,
        }),
        11 => Ok(InterfaceMethodRef {
            class: read_u16(jvm, input)?,
            nat: read_u16(jvm, input)?,
        }),
        12 => Ok(NameAndType {
            name: read_u16(jvm, input)?,
            descriptor: read_u16(jvm, input)?,
        }),
        _ => Err(error!(jvm)),
    }
}

fn read_const_pool(input: &mut &[u8], jvm: &Jvm) -> JvmResult<ConstPool> {
    let length = read_u16(jvm, input)? as usize;
    let mut items = Vec::with_capacity(length);
    items.push(ConstPoolItem::PlaceholderAfterLongOrDoubleEntryOrForEntryZero);
    while items.len() < length {
        items.push(read_const_pool_item(input, jvm)?);
        if matches!(
            items.last().unwrap(),
            ConstPoolItem::Long(_) | ConstPoolItem::Double(_)
        ) {
            items.push(ConstPoolItem::PlaceholderAfterLongOrDoubleEntryOrForEntryZero)
        }
    }
    // Const pool validity is part of format checking
    for item in items.iter() {
        enforce!(
            jvm,
            match item {
                ConstPoolItem::RawClass(index) => {
                    matches!(items.get(*index as usize), Some(ConstPoolItem::Utf8(..)))
                }
                ConstPoolItem::FieldRef { class, nat }
                | ConstPoolItem::MethodRef { class, nat }
                | ConstPoolItem::InterfaceMethodRef { class, nat } => matches!(
                    (items.get(*class as usize), items.get(*nat as usize)),
                    (
                        Some(ConstPoolItem::RawClass(..)),
                        Some(ConstPoolItem::NameAndType { .. })
                    )
                ),
                ConstPoolItem::NameAndType { name, descriptor } => matches!(
                    (items.get(*name as usize), items.get(*descriptor as usize)),
                    (Some(ConstPoolItem::Utf8(..)), Some(ConstPoolItem::Utf8(..)))
                ),
                ConstPoolItem::RawString(index) => {
                    matches!(items.get(*index as usize), Some(ConstPoolItem::Utf8(..)))
                }
                _ => true,
            }
        )
    }

    Ok(ConstPool { items })
}

fn read_interfaces(jvm: &Jvm, input: &mut &[u8], const_pool: &ConstPool) -> JvmResult<Vec<String>> {
    let length = read_u16(jvm, input)?;
    let mut vec = Vec::with_capacity(length as usize);
    for _ in 0..length {
        vec.push(const_pool.get_unresolved_class(jvm, read_u16(jvm, input)?)?);
    }
    Ok(vec)
}

fn read_attribute<'a>(
    jvm: &Jvm,
    input: &mut &'a [u8],
    const_pool: &ConstPool,
) -> JvmResult<(Arc<str>, &'a [u8])> {
    let name = const_pool.get_utf8(jvm, read_u16(jvm, input)?)?;
    let attr_length = read_u32(jvm, input)?;
    enforce!(jvm, attr_length as usize <= input.len());
    let attr_bytes = &input[0..attr_length as usize];
    *input = &input[attr_length as usize..];
    Ok((name, attr_bytes))
}

pub(crate) fn parse_typ_descriptor(
    jvm: &Jvm,
    descriptor: &str,
    start: usize,
) -> JvmResult<(Type, usize)> {
    let str = &descriptor[start..];
    if let Some(typ) = match str.chars().next() {
        Some('Z') => Some(Type::Boolean),
        Some('B') => Some(Type::Byte),
        Some('C') => Some(Type::Char),
        Some('S') => Some(Type::Short),
        Some('D') => Some(Type::Double),
        Some('F') => Some(Type::Float),
        Some('I') => Some(Type::Int),
        Some('J') => Some(Type::Long),
        _ => None,
    } {
        // Primitive
        Ok((typ, start + 1))
    } else if let Some(stripped) = str.strip_prefix('L') {
        // Class
        let Some((class_name, _)) = stripped.split_once(';') else {
            return Err(error!(jvm));
        };
        let class_name = class_name.replace('/', ".");
        let start = start + 1 + class_name.len() + 1;
        let typ = Type::Ref(class_name.into());
        Ok((typ, start))
    } else if str.starts_with('[') {
        // Array
        let mut dimensions = 0;
        while descriptor[start + dimensions..].starts_with('[') {
            dimensions += 1
        }
        enforce!(jvm, dimensions <= 255);
        let (component, end) = parse_typ_descriptor(jvm, descriptor, start + dimensions)?;
        let name = format!("{component}{}", "[]".repeat(dimensions));
        let typ = Type::Ref(name.into());
        Ok((typ, end))
    } else {
        Err(error!(jvm))
    }
}

fn read_field(input: &mut &[u8], constant_pool: &ConstPool, jvm: &Jvm) -> JvmResult<Field> {
    let access_flags = AccessFlags::from_bits_truncate(read_u16(jvm, input)?);
    enforce!(
        jvm,
        access_flags
            .intersection(AccessFlags::PUBLIC | AccessFlags::PRIVATE | AccessFlags::PROTECTED)
            .bits
            .count_ones()
            <= 1
    );
    enforce!(
        jvm,
        !access_flags.contains(AccessFlags::FINAL | AccessFlags::VOLATILE)
    );

    let name = constant_pool.get_utf8(jvm, read_u16(jvm, input)?)?;

    let descriptor = constant_pool.get_utf8(jvm, read_u16(jvm, input)?)?;
    let (typ, end) = parse_typ_descriptor(jvm, &descriptor, 0)?;
    enforce!(jvm, end >= descriptor.len());

    let mut const_value_index = None;
    let attributes_count = read_u16(jvm, input)?;
    for _ in 0..attributes_count {
        let (name, data) = read_attribute(jvm, input, constant_pool)?;
        if access_flags.contains(AccessFlags::STATIC) && (name.as_ref() == "ConstantValue") {
            enforce!(jvm, const_value_index.is_none() & (data.len() == 2));
            const_value_index = Some(((data[0] as u16) << 8) + data[1] as u16)
        }
    }

    Ok(Field {
        nat: FieldNaT { name, typ },
        access_flags,
        // The correct layout is set in read_fields after field disordering
        byte_offset: 0,
        const_value_index,
        class: default(),
    })
}

fn read_fields(input: &mut &[u8], constant_pool: &ConstPool, jvm: &Jvm) -> JvmResult<Vec<Field>> {
    // Read fields
    let length = read_u16(jvm, input)?;
    let mut fields = Vec::new();
    for _ in 0..length {
        let field = read_field(input, constant_pool, jvm)?;
        fields.push(field);
    }
    Ok(fields)
}

fn read_verification_type(
    jvm: &Jvm,
    input: &mut &[u8],
    const_pool: &ConstPool,
) -> JvmResult<VerificationType> {
    Ok(match read_u8(jvm, input)? {
        0 => VerificationType::Top,
        1 => VerificationType::Integer,
        2 => VerificationType::Float,
        3 => VerificationType::Double,
        4 => VerificationType::Long,
        5 => VerificationType::Null,
        6 => VerificationType::UninitializedThis,
        7 => VerificationType::ObjectVariable(const_pool.get_class(jvm, read_u16(jvm, input)?)?),
        8 => VerificationType::UninitializedVariable {
            offset: read_u16(jvm, input)?,
        },
        _ => return Err(error!(jvm)),
    })
}

/// This is called during varification, as it is only needed then and because the const pool has been resolved by then.
pub(crate) fn read_stack_map_table(
    jvm: &Jvm,
    input: Option<&[u8]>,
    const_pool: &ConstPool,
    initial_locals: Vec<VerificationType>,
) -> JvmResult<BTreeMap<u16, StackMapFrame>> {
    let mut frames = BTreeMap::new();
    let mut pc = 0;
    frames.insert(
        pc,
        StackMapFrame {
            stack: [].into(),
            locals: initial_locals,
        },
    );
    let Some(mut input) = input else {
        // Implicit stack map table
        return Ok(frames);
    };
    let input = &mut input;
    let length = read_u16(jvm, input)?;
    for _ in 0..length {
        let prev_frame = frames.iter().next_back().unwrap().1;
        let frame_type = read_u8(jvm, input)?;
        let (offset, current_frame) = if frame_type < 64 {
            // Same
            (
                frame_type as u16,
                StackMapFrame {
                    locals: prev_frame.locals.clone(),
                    stack: vec![],
                },
            )
        } else if frame_type < 128 {
            // Same locals 1 stack item
            (
                frame_type as u16 - 64,
                StackMapFrame {
                    locals: prev_frame.locals.clone(),
                    stack: vec![read_verification_type(jvm, input, const_pool)?],
                },
            )
        } else if frame_type == 247 {
            // Same locals 1 stack item extended
            (
                read_u16(jvm, input)?,
                StackMapFrame {
                    locals: prev_frame.locals.clone(),
                    stack: vec![read_verification_type(jvm, input, const_pool)?],
                },
            )
        } else if (frame_type > 247) & (frame_type < 251) {
            // Chop
            let k = 251 - frame_type;
            enforce!(jvm, prev_frame.locals.len() >= k as usize);
            (
                read_u16(jvm, input)?,
                StackMapFrame {
                    locals: prev_frame.locals[0..prev_frame.locals.len() - k as usize].into(),
                    stack: [].into(),
                },
            )
        } else if frame_type == 251 {
            // Same extended
            (read_u16(jvm, input)?, prev_frame.clone())
        } else if (frame_type > 251) & (frame_type < 255) {
            // Append
            let k = frame_type - 251;
            (
                read_u16(jvm, input)?,
                StackMapFrame {
                    locals: {
                        let mut locals = prev_frame.locals.clone();
                        for _ in 0..k {
                            let typ = read_verification_type(jvm, input, const_pool)?;
                            locals.push(typ);
                            if typ.category_2() {
                                locals.push(VerificationType::Top)
                            }
                        }
                        locals
                    },
                    stack: [].into(),
                },
            )
        } else if frame_type == 255 {
            // Full
            let offset_delta = read_u16(jvm, input)?;
            let num_locals = read_u16(jvm, input)?;
            let mut locals = Vec::with_capacity(num_locals as usize);
            for _ in 0..num_locals {
                let typ = read_verification_type(jvm, input, const_pool)?;
                locals.push(typ);
                if typ.category_2() {
                    locals.push(VerificationType::Top);
                }
            }
            let num_stack = read_u16(jvm, input)?;
            let mut stack = Vec::with_capacity(num_stack as usize);
            for _ in 0..num_stack {
                stack.push(read_verification_type(jvm, input, const_pool)?)
            }
            (offset_delta, StackMapFrame { locals, stack })
        } else {
            return Err(error!(jvm));
        };
        pc += offset;
        frames.insert(pc, current_frame);
        // This is not applied if the previous frame was the initial (implicit) frame
        pc += 1;
    }
    enforce!(jvm, input.is_empty());
    Ok(frames)
}

fn read_code(jvm: &Jvm, mut input: &[u8], constant_pool: &ConstPool) -> JvmResult<Bytecode> {
    let input = &mut input;
    let max_stack = read_u16(jvm, input)?;
    let max_locals = read_u16(jvm, input)?;

    let code_length = read_u32(jvm, input)?;
    let mut bytes = Vec::with_capacity(code_length as usize);
    for _ in 0..code_length {
        bytes.push(read_u8(jvm, input)?);
    }

    let mut exception_table = Vec::new();
    let exception_table_length = read_u16(jvm, input)?;
    for _ in 0..exception_table_length {
        exception_table.push(ExceptionHandler {
            start_pc: read_u16(jvm, input)?,
            end_pc: read_u16(jvm, input)?,
            handler_pc: read_u16(jvm, input)?,
            catch_type: read_u16(jvm, input)?,
        });
    }

    let attributes_count = read_u16(jvm, input)?;
    let mut stack_map_table = None;
    let mut line_number_table = BTreeMap::new();
    let mut local_variables = Vec::new();
    let mut local_variable_types = Vec::new();
    for _ in 0..attributes_count {
        let (name, mut bytes) = read_attribute(jvm, input, constant_pool)?;
        if &*name == "StackMapTable" {
            enforce!(jvm, stack_map_table.is_none());
            stack_map_table = Some(bytes.into());
        }
        if &*name == "LineNumberTable" {
            for _ in 0..read_u16(jvm, &mut bytes)? {
                let start_pc = read_u16(jvm, &mut bytes)?;
                enforce!(jvm, (start_pc as u32) < code_length);
                let line_number = read_u16(jvm, &mut bytes)?;
                line_number_table.insert(start_pc, line_number);
            }
        }
        if &*name == "LocalVariableTable" {
            for _ in 0..read_u16(jvm, &mut bytes)? {
                let start_pc = read_u16(jvm, &mut bytes)?;
                let len = read_u16(jvm, &mut bytes)?;
                enforce!(jvm, (start_pc as u32) + (len as u32) < code_length);
                let name_index = read_u16(jvm, &mut bytes)?;
                let type_index = read_u16(jvm, &mut bytes)?;
                let locals_index = read_u16(jvm, &mut bytes)?;

                local_variables.push(LocalVariable {
                    start_pc,
                    end_pc: start_pc + len,
                    name_index,
                    type_index,
                    locals_index,
                });
            }
        }
        if &*name == "LocalVariableTypeTable" {
            for _ in 0..read_u16(jvm, &mut bytes)? {
                let start_pc = read_u16(jvm, &mut bytes)?;
                let len = read_u16(jvm, &mut bytes)?;
                enforce!(jvm, (start_pc as u32) + (len as u32) < code_length);
                let name_index = read_u16(jvm, &mut bytes)?;
                let signature_index = read_u16(jvm, &mut bytes)?;
                let locals_index = read_u16(jvm, &mut bytes)?;

                local_variable_types.push(LocalVariableType {
                    start_pc,
                    end_pc: start_pc + len,
                    name_index,
                    signature_index,
                    locals_index,
                });
            }
        }
    }

    enforce!(jvm, input.is_empty());

    Ok(Bytecode {
        max_stack,
        max_locals,
        bytes: bytes.into_iter().map(SyncUnsafeCell::new).collect(),
        stack_map_table,
        exception_table,
        line_number_table,
        local_variables,
        local_variable_types,
    })
}

pub(crate) fn parse_method_descriptor(jvm: &Jvm, descriptor: &str) -> JvmResult<MethodDescriptor> {
    enforce!(jvm, descriptor.starts_with('('));
    let mut start = 1;
    let mut args = Vec::new();
    while !descriptor[start..].starts_with(')') {
        let (arg, next_start) = parse_typ_descriptor(jvm, descriptor, start)?;
        args.push(arg);
        start = next_start;
    }
    start += 1;
    if &descriptor[start..] == "V" {
        Ok(MethodDescriptor {
            args,
            returns: None,
        })
    } else {
        let (return_type, start) = parse_typ_descriptor(jvm, descriptor, start)?;
        enforce!(jvm, start >= descriptor.len());
        Ok(MethodDescriptor {
            args,
            returns: Some(return_type),
        })
    }
}

fn read_method(input: &mut &[u8], constant_pool: &ConstPool, jvm: &Jvm) -> JvmResult<Method> {
    let access_flags = AccessFlags::from_bits_truncate(read_u16(jvm, input)?);
    enforce!(
        jvm,
        access_flags
            .intersection(AccessFlags::PUBLIC | AccessFlags::PRIVATE | AccessFlags::PROTECTED)
            .bits
            .count_ones()
            <= 1
    );
    if access_flags.contains(AccessFlags::ABSTRACT) {
        enforce!(
            jvm,
            !access_flags.intersects(
                AccessFlags::PRIVATE
                    | AccessFlags::STATIC
                    | AccessFlags::FINAL
                    | AccessFlags::SYNCHRONIZED
                    | AccessFlags::NATIVE
                    | AccessFlags::STRICT,
            )
        );
    }

    let name = constant_pool.get_utf8(jvm, read_u16(jvm, input)?)?;
    if *name == *"<init>" {
        enforce!(
            jvm,
            !access_flags.intersects(
                AccessFlags::STATIC
                    | AccessFlags::FINAL
                    | AccessFlags::NATIVE
                    | AccessFlags::BRIDGE
                    | AccessFlags::SYNTHETIC
                    | AccessFlags::SYNCHRONIZED
                    | AccessFlags::ABSTRACT
            )
        );
    }

    let descriptor = constant_pool.get_utf8(jvm, read_u16(jvm, input)?)?;
    let descriptor = parse_method_descriptor(jvm, &descriptor)?;

    let mut code = Code::None;
    let attributes_count = read_u16(jvm, input)?;
    for _ in 0..attributes_count {
        let (attr_name, bytes) = read_attribute(jvm, input, constant_pool)?;
        if attr_name.as_ref() == "Code" {
            if let Code::None = code {
                code = Code::Bytecode(read_code(jvm, bytes, constant_pool)?);
            } else {
                return Err(error!(jvm));
            }
        }
    }

    if let Code::None = &code {
        enforce!(
            jvm,
            access_flags.intersects(AccessFlags::ABSTRACT | AccessFlags::NATIVE)
        );
    }

    Ok(Method {
        nat: MethodNaT {
            name,
            typ: descriptor,
        },
        access_flags,
        class: default(),
        code: code.into(),
    })
}

fn read_methods(
    input: &mut &[u8],
    constant_pool: &ConstPool,
    jvm: &Jvm,
) -> JvmResult<HashMap<MethodNaT, Method>> {
    let length = read_u16(jvm, input)?;
    let mut methods = HashMap::with_capacity(length as usize);
    for _ in 0..length {
        let method = read_method(input, constant_pool, jvm)?;
        for existing_nat in methods.keys() {
            enforce!(jvm, existing_nat != &method.nat);
        }
        methods.insert(method.nat.clone(), method);
    }
    Ok(methods)
}

pub(crate) fn read_class_file(mut input: &[u8], jvm: &Jvm) -> JvmResult<ClassDescriptor> {
    let input = &mut input;

    read_magic(jvm, input)?;
    let _minor_version = read_u16(jvm, input)?;
    let major_version = read_u16(jvm, input)?;

    if major_version < 50 {
        return Err(throwable(
            jvm,
            "UnsupportedClassVersionError",
            &*format!("{major_version}"),
        ));
    }

    let const_pool = read_const_pool(input, jvm)?;

    let access_flags = AccessFlags::from_bits_truncate(read_u16(jvm, input)?);

    if access_flags.contains(AccessFlags::INTERFACE) {
        enforce!(
            jvm,
            !access_flags.intersects(AccessFlags::FINAL | AccessFlags::ENUM)
        );
        enforce!(jvm, access_flags.contains(AccessFlags::ABSTRACT));
    } else {
        enforce!(jvm, !access_flags.contains(AccessFlags::ANNOTATION));
        enforce!(
            jvm,
            !access_flags.contains(AccessFlags::FINAL | AccessFlags::ABSTRACT)
        );
    }

    let this_class = const_pool
        .get_unresolved_class(jvm, read_u16(jvm, input)?)?
        .into();
    let super_class = {
        let index = read_u16(jvm, input)?;
        if index > 0 {
            Some(const_pool.get_unresolved_class(jvm, index)?.into())
        } else {
            None
        }
    };

    let interfaces = read_interfaces(jvm, input, &const_pool)?;

    let fields = read_fields(input, &const_pool, jvm)?;
    if access_flags.contains(AccessFlags::INTERFACE) {
        for field in &fields {
            enforce!(
                jvm,
                field
                    .access_flags
                    .contains(AccessFlags::FINAL | AccessFlags::STATIC)
                    | field.access_flags.intersects(
                        AccessFlags::PUBLIC
                            | AccessFlags::PRIVATE
                            | AccessFlags::PROTECTED
                            | AccessFlags::VOLATILE
                            | AccessFlags::TRANSIENT
                            | AccessFlags::ENUM,
                    )
            );
        }
    }

    let methods = read_methods(input, &const_pool, jvm)?;
    if access_flags.contains(AccessFlags::INTERFACE) {
        for method in methods.values() {
            enforce!(
                jvm,
                !method.access_flags.intersects(
                    AccessFlags::PROTECTED
                        | AccessFlags::FINAL
                        | AccessFlags::SYNCHRONIZED
                        | AccessFlags::NATIVE,
                )
            );
        }
    }

    let mut source_file_name = None;
    let attributes_count = read_u16(jvm, input)?;
    for _ in 0..attributes_count {
        let (attr_name, attr_data) = read_attribute(jvm, input, &const_pool)?;
        if &*attr_name == "SourceFile" {
            enforce!(jvm, source_file_name.is_none());
            enforce!(jvm, attr_data.len() == 2);
            let index = u16::from_be_bytes([attr_data[0], attr_data[1]]);
            source_file_name = Some(const_pool.get_utf8(jvm, index)?);
        }
    }

    enforce!(jvm, input.is_empty());

    Ok(ClassDescriptor {
        const_pool,
        access_flags,
        name: this_class,
        super_class,
        source_file_name,
        interfaces,
        fields,
        methods,
    })
}

/// Represents a class with not yet resolved superclass & interfaces
pub(crate) struct ClassDescriptor {
    pub(crate) const_pool: ConstPool,
    pub(crate) access_flags: AccessFlags,
    pub(crate) name: Arc<str>,
    pub(crate) super_class: Option<Arc<str>>,
    pub(crate) source_file_name: Option<Arc<str>>,
    pub(crate) interfaces: Vec<String>,
    pub(crate) fields: Vec<Field>,
    pub(crate) methods: HashMap<MethodNaT, Method>,
}
