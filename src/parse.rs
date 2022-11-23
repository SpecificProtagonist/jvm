use std::{
    collections::{BTreeMap, HashMap},
    default::default,
    sync::Arc,
};

use crate::{
    const_pool::{ConstPool, ConstPoolItem},
    field::{Field, FieldNaT},
    jvm::exception,
    jvm::JVMResult,
    jvm::Jvm,
    method::Code,
    method::ExceptionHandler,
    method::Method,
    method::MethodDescriptor,
    method::MethodNaT,
    object::Object,
    typ::Typ,
    verification::{StackMapFrame, VerificationType},
    AccessFlags,
};

#[inline]
fn read_u8<'a>(jvm: &Jvm, input: &mut &'a [u8]) -> JVMResult<u8> {
    if let Some(u8) = input.first() {
        *input = &input[1..];
        Ok(*u8)
    } else {
        Err(cfe(jvm, "EOF"))
    }
}

fn read_u16<'a>(jvm: &Jvm, input: &mut &'a [u8]) -> JVMResult<u16> {
    let high = read_u8(jvm, input)?;
    let low = read_u8(jvm, input)?;
    Ok(((high as u16) << 8) + low as u16)
}

fn read_u32<'a>(jvm: &Jvm, input: &mut &'a [u8]) -> JVMResult<u32> {
    let high = read_u16(jvm, input)?;
    let low = read_u16(jvm, input)?;
    Ok(((high as u32) << 16) + low as u32)
}

fn read_u64<'a>(jvm: &Jvm, input: &mut &'a [u8]) -> JVMResult<u64> {
    let high = read_u32(jvm, input)?;
    let low = read_u32(jvm, input)?;
    Ok(((high as u64) << 32) + low as u64)
}

fn read_magic<'a>(jvm: &Jvm, input: &mut &'a [u8]) -> JVMResult<()> {
    if 0xCAFEBABE == read_u32(jvm, input)? {
        Ok(())
    } else {
        Err(cfe(jvm, "wrong magic number"))
    }
}

fn read_const_pool_item<'a>(input: &mut &'a [u8], jvm: &Jvm) -> JVMResult<ConstPoolItem> {
    use ConstPoolItem::*;
    match read_u8(jvm, input)? {
        1 => {
            let length = read_u16(jvm, input)?;
            let mut bytes = Vec::with_capacity(length as usize);
            for _ in 0..length {
                bytes.push(read_u8(jvm, input)?);
            }
            // TODO: actually read Modified-UTF8 (null codepoint is remapped)
            let string =
                std::string::String::from_utf8(bytes).map_err(|_| cfe(jvm, "invalid utf8"))?;
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
        _ => Err(cfe(jvm, "Unknown constant pool item")),
    }
}

fn read_const_pool<'a>(input: &mut &'a [u8], jvm: &Jvm) -> JVMResult<ConstPool> {
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
        if !match item {
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
        } {
            return Err(cfe(jvm, "Invalid constant pool item"));
        }
    }

    Ok(ConstPool { items })
}

fn read_interfaces(
    jvm: &Jvm,
    input: &mut &[u8],
    const_pool: &ConstPool,
) -> JVMResult<Vec<Arc<str>>> {
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
) -> JVMResult<(Arc<str>, &'a [u8])> {
    let name = const_pool.get_utf8(jvm, read_u16(jvm, input)?)?;
    let attr_length = read_u32(jvm, input)?;
    if attr_length as usize > input.len() {
        return Err(cfe(jvm, "EOF reading attribute"));
    }
    let attr_bytes = &input[0..attr_length as usize];
    *input = &input[attr_length as usize..];
    Ok((name, attr_bytes))
}

pub(crate) fn parse_field_descriptor(
    jvm: &Jvm,
    descriptor: &str,
    start: usize,
) -> JVMResult<(Typ, usize)> {
    let str = &descriptor[start..];
    if let Some(typ) = match str.chars().next() {
        Some('Z') => Some(Typ::Boolean),
        Some('B') => Some(Typ::Byte),
        Some('C') => Some(Typ::Char),
        Some('S') => Some(Typ::Short),
        Some('D') => Some(Typ::Double),
        Some('F') => Some(Typ::Float),
        Some('I') => Some(Typ::Int),
        Some('J') => Some(Typ::Long),
        _ => None,
    } {
        // Primitive
        Ok((typ, start + 1))
    } else if let Some(stripped) = str.strip_prefix('L') {
        let Some((class_name, _)) = stripped.split_once(';') else {
            return Err(cfe(jvm, "invalid field descriptor"));
        };
        let start = start + 1 + class_name.len() + 1;
        let typ = Typ::Ref(class_name.into());
        // Class
        Ok((typ, start))
    } else if str.starts_with('[') {
        let (_, end) = parse_field_descriptor(jvm, descriptor, start + 1)?;
        let typ = Typ::Ref(str[..end].into());
        if typ.array_dimensions() > 255 {
            return Err(cfe(jvm, "too many array dimensions"));
        }
        // Array
        Ok((typ, end))
    } else {
        Err(cfe(jvm, "invalid field descriptor"))
    }
}

fn read_field(input: &mut &[u8], constant_pool: &ConstPool, jvm: &Jvm) -> JVMResult<Field> {
    let access_flags = AccessFlags::from_bits_truncate(read_u16(jvm, input)?);
    if access_flags.contains(AccessFlags::FINAL | AccessFlags::VOLATILE) {
        return Err(cfe(jvm, "Field may not be final and volatile"));
    }

    let name = constant_pool.get_utf8(jvm, read_u16(jvm, input)?)?;

    let descriptor = constant_pool.get_utf8(jvm, read_u16(jvm, input)?)?;
    let (typ, end) = parse_field_descriptor(jvm, &descriptor, 0)?;
    if end < descriptor.len() {
        return Err(cfe(jvm, "Invalid type descriptor"));
    }

    let mut const_value_index = None;
    let attributes_count = read_u16(jvm, input)?;
    for _ in 0..attributes_count {
        let (name, data) = read_attribute(jvm, input, constant_pool)?;
        if access_flags.contains(AccessFlags::STATIC) && (name.as_ref() == "ConstantValue") {
            if const_value_index.is_none() & (data.len() == 2) {
                const_value_index = Some(((data[0] as u16) << 8) + data[1] as u16)
            } else {
                return Err(cfe(jvm, "Invalid ConstantValue attribute"));
            }
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

fn read_fields(input: &mut &[u8], constant_pool: &ConstPool, jvm: &Jvm) -> JVMResult<Vec<Field>> {
    // Read fields
    let length = read_u16(jvm, input)?;
    let mut fields = Vec::new();
    for _ in 0..length {
        let field = read_field(input, constant_pool, jvm)?;
        fields.push(field);
    }
    Ok(fields)
}

fn read_verification_type<'a>(
    jvm: &Jvm,
    input: &mut &'a [u8],
    const_pool: &ConstPool,
) -> JVMResult<VerificationType> {
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
        _ => return Err(cfe(jvm, "Invalid verification type")),
    })
}

// TODO: check how inefficient this is
/// This is called during varification, as it is only needed then and because the const pool has been resolved by then.
pub(crate) fn read_stack_map_table(
    jvm: &Jvm,
    input: Option<&[u8]>,
    const_pool: &ConstPool,
    initial_locals: Vec<VerificationType>,
) -> JVMResult<BTreeMap<u16, StackMapFrame>> {
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
        return Ok(frames);
    };
    let input = &mut input;
    let length = read_u16(jvm, input)?;
    for _ in 0..length {
        let prev_frame = frames.iter().rev().next().unwrap().1;
        let frame_type = read_u8(jvm, input)?;
        let (offset, current_frame) = if frame_type < 64 {
            // Same
            (frame_type as u16, prev_frame.clone())
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
            (
                read_u16(jvm, input)?,
                StackMapFrame {
                    locals: if prev_frame.locals.len() < k as usize {
                        return Err(cfe(jvm, "invalid stack frame"));
                    } else {
                        prev_frame.locals[0..prev_frame.locals.len() - k as usize].into()
                    },
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
                locals.push(read_verification_type(jvm, input, const_pool)?)
            }
            let num_stack = read_u16(jvm, input)?;
            let mut stack = Vec::with_capacity(num_stack as usize);
            for _ in 0..num_stack {
                stack.push(read_verification_type(jvm, input, const_pool)?)
            }
            (offset_delta, StackMapFrame { locals, stack })
        } else {
            return Err(cfe(jvm, "invalid stack map frame type"));
        };
        pc += offset;
        frames.insert(pc, current_frame);
        // This is not applied if the previous frame was the initial (implicit) frame
        pc += 1;
    }
    if !input.is_empty() {
        return Err(cfe(jvm, "invalid stack map table"));
    }
    Ok(frames)
}

fn read_code(jvm: &Jvm, mut input: &[u8], constant_pool: &ConstPool) -> JVMResult<Code> {
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
    for _ in 0..attributes_count {
        // Ignore attributes for now
        let (name, bytes) = read_attribute(jvm, input, constant_pool)?;
        if name.as_ref() == "StackMapTable" {
            if stack_map_table.is_none() {
                stack_map_table = Some(bytes.into());
            } else {
                return Err(cfe(jvm, "Duplicate StackMapTable"));
            }
        }
    }

    if !input.is_empty() {
        return Err(cfe(jvm, "End of code attribute not reached"));
    }

    Ok(Code {
        max_stack,
        max_locals,
        bytes,
        stack_map_table,
        exception_table,
    })
}

pub(crate) fn parse_method_descriptor(jvm: &Jvm, descriptor: &str) -> JVMResult<MethodDescriptor> {
    if !descriptor.starts_with('(') {
        return Err(cfe(jvm, "Invalid method descriptor"));
    }
    let mut start = 1;
    let mut args = Vec::new();
    while !descriptor[start..].starts_with(')') {
        let (arg, next_start) = parse_field_descriptor(jvm, descriptor, start)?;
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
        let (return_type, start) = parse_field_descriptor(jvm, descriptor, start)?;
        if start < descriptor.len() {
            return Err(cfe(jvm, "Invalid method descriptor"));
        }
        Ok(MethodDescriptor {
            args,
            returns: Some(return_type),
        })
    }
}

fn read_method<'b, 'c>(
    input: &'b mut &'c [u8],
    constant_pool: &'b ConstPool,
    jvm: &'b Jvm,
) -> JVMResult<Method> {
    let access_flags = AccessFlags::from_bits_truncate(read_u16(jvm, input)?);

    let name = constant_pool.get_utf8(jvm, read_u16(jvm, input)?)?;

    let descriptor = constant_pool.get_utf8(jvm, read_u16(jvm, input)?)?;
    let descriptor = parse_method_descriptor(jvm, &descriptor)?;

    let mut code = None;
    let attributes_count = read_u16(jvm, input)?;
    for _ in 0..attributes_count {
        let (attr_name, bytes) = read_attribute(jvm, input, constant_pool)?;
        if attr_name.as_ref() == "Code" {
            if code.is_none() {
                code = Some(read_code(jvm, bytes, constant_pool)?);
            } else {
                return Err(cfe(jvm, "Multiple code attributes on same method"));
            }
        }
    }

    let typ = jvm.method_descriptor_storage.lock().alloc(descriptor);

    Ok(Method {
        nat: MethodNaT { name, typ },
        access_flags,
        class: default(),
        code,
    })
}

fn read_methods(
    input: &mut &[u8],
    constant_pool: &ConstPool,
    jvm: &Jvm,
) -> JVMResult<HashMap<MethodNaT<'static>, &'static Method>> {
    let length = read_u16(jvm, input)?;
    let mut methods = HashMap::with_capacity(length as usize);
    for _ in 0..length {
        let method = read_method(input, constant_pool, jvm)?;
        for existing_nat in methods.keys() {
            if existing_nat == &method.nat {
                return Err(cfe(jvm, "Duplicate method in same class"));
            }
        }
        let method = jvm.method_storage.lock().alloc(method);
        methods.insert(method.nat.clone(), &*method);
    }
    Ok(methods)
}

/// class of each field is set to the dummy,
/// super_class is set to None,
/// actual superclass name (which needs to be resolved) is returned alongside,
pub(crate) fn read_class_file(mut input: &[u8], jvm: &Jvm) -> JVMResult<ClassDescriptor> {
    let input = &mut input;

    read_magic(jvm, input)?;
    let _minor_version = read_u16(jvm, input)?;
    let major_version = read_u16(jvm, input)?;

    if !(50..=52).contains(&major_version) {
        return Err(exception(jvm, "UnsupportedClassVersionError"));
    }

    let const_pool = read_const_pool(input, jvm)?;

    let access_flags = AccessFlags::from_bits_truncate(read_u16(jvm, input)?);

    let this_class = const_pool.get_unresolved_class(jvm, read_u16(jvm, input)?)?;
    let super_class = {
        let index = read_u16(jvm, input)?;
        if index > 0 {
            Some(const_pool.get_unresolved_class(jvm, index)?)
        } else {
            None
        }
    };

    let interfaces = read_interfaces(jvm, input, &const_pool)?;
    let fields = read_fields(input, &const_pool, jvm)?;
    let methods = read_methods(input, &const_pool, jvm)?;

    let attributes_count = read_u16(jvm, input)?;
    for _ in 0..attributes_count {
        read_attribute(jvm, input, &const_pool)?;
    }

    if !input.is_empty() {
        return Err(cfe(jvm, "expected EOF"));
    }

    Ok(ClassDescriptor {
        const_pool,
        access_flags,
        name: this_class,
        super_class,
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
    pub(crate) interfaces: Vec<Arc<str>>,
    pub(crate) fields: Vec<Field>,
    pub(crate) methods: HashMap<MethodNaT<'static>, &'static Method>,
}

fn cfe(jvm: &Jvm, _message: &str) -> Object {
    exception(jvm, "ClassFormatError")
}
