use std::{
    alloc::Layout,
    cell::Cell,
    collections::{BTreeMap, HashMap},
};

use crate::{
    class::{Field, FieldNaT, Method, MethodNaT},
    const_pool::{ConstPool, ConstPoolItem},
    object::header_size,
    verification::{StackMapFrame, VerificationType},
    AccessFlags, Code, FieldStorage, IntStr, MethodDescriptor, Typ, JVM,
};
use anyhow::{bail, Context, Result};

fn read_u8(input: &mut &[u8]) -> Result<u8> {
    if let Some(u8) = input.get(0) {
        *input = &input[1..];
        Ok(*u8)
    } else {
        bail!("EOF")
    }
}

fn read_u16(input: &mut &[u8]) -> Result<u16> {
    let high = read_u8(input)?;
    let low = read_u8(input)?;
    Ok(((high as u16) << 8) + low as u16)
}

fn read_u32(input: &mut &[u8]) -> Result<u32> {
    let high = read_u16(input)?;
    let low = read_u16(input)?;
    Ok(((high as u32) << 16) + low as u32)
}

fn read_u64(input: &mut &[u8]) -> Result<u64> {
    let high = read_u32(input)?;
    let low = read_u32(input)?;
    Ok(((high as u64) << 32) + low as u64)
}

fn read_magic(input: &mut &[u8]) -> Result<()> {
    if 0xCAFEBABE == read_u32(input)? {
        Ok(())
    } else {
        bail!("Not a class file")
    }
}

fn read_const_pool_item<'a, 'b, 'c>(
    input: &'b mut &'c [u8],
    jvm: &'b JVM<'a>,
) -> Result<ConstPoolItem<'a>> {
    use ConstPoolItem::*;
    match read_u8(input)? {
        1 => {
            let length = read_u16(input)?;
            let mut bytes = Vec::with_capacity(length as usize);
            for _ in 0..length {
                bytes.push(read_u8(input)?);
            }
            // TODO: actually read Modified-UTF8
            let string = std::string::String::from_utf8(bytes)?;
            Ok(Utf8(jvm.intern_str(&string)))
        }
        3 => Ok(Integer(read_u32(input)? as i32)),
        4 => Ok(Float(f32::from_bits(read_u32(input)?))),
        5 => Ok(Long(read_u64(input)? as i64)),
        6 => Ok(Double(f64::from_bits(read_u64(input)?))),
        7 => Ok(RawClass(read_u16(input)?)),
        8 => Ok(RawString(read_u16(input)?)),
        9 => Ok(FieldRef {
            class: read_u16(input)?,
            nat: read_u16(input)?,
        }),
        10 => Ok(MethodRef {
            class: read_u16(input)?,
            nat: read_u16(input)?,
        }),
        11 => Ok(InterfaceMethodRef {
            class: read_u16(input)?,
            nat: read_u16(input)?,
        }),
        12 => Ok(NameAndType {
            name: read_u16(input)?,
            descriptor: read_u16(input)?,
        }),
        _ => bail!("Unknown constant pool item"),
    }
}

fn read_const_pool<'a, 'b, 'c>(input: &'b mut &'c [u8], jvm: &'b JVM<'a>) -> Result<ConstPool<'a>> {
    let length = read_u16(input)? as usize;
    let mut items = Vec::with_capacity(length);
    items.push(Cell::new(
        ConstPoolItem::PlaceholderAfterLongOrDoubleEntryOrForEntryZero,
    ));
    while items.len() < length {
        items.push(Cell::new(read_const_pool_item(input, jvm)?));
        if matches!(
            items.last().unwrap().get(),
            ConstPoolItem::Long(_) | ConstPoolItem::Double(_)
        ) {
            items.push(Cell::new(
                ConstPoolItem::PlaceholderAfterLongOrDoubleEntryOrForEntryZero,
            ))
        }
    }
    // Const pool validity is part of format checking
    for item in items.iter() {
        if !match item.get() {
            ConstPoolItem::RawClass(index) => matches!(
                items.get(index as usize).map(Cell::get),
                Some(ConstPoolItem::Utf8(..))
            ),
            ConstPoolItem::FieldRef { class, nat }
            | ConstPoolItem::MethodRef { class, nat }
            | ConstPoolItem::InterfaceMethodRef { class, nat } => matches!(
                (
                    items.get(class as usize).map(Cell::get),
                    items.get(nat as usize).map(Cell::get)
                ),
                (
                    Some(ConstPoolItem::RawClass(..)),
                    Some(ConstPoolItem::NameAndType { .. })
                )
            ),
            ConstPoolItem::NameAndType { name, descriptor } => matches!(
                (
                    items.get(name as usize).map(Cell::get),
                    items.get(descriptor as usize).map(Cell::get)
                ),
                (Some(ConstPoolItem::Utf8(..)), Some(ConstPoolItem::Utf8(..)))
            ),
            ConstPoolItem::RawString(index) => {
                matches!(
                    items.get(index as usize).map(Cell::get),
                    Some(ConstPoolItem::Utf8(..))
                )
            }
            _ => true,
        } {
            bail!("Invalid constant pool item: {:?}", item.get())
        }
    }

    Ok(ConstPool { items })
}

fn read_interfaces<'a>(input: &mut &[u8], const_pool: &ConstPool<'a>) -> Result<Vec<IntStr<'a>>> {
    let length = read_u16(input)?;
    let mut vec = Vec::with_capacity(length as usize);
    for _ in 0..length {
        vec.push(const_pool.get_unresolved_class(read_u16(input)?)?);
    }
    Ok(vec)
}

fn read_attribute<'a, 'b, 'c>(
    input: &mut &'b [u8],
    const_pool: &'c ConstPool<'a>,
) -> Result<(IntStr<'a>, &'b [u8])> {
    let name = const_pool.get_utf8(read_u16(input)?)?;
    let attr_length = read_u32(input)?;
    if attr_length as usize > input.len() {
        bail!("EOF")
    }
    let attr_bytes = &input[0..attr_length as usize];
    *input = &input[attr_length as usize..];
    Ok((name, attr_bytes))
}

pub(crate) fn parse_field_descriptor<'a, 'b>(
    jvm: &'b JVM<'a>,
    descriptor: IntStr<'a>,
    start: usize,
) -> Result<(Typ<'a>, usize)> {
    let str = &descriptor.0[start..];
    if let Some(typ) = match str.chars().next() {
        Some('Z') => Some(Typ::Bool),
        Some('B') => Some(Typ::Byte),
        Some('C') => Some(Typ::Char),
        Some('S') => Some(Typ::Short),
        Some('D') => Some(Typ::Double),
        Some('F') => Some(Typ::Float),
        Some('I') => Some(Typ::Int),
        Some('J') => Some(Typ::Long),
        _ => None,
    } {
        return Ok((typ, start + 1));
    }

    if let Some(stripped) = str.strip_prefix('L') {
        if let Some((class_name, _)) = stripped.split_once(';') {
            let start = start + 1 + class_name.len() + 1;
            let class_name = String::from(class_name);
            let typ = Typ::Ref(jvm.intern_str(&class_name));
            return Ok((typ, start));
        } else {
            bail!("Invalid type descriptor")
        }
    }

    if str.starts_with('[') {
        let (_, end) = parse_field_descriptor(jvm, descriptor, start + 1)?;
        let typ = Typ::Ref(jvm.intern_str(&str[..end]));
        if typ.array_dimensions() > 255 {
            bail!("Too many array dimensions")
        }
        return Ok((typ, start));
    }

    bail!("Invalid type descriptor: {}", str)
}

fn read_field<'a, 'b, 'c>(
    input: &'b mut &'c [u8],
    constant_pool: &ConstPool<'a>,
    jvm: &'b JVM<'a>,
) -> Result<Field<'a>> {
    let access_flags = AccessFlags::from_bits_truncate(read_u16(input)?);

    let name = constant_pool.get_utf8(read_u16(input)?)?;

    let descriptor_str = constant_pool.get_utf8(read_u16(input)?)?;
    let (descriptor, remaining) = parse_field_descriptor(jvm, descriptor_str, 0)?;
    if remaining < descriptor_str.0.len() {
        bail!("Invalid type descriptor")
    }

    let mut const_value_index = None;
    let attributes_count = read_u16(input)?;
    for _ in 0..attributes_count {
        let (name, data) = read_attribute(input, constant_pool)?;
        if access_flags.contains(AccessFlags::STATIC) && (name.0 == "ConstantValue") {
            if const_value_index.is_none() & (data.len() == 2) {
                const_value_index = Some(((data[0] as u16) << 8) + data[1] as u16)
            } else {
                bail!("Invalid ConstantValue attribute")
            }
        }
    }

    Ok(Field {
        name,
        access_flags,
        descriptor,
        // The correct layout is set in read_fields after field disordering
        byte_offset: 0,
        const_value_index,
        class: Cell::new(jvm.dummy_class),
    })
}

fn read_fields<'a, 'b, 'c>(
    input: &'b mut &'c [u8],
    constant_pool: &ConstPool<'a>,
    jvm: &'b JVM<'a>,
) -> Result<(
    HashMap<FieldNaT<'a>, Field<'a>>,
    /*static fields size*/ usize,
    /*object fields size*/ usize,
)> {
    // Read fields
    let length = read_u16(input)?;
    let mut fields = Vec::new();
    for _ in 0..length {
        let field = read_field(input, constant_pool, jvm)?;
        fields.push(field);
    }
    // Decide layout
    fields.sort_by_key(|field| field.descriptor.layout().align());
    let mut static_layout = Layout::new::<()>();
    let mut object_layout = Layout::from_size_align(header_size(), 8).unwrap();
    let fields = fields
        .into_iter()
        .map(|mut field| {
            let layout = field.descriptor.layout();
            let fields_layout = if field.access_flags.contains(AccessFlags::STATIC) {
                &mut static_layout
            } else {
                &mut object_layout
            };
            let result = fields_layout.extend(layout).unwrap();
            *fields_layout = result.0;
            field.byte_offset = result.1 as u32;
            (
                FieldNaT {
                    name: field.name,
                    typ: field.descriptor,
                },
                field,
            )
        })
        .collect();

    Ok((fields, static_layout.size(), object_layout.size()))
}

fn read_verification_type<'c, 'b, 'a>(
    input: &'b mut &'c [u8],
    const_pool: &'b ConstPool<'a>,
) -> Result<VerificationType<'a>> {
    Ok(match read_u8(input)? {
        0 => VerificationType::Top,
        1 => VerificationType::Integer,
        2 => VerificationType::Float,
        3 => VerificationType::Double,
        4 => VerificationType::Long,
        5 => VerificationType::Null,
        6 => VerificationType::UninitializedThis,
        7 => VerificationType::ObjectVariable(const_pool.get_class(read_u16(input)?)?),
        8 => VerificationType::UninitializedVariable {
            offset: read_u16(input)?,
        },
        _ => bail!("Invalid verification type"),
    })
}

// TODO: check how inefficient this is
/// This is called during varification, as it is only needed then and because the const pool has been resolved by then.
pub(crate) fn read_stack_map_table<'c, 'b, 'a>(
    input: Option<&'c [u8]>,
    const_pool: &'b ConstPool<'a>,
    initial_locals: Vec<VerificationType<'a>>,
) -> Result<BTreeMap<u16, StackMapFrame<'a>>> {
    let mut frames = BTreeMap::new();
    let mut pc = 0;
    frames.insert(
        pc,
        StackMapFrame {
            stack: [].into(),
            locals: initial_locals,
        },
    );
    let mut input = if let Some(input) = input {
        input
    } else {
        return Ok(frames);
    };
    let input = &mut input;
    let length = read_u16(input)?;
    for _ in 0..length {
        let prev_frame = frames.iter().rev().next().unwrap().1;
        let frame_type = read_u8(input)?;
        let (offset, current_frame) = if frame_type < 64 {
            // same
            (frame_type as u16, prev_frame.clone())
        } else if frame_type < 128 {
            // same locals 1 stack item
            (
                frame_type as u16 - 64,
                StackMapFrame {
                    locals: prev_frame.locals.clone(),
                    stack: vec![read_verification_type(input, const_pool)?],
                },
            )
        } else if frame_type == 247 {
            // same locals 1 stack item extended
            (
                read_u16(input)?,
                StackMapFrame {
                    locals: prev_frame.locals.clone(),
                    stack: vec![read_verification_type(input, const_pool)?],
                },
            )
        } else if (frame_type > 247) & (frame_type < 251) {
            // chop
            let k = 251 - frame_type;
            (
                read_u16(input)?,
                StackMapFrame {
                    locals: if prev_frame.locals.len() < k as usize {
                        bail!("invalid stack frame")
                    } else {
                        prev_frame.locals[0..prev_frame.locals.len() - k as usize].into()
                    },
                    stack: [].into(),
                },
            )
        } else if frame_type == 251 {
            // same extended
            (read_u16(input)?, prev_frame.clone())
        } else if (frame_type > 251) & (frame_type < 255) {
            // append
            let k = frame_type - 251;
            (
                read_u16(input)?,
                StackMapFrame {
                    locals: {
                        let mut locals = prev_frame.locals.clone();
                        for _ in 0..k {
                            let typ = read_verification_type(input, const_pool)?;
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
            // full
            let offset_delta = read_u16(input)?;
            let num_locals = read_u16(input)?;
            let mut locals = Vec::with_capacity(num_locals as usize);
            for _ in 0..num_locals {
                locals.push(read_verification_type(input, const_pool)?)
            }
            let num_stack = read_u16(input)?;
            let mut stack = Vec::with_capacity(num_stack as usize);
            for _ in 0..num_stack {
                stack.push(read_verification_type(input, const_pool)?)
            }
            (offset_delta, StackMapFrame { locals, stack })
        } else {
            bail!("invalid verification type: {}", frame_type)
        };
        pc += offset as u16;
        frames.insert(pc, current_frame);
        // This is not applied if the previous frame was the initial (implicit) frame
        pc += 1;
    }
    if !input.is_empty() {
        bail!("invalid stack map table: {:?}", input)
    }
    Ok(frames)
}

fn read_code<'a>(mut input: &[u8], constant_pool: &ConstPool<'a>) -> Result<Code> {
    let input = &mut input;
    let max_stack = read_u16(input)?;
    let max_locals = read_u16(input)?;

    let code_length = read_u32(input)?;
    let mut bytes = Vec::with_capacity(code_length as usize);
    for _ in 0..code_length {
        bytes.push(read_u8(input)?);
    }

    let exception_table_length = read_u16(input)?;
    for _ in 0..exception_table_length {
        // TODO: unsupport exceptions
        read_u64(input)?;
    }

    let attributes_count = read_u16(input)?;
    let mut stack_map_table = None;
    for _ in 0..attributes_count {
        // Ignore attributes for now
        let (name, bytes) = read_attribute(input, constant_pool).context("Reading attributes")?;
        if name.0 == "StackMapTable" {
            if stack_map_table.is_none() {
                stack_map_table = Some(bytes.into());
            } else {
                bail!("Duplicate StackMapTable")
            }
        }
    }

    if !input.is_empty() {
        bail!("End of code attribute not reached")
    }

    Ok(Code {
        max_stack,
        max_locals,
        bytes,
        stack_map_table,
    })
}

pub(crate) fn parse_method_descriptor<'a, 'b>(
    jvm: &'b JVM<'a>,
    descriptor: IntStr<'a>,
) -> Result<MethodDescriptor<'a>> {
    if !descriptor.0.starts_with('(') {
        bail!("Invalid method descriptor")
    }
    let mut start = 1;
    let mut args = Vec::new();
    while !descriptor.0[start..].starts_with(')') {
        let (arg, next_start) =
            parse_field_descriptor(jvm, descriptor, start).context("Parsing method descriptor")?;
        args.push(arg);
        start = next_start;
    }
    start += 1;
    if &descriptor.0[start..] == "V" {
        Ok(MethodDescriptor(args, None))
    } else {
        let (return_type, start) =
            parse_field_descriptor(jvm, descriptor, start).context("Parsing method descriptor")?;
        if start < descriptor.0.len() {
            bail!("Invalid method descriptor")
        }
        Ok(MethodDescriptor(args, Some(return_type)))
    }
}

fn read_method<'a, 'b, 'c>(
    input: &'b mut &'c [u8],
    constant_pool: &'b ConstPool<'a>,
    jvm: &'b JVM<'a>,
) -> Result<Method<'a>> {
    let access_flags = AccessFlags::from_bits_truncate(read_u16(input)?);

    let name = constant_pool.get_utf8(read_u16(input)?)?;

    let descriptor = constant_pool.get_utf8(read_u16(input)?)?;
    let descriptor = parse_method_descriptor(jvm, descriptor)?;

    let mut code = None;
    let attributes_count = read_u16(input)?;
    for _ in 0..attributes_count {
        let (attr_name, bytes) = read_attribute(input, constant_pool)?;
        if attr_name.0 == "Code" {
            if code.is_none() {
                code = Some(read_code(bytes, constant_pool).context("Reading bytecode")?);
            } else {
                bail!("Multiple code attributes on same method")
            }
        }
    }

    let typ = jvm
        .method_descriptor_storage
        .lock()
        .unwrap()
        .alloc(descriptor);

    Ok(Method {
        nat: MethodNaT { name, typ },
        access_flags,
        class: Cell::new(jvm.dummy_class),
        code,
    })
}

fn read_methods<'a, 'b, 'c>(
    input: &'b mut &'c [u8],
    constant_pool: &ConstPool<'a>,
    jvm: &'b JVM<'a>,
) -> Result<HashMap<MethodNaT<'a>, &'a Method<'a>>> {
    let length = read_u16(input)?;
    let mut methods = HashMap::with_capacity(length as usize);
    for _ in 0..length {
        let method = read_method(input, constant_pool, jvm)?;
        for existing_nat in methods.keys() {
            if existing_nat == &method.nat {
                bail!("Duplicate method in same class")
            }
        }
        let method = jvm.method_storage.lock().unwrap().alloc(method);
        methods.insert(method.nat, &*method);
    }
    Ok(methods)
}

/// class of each field is set to the dummy,
/// super_class is set to None,
/// actual superclass name (which needs to be resolved) is returned alongside,
pub(crate) fn read_class_file<'a, 'b, 'c>(
    mut input: &'c [u8],
    jvm: &'b JVM<'a>,
) -> Result<ClassDescriptor<'a>> {
    let input = &mut input;

    read_magic(input)?;
    let minor_version = read_u16(input)?;
    let major_version = read_u16(input)?;

    if major_version > 52 {
        bail!(
            "Class version {}.{} > 52 not supported yet",
            major_version,
            minor_version
        )
    }
    if major_version < 50 {
        // Verification by type inference not implemented yet
        bail!(
            "Class version {}.{} < 50 not supported yet",
            major_version,
            minor_version
        )
    }

    let const_pool = read_const_pool(input, jvm)?;

    let access_flags = AccessFlags::from_bits_truncate(read_u16(input)?);

    let this_class = const_pool.get_unresolved_class(read_u16(input)?)?;
    let super_class = {
        let index = read_u16(input)?;
        if index > 0 {
            Some(const_pool.get_unresolved_class(index)?)
        } else {
            None
        }
    };

    let interfaces =
        read_interfaces(input, &const_pool).context("Reading implemented interfaces")?;
    let (fields, static_layout, object_size) =
        read_fields(input, &const_pool, jvm).context("Reading fields")?;
    let methods = read_methods(input, &const_pool, jvm).context("Reading methods")?;

    let static_storage = FieldStorage::new(static_layout);

    let attributes_count = read_u16(input)?;
    for _ in 0..attributes_count {
        read_attribute(input, &const_pool)?;
    }

    if !input.is_empty() {
        bail!("Malformed class file: Expected EOF")
    }

    Ok(ClassDescriptor {
        const_pool,
        access_flags,
        name: this_class,
        super_class,
        interfaces,
        static_storage,
        object_size,
        fields,
        methods,
    })
}

/// Represents a class with not yet resolved superclass & interfaces
pub(crate) struct ClassDescriptor<'a> {
    pub(crate) const_pool: ConstPool<'a>,
    pub(crate) access_flags: AccessFlags,
    pub(crate) name: IntStr<'a>,
    pub(crate) super_class: Option<IntStr<'a>>,
    pub(crate) interfaces: Vec<IntStr<'a>>,
    pub(crate) fields: HashMap<FieldNaT<'a>, Field<'a>>,
    pub(crate) methods: HashMap<MethodNaT<'a>, &'a Method<'a>>,
    pub(crate) static_storage: FieldStorage,
    pub(crate) object_size: usize,
}
