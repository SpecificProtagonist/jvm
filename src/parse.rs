use std::alloc::Layout;

use crate::{
    class::{Class, FieldMeta, MethodMeta},
    const_pool::{ConstPool, ConstPoolItem},
    object::min_object_layout,
    AccessFlags, Code, FieldRef, FieldStorage, IntStr, MethodDescriptor, MethodRef, Typ, JVM,
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
        7 => Ok(Class(read_u16(input)?)),
        8 => Ok(RawString(read_u16(input)?)),
        9 => Ok(RawFieldRef {
            class: read_u16(input)?,
            nat: read_u16(input)?,
        }),
        10 => Ok(RawMethodRef {
            class: read_u16(input)?,
            nat: read_u16(input)?,
        }),
        11 => Ok(InterfaceMethodRef {
            class: read_u16(input)?,
            nat: read_u16(input)?,
        }),
        12 => Ok(RawNameAndType {
            name: read_u16(input)?,
            descriptor: read_u16(input)?,
        }),
        _ => bail!("Unknown constant pool item"),
    }
}

fn read_const_pool<'a, 'b, 'c>(input: &'b mut &'c [u8], jvm: &'b JVM<'a>) -> Result<ConstPool<'a>> {
    let length = read_u16(input)? as usize;
    let mut vec = Vec::with_capacity(length);
    vec.push(ConstPoolItem::PlaceholderAfterLongOrDoubleEntryOrForEntryZero);
    while vec.len() < length {
        vec.push(read_const_pool_item(input, jvm)?);
        if matches!(
            vec.last().unwrap(),
            ConstPoolItem::Long(_) | ConstPoolItem::Double(_)
        ) {
            vec.push(ConstPoolItem::PlaceholderAfterLongOrDoubleEntryOrForEntryZero)
        }
    }
    let mut pool = ConstPool(vec);

    // Post-process to get desireable format for run-time pool
    // (can't do this in the first pass because items may refer to later items)
    for i in 0..pool.0.len() {
        if let ConstPoolItem::RawFieldRef { class, nat } = &pool.0[i] {
            let class = pool.get_class(*class)?;
            let (name, descriptor) = pool.get_raw_nat(*nat)?;
            let (typ, _) = parse_field_descriptor(jvm, descriptor, 0)?;
            pool.0[i] = ConstPoolItem::FieldRef(FieldRef { class, name, typ })
        }
        if let ConstPoolItem::RawMethodRef { class, nat } = &pool.0[i] {
            let class = pool.get_class(*class)?;
            let (name, descriptor) = pool.get_raw_nat(*nat)?;
            let typ = parse_method_descriptor(jvm, descriptor)?;
            pool.0[i] = ConstPoolItem::MethodRef(MethodRef { class, name, typ })
        }
    }

    Ok(pool)
}

fn read_interfaces<'a, 'b>(
    input: &mut &[u8],
    const_pool: &ConstPool<'a>,
) -> Result<Vec<IntStr<'a>>> {
    let length = read_u16(input)?;
    let mut vec = Vec::with_capacity(length as usize);
    for _ in 0..length {
        vec.push(const_pool.get_class(read_u16(input)?)?);
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

fn parse_field_descriptor<'a, 'b>(
    jvm: &'b JVM<'a>,
    descriptor: IntStr<'a>,
    start: usize,
) -> Result<(&'a Typ<'a>, usize)> {
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
        return Ok((jvm.intern_type(typ), start + 1));
    }

    if str.starts_with('L') {
        if let Some((class_name, _)) = str[1..].split_once(';') {
            let start = start + 1 + class_name.len() + 1;
            let class_name = String::from(class_name);
            let typ = Typ::Class(jvm.intern_str(&class_name));
            return Ok((jvm.intern_type(typ), start));
        } else {
            bail!("Invalid type descriptor")
        }
    }

    if str.starts_with('[') {
        // TODO: array descriptor only valid for up to 255 dimensions
        let (base_type, start) = parse_field_descriptor(jvm, descriptor, start + 1)?;
        let typ = match base_type {
            Typ::Array { base, dimensions } => {
                if *dimensions == u8::MAX {
                    bail!("Array has too many dimensions")
                }
                Typ::Array {
                    base,
                    dimensions: dimensions + 1,
                }
            }
            _ => Typ::Array {
                base: base_type,
                dimensions: 1,
            },
        };
        return Ok((jvm.intern_type(typ), start));
    }

    bail!("Invalid type descriptor: {}", str)
}

fn read_field<'a, 'b, 'c>(
    input: &'b mut &'c [u8],
    constant_pool: &ConstPool<'a>,
    jvm: &'b JVM<'a>,
) -> Result<FieldMeta<'a>> {
    let access_flags = AccessFlags::from_bits_truncate(read_u16(input)?);

    let name = constant_pool.get_utf8(read_u16(input)?)?;

    let descriptor_str = constant_pool.get_utf8(read_u16(input)?)?;
    let (descriptor, remaining) = parse_field_descriptor(jvm, descriptor_str, 0)?;
    if remaining < descriptor_str.0.len() {
        bail!("Invalid type descriptor")
    }

    let attributes_count = read_u16(input)?;
    for _ in 0..attributes_count {
        // Ignore attributes
        let _ = read_attribute(input, constant_pool);
    }

    Ok(FieldMeta {
        name,
        access_flags,
        descriptor,
        // The correct layout is set in read_fields after field disordering
        byte_offset: 0,
    })
}

fn read_fields<'a, 'b, 'c>(
    input: &'b mut &'c [u8],
    constant_pool: &ConstPool<'a>,
    jvm: &'b JVM<'a>,
) -> Result<(
    Vec<FieldMeta<'a>>,
    /*static*/ Layout,
    /*object*/ Layout,
)> {
    // Read fields
    let length = read_u16(input)?;
    let mut fields = Vec::new();
    for _ in 0..length {
        let field = read_field(input, constant_pool, jvm)?;
        fields.push(field);
    }
    // Decide layout
    let types = jvm.types.read().unwrap();
    fields.sort_by_key(|field| field.descriptor.layout().align());
    let mut static_layout = Layout::new::<()>();
    let mut object_layout = min_object_layout();
    for field in &mut fields {
        let layout = field.descriptor.layout();
        let fields_layout = if field.access_flags.contains(AccessFlags::STATIC) {
            &mut static_layout
        } else {
            &mut object_layout
        };
        let result = fields_layout.extend(layout).unwrap();
        *fields_layout = result.0;
        field.byte_offset = result.1 as u32;
    }

    Ok((fields, static_layout, object_layout))
}

fn read_code(mut input: &[u8], constant_pool: &ConstPool) -> Result<Code> {
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
    for _ in 0..attributes_count {
        // Ignore attributes for now
        let _ = read_attribute(input, constant_pool).context("Reading attributes")?;
    }

    if input.len() > 0 {
        bail!("End of code attribute not reached")
    }

    Ok(Code {
        max_stack,
        max_locals,
        bytes,
    })
}

fn parse_method_descriptor<'a, 'b>(
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
    constant_pool: &ConstPool<'a>,
    jvm: &'b JVM<'a>,
) -> Result<MethodMeta<'a>> {
    let access_flags = AccessFlags::from_bits_truncate(read_u16(input)?);

    let name = constant_pool.get_utf8(read_u16(input)?)?;

    let descriptor = constant_pool.get_utf8(read_u16(input)?)?;
    let descriptor = parse_method_descriptor(jvm, descriptor)?;

    let mut code = None;
    let attributes_count = read_u16(input)?;
    for _ in 0..attributes_count {
        let (attr_name, bytes) = read_attribute(input, constant_pool)?;
        if attr_name == jvm.intern_str("Code") {
            if code.is_none() {
                code = Some(read_code(bytes, constant_pool).context("Reading bytecode")?);
            } else {
                bail!("Multiple code attributes on same method")
            }
        }
    }

    Ok(MethodMeta {
        name,
        access_flags,
        descriptor,
        code,
    })
}

fn read_methods<'a, 'b, 'c>(
    input: &'b mut &'c [u8],
    constant_pool: &ConstPool<'a>,
    jvm: &'b JVM<'a>,
) -> Result<Vec<MethodMeta<'a>>> {
    let length = read_u16(input)?;
    let mut methods: Vec<MethodMeta> = Vec::with_capacity(length as usize);
    for _ in 0..length {
        let method = read_method(input, constant_pool, jvm)?;
        for existing_method in &methods {
            if (existing_method.name == method.name)
                && (existing_method.descriptor == method.descriptor)
            {
                bail!("Duplicate method in same class")
            }
        }
        methods.push(method);
    }
    Ok(methods)
}

pub(crate) fn read_class_file<'a, 'b, 'c>(
    mut input: &'c [u8],
    jvm: &'b JVM<'a>,
) -> Result<Class<'a>> {
    let input = &mut input;

    read_magic(input)?;
    let minor_version = read_u16(input)?;
    let major_version = read_u16(input)?;

    let const_pool = read_const_pool(input, jvm)?;

    let access_flags = AccessFlags::from_bits_truncate(read_u16(input)?);

    let this_class = const_pool.get_class(read_u16(input)?)?;
    let super_class = {
        let index = read_u16(input)?;
        if index > 0 {
            Some(const_pool.get_class(index)?)
        } else {
            None
        }
    };

    let interfaces =
        read_interfaces(input, &const_pool).context("Reading implemented interfaces")?;
    let (fields, static_layout, object_layout) =
        read_fields(input, &const_pool, jvm).context("Reading fields")?;
    let methods = read_methods(input, &const_pool, jvm).context("Reading methods")?;

    let static_storage = FieldStorage::new(static_layout);

    Ok(Class {
        version: (major_version, minor_version),
        const_pool,
        access_flags,
        name: this_class,
        super_class,
        interfaces,
        fields,
        methods,
        static_storage,
        object_layout,
    }
    .into())
}
