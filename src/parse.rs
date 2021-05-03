use std::collections::HashMap;

use crate::{
    Classfile, Code, ConstPool, ConstPoolItem, Field, InternedString, Method, StringInterner,
};
use anyhow::{anyhow, bail, Context, Result};

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

fn read_const_pool_item(
    input: &mut &[u8],
    str_interner: &mut StringInterner,
) -> Result<ConstPoolItem> {
    use ConstPoolItem::*;
    match read_u8(input)? {
        1 => {
            let length = read_u16(input)?;
            let mut bytes = Vec::with_capacity(length as usize);
            for _ in 0..length {
                bytes.push(read_u8(input)?);
            }
            let string = std::string::String::from_utf8(bytes)?;
            Ok(Utf8(str_interner.intern(string)))
        }
        3 => Ok(Integer(read_u32(input)? as i32)),
        4 => Ok(Float(f32::from_bits(read_u32(input)?))),
        5 => Ok(Long(read_u64(input)? as i64)),
        6 => Ok(Double(f64::from_bits(read_u64(input)?))),
        7 => Ok(Class(read_u16(input)?)),
        8 => Ok(String(read_u16(input)?)),
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

fn read_const_pool(input: &mut &[u8], str_interner: &mut StringInterner) -> Result<ConstPool> {
    let length = read_u16(input)?;
    let mut vec = Vec::with_capacity(length as usize);
    vec.push(ConstPoolItem::DumbPlaceholderAfterLongOrDoubleEntryOrForEntryZero);
    while (vec.len() as u16) < length {
        vec.push(read_const_pool_item(input, str_interner)?);
        if matches!(
            vec.last().unwrap(),
            ConstPoolItem::Long(_) | ConstPoolItem::Double(_)
        ) {
            vec.push(ConstPoolItem::DumbPlaceholderAfterLongOrDoubleEntryOrForEntryZero)
        }
    }
    Ok(ConstPool(vec))
}

fn read_interfaces(input: &mut &[u8]) -> Result<Vec<u16>> {
    let length = read_u16(input)?;
    let mut vec = Vec::with_capacity(length as usize);
    for _ in 0..length {
        vec.push(read_u16(input)?);
    }
    Ok(vec)
}

fn read_attribute<'input, 'cp>(
    input: &mut &'input [u8],
    const_pool: &'cp ConstPool,
) -> Result<(InternedString, &'input [u8])> {
    let name = const_pool.get_utf8(read_u16(input)?)?;
    let attr_length = read_u32(input)?;
    let attr_bytes = &input[0..attr_length as usize];
    *input = &input[attr_length as usize..];
    Ok((name, attr_bytes))
}

fn read_field(
    input: &mut &[u8],
    class: InternedString,
    constant_pool: &ConstPool,
) -> Result<Field> {
    let access_flags = read_u16(input)?;

    let name_index = read_u16(input)?;
    let name = constant_pool.get_utf8(name_index)?;

    let descriptor = read_u16(input)?;

    let attributes_count = read_u16(input)?;
    for _ in 0..attributes_count {
        // Ignore attributes
        let _ = read_attribute(input, constant_pool);
    }

    Ok(Field {
        class,
        name,
        access_flags,
        descriptor,
    })
}

fn read_fields(
    input: &mut &[u8],
    class: InternedString,
    constant_pool: &ConstPool,
) -> Result<HashMap<InternedString, Field>> {
    let length = read_u16(input)?;
    let mut fields = HashMap::with_capacity(length as usize);
    for _ in 0..length {
        let field = read_field(input, class, constant_pool)?;
        fields.insert(field.name, field);
    }
    Ok(fields)
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

    let exception_table_length = read_u32(input)?;
    for _ in 0..exception_table_length {
        // TODO: unsupport exceptions
        read_u64(input)?;
    }

    let attributes_count = read_u16(input)?;
    for _ in 0..attributes_count {
        // Ignore attributes for now
        let _ = read_attribute(input, constant_pool)?;
    }

    Ok(Code {
        max_stack,
        max_locals,
        bytes,
    })
}

fn read_method(
    input: &mut &[u8],
    class: InternedString,
    constant_pool: &ConstPool,
    str_interner: &StringInterner,
) -> Result<Method> {
    let access_flags = read_u16(input)?;

    let name = constant_pool.get_utf8(read_u16(input)?)?;

    let descriptor = read_u16(input)?;

    let mut code = None;
    let attributes_count = read_u16(input)?;
    for _ in 0..attributes_count {
        let (attr_name, bytes) = read_attribute(input, constant_pool)?;
        if Some(attr_name) == str_interner.get("Code") {
            if code.is_none() {
                code = Some(read_code(bytes, constant_pool).context("Reading bytecode")?);
            } else {
                bail!("Multiple code attributes on same method")
            }
        }
    }

    Ok(Method {
        class,
        name,
        access_flags,
        descriptor,
        code,
    })
}

fn read_methods(
    input: &mut &[u8],
    class: InternedString,
    constant_pool: &ConstPool,
    str_interner: &StringInterner,
) -> Result<HashMap<InternedString, Method>> {
    let length = read_u16(input)?;
    let mut methods = HashMap::with_capacity(length as usize);
    for _ in 0..length {
        let method = read_method(input, class, constant_pool, str_interner)?;
        methods.insert(method.name, method);
    }
    Ok(methods)
}

pub fn read_class_file(mut input: &[u8], str_interner: &mut StringInterner) -> Result<Classfile> {
    let input = &mut input;

    read_magic(input)?;
    let minor_version = read_u16(input)?;
    let major_version = read_u16(input)?;

    let const_pool = read_const_pool(input, str_interner)?;

    let access_flags = read_u16(input)?;

    let this_class = const_pool.get_class(read_u16(input)?)?;
    let super_class = const_pool.get_class(read_u16(input)?)?;

    let interfaces = read_interfaces(input)?;
    let fields = read_fields(input, this_class, &const_pool).context("Reading fields")?;
    let methods =
        read_methods(input, this_class, &const_pool, str_interner).context("Reading methods")?;

    Ok(Classfile {
        version: (major_version, minor_version),
        const_pool,
        access_flags,
        name: this_class,
        super_class,
        interfaces,
        fields,
        methods,
    })
}
