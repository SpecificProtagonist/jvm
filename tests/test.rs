use std::sync::Arc;

use anyhow::Result;
use jvm::interp::JVMValue;
use jvm::*;

#[test]
fn circular_loading() {
    let jvm = JVM::new(vec!["classes".into(), "tests".into()]);
    assert!(jvm
        .resolve_class("CircularA")
        .unwrap_err()
        .to_string()
        .starts_with("Circular"));
}

#[test]
fn initialization() -> Result<()> {
    let jvm = JVM::new(vec!["classes".into(), "tests".into()]);
    let method = jvm.resolve_method("Initialization", "check_init", vec![], Some(Typ::Bool))?;
    assert_eq!(interp::invoke(&jvm, method, &[])?, Some(JVMValue::Int(1)));
    Ok(())
}

#[test]
fn init_lock() -> Result<()> {
    let jvm = Arc::new(JVM::new(vec!["classes".into(), "tests".into()]));
    let class = Arc::new(JVM::resolve_class(&jvm, "InitLock")?);
    // TODO: use scoped threads when stabilized
    let threads = (0..50)
        .map(|_| {
            let jvm = jvm.clone();
            let class = class.clone();
            std::thread::spawn(move || {
                let method = class
                    .method(
                        jvm.intern_str("check"),
                        &MethodDescriptor(vec![], Some(Typ::Bool)),
                    )
                    .unwrap();
                assert_eq!(
                    interp::invoke(&jvm, method, &[]).unwrap(),
                    Some(JVMValue::Int(1))
                );
            })
        })
        .collect::<Vec<_>>();
    for thread in threads {
        thread.join().unwrap()
    }
    Ok(())
}

#[test]
fn control_flow() -> Result<()> {
    let jvm = JVM::new(vec!["classes".into(), "tests".into()]);
    let method = jvm.resolve_method("ControlFlow", "is_even", vec![Typ::Int], Some(Typ::Bool))?;
    let even_10 = interp::invoke(&jvm, method, &[JVMValue::Int(10)])?;
    assert_eq!(even_10, Some(JVMValue::Int(1)));
    let even_13 = interp::invoke(&jvm, method, &[JVMValue::Int(13)])?;
    assert_eq!(even_13, Some(JVMValue::Int(0)));
    Ok(())
}

#[test]
fn field_access() -> Result<()> {
    let jvm = JVM::new(vec!["classes".into(), "tests".into()]);
    let class = jvm.resolve_class("FieldAccess")?;
    let set_method = class
        .method(
            jvm.intern_str("set"),
            &MethodDescriptor(vec![Typ::Int], None),
        )
        .unwrap();
    let get_method = class
        .method(
            jvm.intern_str("get"),
            &MethodDescriptor(vec![], Some(Typ::Int)),
        )
        .unwrap();
    assert_eq!(
        interp::invoke(&jvm, set_method, &[JVMValue::Int(42)])?,
        None
    );
    assert_eq!(
        interp::invoke(&jvm, get_method, &[])?,
        Some(JVMValue::Int(42))
    );
    Ok(())
}

#[test]
fn invoke_static() -> Result<()> {
    let jvm = JVM::new(vec!["classes".into(), "tests".into()]);
    let method = jvm.resolve_method(
        "InvokeStatic",
        "test",
        vec![Typ::Bool, Typ::Bool],
        Some(Typ::Bool),
    )?;
    assert_eq!(
        interp::invoke(&jvm, method, &[JVMValue::Int(1), JVMValue::Int(0)])?,
        Some(JVMValue::Int(1))
    );

    Ok(())
}

#[test]
fn invoke_virtual() -> Result<()> {
    let jvm = JVM::new(vec!["classes".into(), "tests".into()]);
    let method = jvm.resolve_method("InvokeVirtual", "test", vec![], Some(Typ::Bool))?;
    assert_eq!(interp::invoke(&jvm, method, &[])?, Some(JVMValue::Int(1)));
    Ok(())
}

#[test]
fn arrays() -> Result<()> {
    let jvm = JVM::new(vec!["classes".into(), "tests".into()]);
    let method = jvm.resolve_method("Arrays", "test", vec![], Some(Typ::Bool))?;
    assert_eq!(interp::invoke(&jvm, method, &[])?, Some(JVMValue::Int(1)));
    Ok(())
}

/// Lazy resolution isn't mandatory for the spec, but I still want it
#[test]
fn lazy_resolve() -> Result<()> {
    let jvm = JVM::new(vec!["classes".into(), "tests".into()]);
    jvm.resolve_class("LazyResolve")?;
    Ok(())
}

#[test]
fn lazy_init() -> Result<()> {
    let jvm = JVM::new(vec!["classes".into(), "tests".into()]);
    let method = jvm.resolve_method("LazyInit", "test", vec![Typ::Bool], Some(Typ::Int))?;
    assert_eq!(
        interp::invoke(&jvm, method, &[JVMValue::Int(0)])?,
        Some(JVMValue::Int(1))
    );

    let jvm = JVM::new(vec!["classes".into(), "tests".into()]);
    let method = jvm.resolve_method("LazyInit", "test", vec![Typ::Bool], Some(Typ::Int))?;
    assert_eq!(
        interp::invoke(&jvm, method, &[JVMValue::Int(1)])?,
        Some(JVMValue::Int(2))
    );
    Ok(())
}

#[test]
fn many_allocs() -> Result<()> {
    let jvm = JVM::new(vec!["classes".into(), "tests".into()]);
    let method = jvm.resolve_method("ManyAllocs", "test", vec![], None)?;
    assert_eq!(interp::invoke(&jvm, method, &[])?, None);
    Ok(())
}
