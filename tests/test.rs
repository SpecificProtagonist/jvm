use std::sync::Arc;

use anyhow::Result;
use jvm::interp::{LocalValue, ReturnValue};
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
    assert_eq!(interp::invoke(&jvm, method, &[])?, ReturnValue::Int(1));
    Ok(())
}

#[test]
fn init_lock() -> Result<()> {
    let jvm = Arc::new(JVM::new(vec!["classes".into(), "tests".into()]));
    let class = Arc::new(JVM::resolve_class(&jvm, "InitLock")?);
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
                    ReturnValue::Int(1)
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
    let method = jvm.resolve_method("ControlFlow", "test", vec![Typ::Int], Some(Typ::Bool))?;
    let even = interp::invoke(&jvm, method, &[LocalValue::Int(10)])?;
    let odd = interp::invoke(&jvm, method, &[LocalValue::Int(13)])?;
    assert_eq!(even, ReturnValue::Int(0));
    assert_eq!(odd, ReturnValue::Int(1));
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
        interp::invoke(&jvm, set_method, &[LocalValue::Int(42)])?,
        ReturnValue::Void
    );
    assert_eq!(interp::invoke(&jvm, get_method, &[])?, ReturnValue::Int(42));
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
        interp::invoke(&jvm, method, &[LocalValue::Int(1), LocalValue::Int(0)])?,
        ReturnValue::Int(1)
    );

    Ok(())
}

#[test]
fn invoke_virtual() -> Result<()> {
    let jvm = JVM::new(vec!["classes".into(), "tests".into()]);
    let method = jvm.resolve_method("InvokeVirtual", "test", vec![], Some(Typ::Bool))?;
    assert_eq!(interp::invoke(&jvm, method, &[])?, ReturnValue::Int(1));
    Ok(())
}

#[test]
fn arrays() -> Result<()> {
    let jvm = JVM::new(vec!["classes".into(), "tests".into()]);
    let method = jvm.resolve_method("Arrays", "test", vec![], Some(Typ::Bool))?;
    assert_eq!(interp::invoke(&jvm, method, &[])?, ReturnValue::Int(1));
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
        interp::invoke(&jvm, method, &[LocalValue::Int(0)])?,
        ReturnValue::Int(1)
    );

    let jvm = JVM::new(vec!["classes".into(), "tests".into()]);
    let method = jvm.resolve_method("LazyInit", "test", vec![Typ::Bool], Some(Typ::Int))?;
    assert_eq!(
        interp::invoke(&jvm, method, &[LocalValue::Int(1)])?,
        ReturnValue::Int(2)
    );
    Ok(())
}
