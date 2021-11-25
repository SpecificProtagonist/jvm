use jvm::interp::{LocalValue, ReturnValue};
use jvm::*;

#[test]
fn circular_loading() {
    let jvm = JVM::new(vec!["classes".into(), "tests/fail".into()]);
    let name = jvm.intern_str("CircularA");
    assert!(jvm
        .resolve_class(name)
        .unwrap_err()
        .to_string()
        .starts_with("Circular"));
}

#[test]
fn field_access() -> anyhow::Result<()> {
    let jvm = JVM::new(vec!["classes".into(), "tests/pass".into()]);
    let class = jvm.resolve_class(jvm.intern_str("FieldAccess".into()))?;
    let set_method = class
        .method(
            jvm.intern_str("set".into()),
            &MethodDescriptor(vec![Typ::Int], None),
        )
        .unwrap();
    let get_method = class
        .method(
            jvm.intern_str("get".into()),
            &MethodDescriptor(vec![], Some(Typ::Int)),
        )
        .unwrap();
    assert_eq!(
        interp::run(&jvm, set_method, &[LocalValue::Int(42)])?,
        ReturnValue::Void
    );
    assert_eq!(interp::run(&jvm, get_method, &[])?, ReturnValue::Int(42));
    Ok(())
}

#[test]
fn init() -> anyhow::Result<()> {
    let jvm = JVM::new(vec!["classes".into(), "tests/pass".into()]);
    let class = jvm.resolve_class(jvm.intern_str("Initialization"))?;
    let method = class
        .method(
            jvm.intern_str("check_init"),
            &MethodDescriptor(vec![], Some(Typ::Bool)),
        )
        .unwrap();
    assert_eq!(interp::run(&jvm, method, &[])?, ReturnValue::Int(1));

    Ok(())
}

#[test]
fn invoke_static() -> anyhow::Result<()> {
    let jvm = JVM::new(vec!["classes".into(), "tests/pass".into()]);
    let class = jvm.resolve_class(jvm.intern_str("InvokeStatic"))?;
    let method = class
        .method(
            jvm.intern_str("test"),
            &MethodDescriptor(vec![Typ::Bool, Typ::Bool], Some(Typ::Bool)),
        )
        .unwrap();
    assert_eq!(
        interp::run(&jvm, method, &[LocalValue::Int(1), LocalValue::Int(0)])?,
        ReturnValue::Int(1)
    );

    Ok(())
}

#[test]
fn invoke_virtual() -> anyhow::Result<()> {
    let jvm = JVM::new(vec!["classes".into(), "tests/pass".into()]);
    let class = jvm.resolve_class(jvm.intern_str("InvokeVirtual".into()))?;
    let method = class
        .method(
            jvm.intern_str("test".into()),
            &MethodDescriptor(vec![], Some(Typ::Bool)),
        )
        .unwrap();
    assert_eq!(interp::run(&jvm, method, &[])?, ReturnValue::Int(1));
    Ok(())
}

#[test]
fn arrays() -> anyhow::Result<()> {
    let jvm = JVM::new(vec!["classes".into(), "tests/pass".into()]);
    let class = jvm.resolve_class(jvm.intern_str("Arrays".into()))?;
    let method = class
        .method(
            jvm.intern_str("test".into()),
            &MethodDescriptor(vec![], Some(Typ::Bool)),
        )
        .unwrap();
    assert_eq!(interp::run(&jvm, method, &[])?, ReturnValue::Int(1));
    Ok(())
}
