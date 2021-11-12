use jvm::interp::{LocalValue, ReturnValue};
use jvm::*;

#[test]
fn invoke_static() -> anyhow::Result<()> {
    let jvm = JVM::new(vec!["classes".into(), "tests/pass".into()]);
    let bool = jvm.intern_type(Typ::Bool);
    let class = jvm.resolve_class(jvm.intern_str("InvokeStatic"))?;
    let method = class
        .method(
            jvm.intern_str("test"),
            &MethodDescriptor(vec![bool, bool], Some(bool)),
        )
        .unwrap();
    assert_eq!(
        interp::run(&jvm, method, &[LocalValue::Int(1), LocalValue::Int(0)])?,
        ReturnValue::Int(1)
    );

    Ok(())
}
