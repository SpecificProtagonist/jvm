use jvm::interp::ReturnValue;
use jvm::*;

#[test]
fn field_access() -> anyhow::Result<()> {
    let jvm = JVM::new(vec!["classes".into(), "tests/pass".into()]);
    let boolean = jvm.intern_type(Typ::Bool);
    let class = jvm.resolve_class(jvm.intern_str("InvokeVirtual".into()))?;
    let method = class
        .method(
            jvm.intern_str("test".into()),
            &MethodDescriptor(vec![], Some(boolean)),
        )
        .unwrap();
    assert_eq!(interp::run(&jvm, method, &[])?, ReturnValue::Int(1));
    Ok(())
}
