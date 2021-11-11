use jvm::interp::ReturnValue;
use jvm::*;

#[test]
fn init() -> anyhow::Result<()> {
    let jvm = JVM::new(vec!["classes".into(), "tests/pass".into()]);
    let bool = jvm.intern_type(Typ::Bool);
    let class = jvm.resolve_class(jvm.intern_str("Initialization"))?;
    let method = class
        .method(
            jvm.intern_str("check_init"),
            &MethodDescriptor(vec![], Some(bool)),
        )
        .unwrap();
    assert_eq!(interp::run(&jvm, method, &[])?, ReturnValue::Int(1));

    Ok(())
}
