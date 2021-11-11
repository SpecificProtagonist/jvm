use jvm::interp::{LocalValue, ReturnValue};
use jvm::*;

#[test]
fn field_access() -> anyhow::Result<()> {
    let jvm = JVM::new(vec!["classes".into(), "tests/pass".into()]);
    let int = jvm.intern_type(Typ::Int);
    let class = jvm.resolve_class(jvm.intern_str("FieldAccess".into()))?;
    let set_method = class
        .method(
            jvm.intern_str("set".into()),
            &MethodDescriptor(vec![int], None),
        )
        .unwrap();
    let get_method = class
        .method(
            jvm.intern_str("get".into()),
            &MethodDescriptor(vec![], Some(int)),
        )
        .unwrap();
    assert_eq!(
        interp::run(&jvm, set_method, &[LocalValue::Int(42)])?,
        ReturnValue::Void
    );
    assert_eq!(interp::run(&jvm, get_method, &[])?, ReturnValue::Int(42));
    Ok(())
}
