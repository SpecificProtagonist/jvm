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
