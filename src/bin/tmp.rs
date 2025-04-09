use jvm::*;

fn make_jvm() -> Jvm {
    let jvm = Jvm::new(
        FolderClassLoader::new_boxed(["classes", "tests/classes"]),
        2_000_000,
    );
    jvm.bind_provided_native_methods();
    jvm
}

fn main() {
    let jvm = make_jvm();
    let err = jvm
        .resolve_method("Stackoverflow", "overflow", [], None)
        .unwrap()
        .invoke(&jvm, &[])
        .unwrap_err();
    assert_eq!(err.class(&jvm,).name(&jvm), "java.lang.StackOverflowError");
}
