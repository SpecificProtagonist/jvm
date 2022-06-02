use std::sync::Arc;

use jvm::*;

#[test]
fn circular_loading() {
    let jvm = JVM::new(DefaultClassLoader::new_boxed(["classes", "tests/classes"]));
    assert_eq!(
        jvm.resolve_class("CircularA")
            .unwrap_err()
            .class()
            .name()
            .get(),
        "java/lang/ClassCircularityError"
    )
}

#[test]
fn initialization() {
    let jvm = JVM::new(DefaultClassLoader::new_boxed(["classes", "tests/classes"]));
    let method = jvm
        .resolve_method("Initialization", "check_init", vec![], Some(Typ::Bool))
        .unwrap();
    assert_eq!(jvm.invoke(method, &[]).unwrap(), Some(JVMValue::Int(1)));
}

#[test]
fn init_lock() {
    let jvm = Arc::new(JVM::new(DefaultClassLoader::new_boxed([
        "classes",
        "tests/classes",
    ])));
    let class = Arc::new(JVM::resolve_class(&jvm, "InitLock").unwrap());
    // TODO: use scoped threads when stabilized
    let threads = (0..50)
        .map(|_| {
            let jvm = jvm.clone();
            let class = class.clone();
            std::thread::spawn(move || {
                let method = class
                    .method(&MethodNaT {
                        name: jvm.intern_str("check"),
                        typ: &MethodDescriptor(vec![], Some(Typ::Bool)),
                    })
                    .unwrap();
                assert_eq!(jvm.invoke(method, &[]).unwrap(), Some(JVMValue::Int(1)));
            })
        })
        .collect::<Vec<_>>();
    for thread in threads {
        thread.join().unwrap()
    }
}

#[test]
fn control_flow() {
    let jvm = JVM::new(DefaultClassLoader::new_boxed(["classes", "tests/classes"]));
    let method = jvm
        .resolve_method("ControlFlow", "is_even", vec![Typ::Int], Some(Typ::Bool))
        .unwrap();
    let even_10 = jvm.invoke(method, &[JVMValue::Int(10)]).unwrap();
    assert_eq!(even_10, Some(JVMValue::Int(1)));
    let even_13 = jvm.invoke(method, &[JVMValue::Int(13)]).unwrap();
    assert_eq!(even_13, Some(JVMValue::Int(0)));
}

#[test]
fn field_access() {
    let jvm = JVM::new(DefaultClassLoader::new_boxed(["classes", "tests/classes"]));
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let class = jvm.resolve_class("FieldAccess").unwrap();
    let set_method = class
        .method(&MethodNaT {
            name: jvm.intern_str("set"),
            typ: &MethodDescriptor(vec![Typ::Int], None),
        })
        .unwrap();
    let get_method = class
        .method(&MethodNaT {
            name: jvm.intern_str("get"),
            typ: &MethodDescriptor(vec![], Some(Typ::Int)),
        })
        .unwrap();
    assert_eq!(jvm.invoke(set_method, &[JVMValue::Int(42)]).unwrap(), None);
    assert_eq!(
        jvm.invoke(get_method, &[]).unwrap(),
        Some(JVMValue::Int(42))
    );
}

#[test]
fn invoke_static() {
    let jvm = JVM::new(DefaultClassLoader::new_boxed(["classes", "tests/classes"]));
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let method = jvm
        .resolve_method(
            "InvokeStatic",
            "test",
            vec![Typ::Bool, Typ::Bool],
            Some(Typ::Bool),
        )
        .unwrap();
    assert_eq!(
        jvm.invoke(method, &[JVMValue::Int(1), JVMValue::Int(0)])
            .unwrap(),
        Some(JVMValue::Int(1))
    );
}

#[test]
fn invoke_virtual() {
    let jvm = JVM::new(DefaultClassLoader::new_boxed(["classes", "tests/classes"]));
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let method = jvm
        .resolve_method("InvokeVirtual", "test", vec![], Some(Typ::Bool))
        .unwrap();
    assert_eq!(jvm.invoke(method, &[]).unwrap(), Some(JVMValue::Int(1)));
}

#[test]
fn arrays() {
    let jvm = JVM::new(DefaultClassLoader::new_boxed(["classes", "tests/classes"]));
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let method = jvm
        .resolve_method("Arrays", "test", vec![], Some(Typ::Bool))
        .unwrap();
    assert_eq!(jvm.invoke(method, &[]).unwrap(), Some(JVMValue::Int(1)));
}

/// Lazy resolution isn't mandatory for the spec, but I still want it
#[test]
fn lazy_resolve() {
    let jvm = JVM::new(DefaultClassLoader::new_boxed(["classes", "tests/classes"]));
    jvm.resolve_class("LazyResolve").unwrap();
}

#[test]
fn lazy_init() {
    let jvm = JVM::new(DefaultClassLoader::new_boxed(["classes", "tests/classes"]));
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let method = jvm
        .resolve_method("LazyInit", "test", vec![Typ::Bool], Some(Typ::Int))
        .unwrap();
    assert_eq!(
        jvm.invoke(method, &[JVMValue::Int(0)]).unwrap(),
        Some(JVMValue::Int(1))
    );

    let jvm = JVM::new(DefaultClassLoader::new_boxed(["classes", "tests/classes"]));
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let method = jvm
        .resolve_method("LazyInit", "test", vec![Typ::Bool], Some(Typ::Int))
        .unwrap();
    assert_eq!(
        jvm.invoke(method, &[JVMValue::Int(1)]).unwrap(),
        Some(JVMValue::Int(2))
    );
}

#[test]
fn many_allocs() {
    let jvm = JVM::new(DefaultClassLoader::new_boxed(["classes", "tests/classes"]));
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let method = jvm
        .resolve_method("ManyAllocs", "test", vec![], None)
        .unwrap();
    assert_eq!(jvm.invoke(method, &[]).unwrap(), None);
}

#[test]
fn exceptions() {
    let jvm = JVM::new(DefaultClassLoader::new_boxed(["classes", "tests/classes"]));
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let method = jvm
        .resolve_method("Exceptions", "test", vec![Typ::Bool], Some(Typ::Bool))
        .unwrap();
    assert_eq!(
        jvm.invoke(method, &[JVMValue::Int(0)]).unwrap(),
        Some(JVMValue::Int(0))
    );
    assert_eq!(
        jvm.invoke(method, &[JVMValue::Int(1)]).unwrap(),
        Some(JVMValue::Int(1))
    );
}
