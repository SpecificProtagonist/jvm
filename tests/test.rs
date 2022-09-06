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
    assert_eq!(jvm.invoke(method, &[]).unwrap(), Some(1.into()));
}

#[test]
fn init_lock() {
    let jvm = JVM::new(DefaultClassLoader::new_boxed(["classes", "tests/classes"]));
    let class = JVM::resolve_class(&jvm, "InitLock").unwrap();
    std::thread::scope(|s| {
        for _ in 0..100 {
            s.spawn(|| {
                let method = class
                    .method(&MethodNaT {
                        name: jvm.intern_str("check"),
                        typ: &MethodDescriptor(vec![], Some(Typ::Bool)),
                    })
                    .unwrap();
                assert_eq!(jvm.invoke(method, &[]).unwrap(), Some(1.into()));
            });
        }
    });
}

#[test]
fn control_flow() {
    let jvm = JVM::new(DefaultClassLoader::new_boxed(["classes", "tests/classes"]));
    let method = jvm
        .resolve_method("ControlFlow", "is_even", vec![Typ::Int], Some(Typ::Bool))
        .unwrap();
    let even_10 = jvm.invoke(method, &[10.into()]).unwrap();
    assert_eq!(even_10, Some(1.into()));
    let even_13 = jvm.invoke(method, &[13.into()]).unwrap();
    assert_eq!(even_13, Some(0.into()));
}

#[test]
fn field_access() {
    let jvm = JVM::new(DefaultClassLoader::new_boxed(["classes", "tests/classes"]));
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
    assert_eq!(jvm.invoke(set_method, &[42.into()]).unwrap(), None);
    assert_eq!(jvm.invoke(get_method, &[]).unwrap(), Some(42.into()));
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
        jvm.invoke(method, &[1.into(), 0.into()]).unwrap(),
        Some(1.into())
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
    assert_eq!(jvm.invoke(method, &[]).unwrap(), Some(1.into()));
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
    assert_eq!(jvm.invoke(method, &[]).unwrap(), Some(1.into()));
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
    assert_eq!(jvm.invoke(method, &[0.into()]).unwrap(), Some(1.into()));

    let jvm = JVM::new(DefaultClassLoader::new_boxed(["classes", "tests/classes"]));
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let method = jvm
        .resolve_method("LazyInit", "test", vec![Typ::Bool], Some(Typ::Int))
        .unwrap();
    assert_eq!(jvm.invoke(method, &[1.into()]).unwrap(), Some(2.into()));
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
    assert_eq!(jvm.invoke(method, &[0.into()]).unwrap(), Some(0.into()));
    assert_eq!(jvm.invoke(method, &[1.into()]).unwrap(), Some(1.into()));
}

#[test]
fn strings() {
    let jvm = JVM::new(DefaultClassLoader::new_boxed(["classes", "tests/classes"]));
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let class = jvm.resolve_class("Strings").unwrap();
    // Ensure the class is initialized
    class.ensure_init(&jvm).unwrap();
    let string = match class
        .field(&jvm, "foo", Typ::Ref(jvm.intern_str("java/lang/String")))
        .unwrap()
        .static_get()
    {
        JVMValue::Ref(Some(string)) => string,
        _ => panic!(),
    };
    let backing_field = jvm
        .resolve_class("java/lang/String")
        .unwrap()
        .field(&jvm, "chars", Typ::Ref(jvm.intern_str("[C")))
        .unwrap();
    let chars = match backing_field.instance_get(string) {
        JVMValue::Ref(Some(chars)) => chars,
        _ => panic!(),
    };
    assert_eq!(format!("{:?}", chars), "\"bar\"");
}

#[test]
fn object_reference_niche() {
    assert_eq!(
        std::mem::size_of::<Object>(),
        std::mem::size_of::<Option<Object>>()
    )
}

// TODO: Compile-fail test for interacting with objects of one JVM via another
