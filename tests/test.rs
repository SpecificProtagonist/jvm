use jvm::*;

#[test]
fn circular_loading() {
    let jvm = Jvm::new(FolderClassLoader::new_boxed(["classes", "tests/classes"]));
    assert_eq!(
        jvm.resolve_class("CircularA").unwrap_err().class().name(),
        "java/lang/ClassCircularityError"
    )
}

#[test]
fn initialization() {
    let jvm = Jvm::new(FolderClassLoader::new_boxed(["classes", "tests/classes"]));
    let method = jvm
        .resolve_method("Initialization", "check_init", vec![], Some(Typ::Boolean))
        .unwrap();
    assert_eq!(method.invoke(&[]).unwrap().unwrap(), true);
}

#[test]
fn init_lock() {
    let jvm = Jvm::new(FolderClassLoader::new_boxed(["classes", "tests/classes"]));
    let class = Jvm::resolve_class(&jvm, "InitLock").unwrap();
    std::thread::scope(|s| {
        for _ in 0..100 {
            s.spawn(|| {
                let method = class.method("check", vec![], Some(Typ::Boolean)).unwrap();
                assert_eq!(method.invoke(&[]).unwrap().unwrap(), true);
            });
        }
    });
}

#[test]
fn control_flow() {
    let jvm = Jvm::new(FolderClassLoader::new_boxed(["classes", "tests/classes"]));
    let is_even = jvm
        .resolve_method("ControlFlow", "is_even", vec![Typ::Int], Some(Typ::Boolean))
        .unwrap();
    let even_10 = is_even.invoke(&[10.into()]).unwrap();
    assert_eq!(even_10, Some(true.into()));
    let even_13 = is_even.invoke(&[13.into()]).unwrap();
    assert_eq!(even_13, Some(false.into()));
}

#[test]
fn field_access() {
    let jvm = Jvm::new(FolderClassLoader::new_boxed(["classes", "tests/classes"]));
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let class = jvm.resolve_class("FieldAccess").unwrap();
    let set_method = class.method("set", vec![Typ::Int], None).unwrap();
    let get_method = class.method("get", vec![], Some(Typ::Int)).unwrap();
    assert_eq!(set_method.invoke(&[42.into()]).unwrap(), None);
    assert_eq!(get_method.invoke(&[]).unwrap().unwrap(), 42);
}

#[test]
fn field_access_inheritance() {
    let jvm = Jvm::new(FolderClassLoader::new_boxed(["classes", "tests/classes"]));
    let class = jvm.resolve_class("FieldAccessInheritance").unwrap();
    let super_class = class.super_class().unwrap();
    let a = super_class.field("a", Typ::Int).unwrap();
    let b = class.field("b", Typ::Int).unwrap();
    let instance = class.create_object_uninit();
    a.instance_set(instance, 1);
    b.instance_set(instance, 2);
    assert_eq!(a.instance_get(instance), 1);
    assert_eq!(b.instance_get(instance), 2);
}

#[test]
fn invoke_static() {
    let jvm = Jvm::new(FolderClassLoader::new_boxed(["classes", "tests/classes"]));
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let method = jvm
        .resolve_method(
            "InvokeStatic",
            "test",
            vec![Typ::Boolean, Typ::Boolean],
            Some(Typ::Boolean),
        )
        .unwrap();
    assert_eq!(
        method.invoke(&[true.into(), false.into()]).unwrap(),
        Some(true.into())
    );
}

#[test]
fn invoke_virtual() {
    let jvm = Jvm::new(FolderClassLoader::new_boxed(["classes", "tests/classes"]));
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let method = jvm
        .resolve_method("InvokeVirtual", "test", vec![], Some(Typ::Boolean))
        .unwrap();
    assert_eq!(method.invoke(&[]).unwrap(), Some(true.into()));
}

#[test]
fn arrays() {
    let jvm = Jvm::new(FolderClassLoader::new_boxed(["classes", "tests/classes"]));
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let method = jvm
        .resolve_method("Arrays", "test", vec![], Some(Typ::Boolean))
        .unwrap();
    assert_eq!(method.invoke(&[]).unwrap(), Some(true.into()));
}

/// Lazy resolution isn't mandatory for the spec, but I still want it
#[test]
fn lazy_resolve() {
    let jvm = Jvm::new(FolderClassLoader::new_boxed(["classes", "tests/classes"]));
    jvm.resolve_class("LazyResolve").unwrap();
}

#[test]
fn lazy_init() {
    let jvm = Jvm::new(FolderClassLoader::new_boxed(["classes", "tests/classes"]));
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let method = jvm
        .resolve_method("LazyInit", "test", vec![Typ::Boolean], Some(Typ::Int))
        .unwrap();
    assert_eq!(method.invoke(&[0.into()]).unwrap(), Some(1.into()));

    let jvm = Jvm::new(FolderClassLoader::new_boxed(["classes", "tests/classes"]));
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let method = jvm
        .resolve_method("LazyInit", "test", vec![Typ::Boolean], Some(Typ::Int))
        .unwrap();
    assert_eq!(method.invoke(&[true.into()]).unwrap(), Some(2.into()));
}

#[test]
fn many_allocs() {
    let jvm = Jvm::new(FolderClassLoader::new_boxed(["classes", "tests/classes"]));
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let method = jvm
        .resolve_method("ManyAllocs", "test", vec![], None)
        .unwrap();
    assert_eq!(method.invoke(&[]).unwrap(), None);
}

#[test]
fn exceptions() {
    let jvm = Jvm::new(FolderClassLoader::new_boxed(["classes", "tests/classes"]));
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let method = jvm
        .resolve_method("Exceptions", "test", vec![Typ::Boolean], Some(Typ::Boolean))
        .unwrap();
    assert_eq!(method.invoke(&[false.into()]).unwrap(), Some(false.into()));
    assert_eq!(method.invoke(&[true.into()]).unwrap(), Some(true.into()));
}

#[test]
fn strings() {
    let jvm = Jvm::new(FolderClassLoader::new_boxed(["classes", "tests/classes"]));
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let class = jvm.resolve_class("Strings").unwrap();
    // Ensure the class is initialized
    class.ensure_init().unwrap();
    let string = match class
        .field("foo", Typ::Ref("java/lang/String".into()))
        .unwrap()
        .static_get()
    {
        Value::Ref(Some(string)) => string,
        _ => panic!(),
    };
    let backing_field = jvm
        .resolve_class("java/lang/String")
        .unwrap()
        .field("chars", Typ::Ref("[C".into()))
        .unwrap();
    let chars = match backing_field.instance_get(string) {
        Value::Ref(Some(chars)) => chars,
        _ => panic!(),
    };
    assert_eq!(format!("{:?}", chars), "\"bar\"");
}

// TODO: Compile-fail test for interacting with objects of one JVM via another
