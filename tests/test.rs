use jvm::*;

fn make_jvm() -> Jvm {
    Jvm::new(FolderClassLoader::new_boxed(["classes", "tests/classes"]))
}

#[test]
fn circular_loading() {
    let jvm = make_jvm();
    assert_eq!(
        jvm.resolve_class("CircularA").unwrap_err().class().name(),
        "java.lang.ClassCircularityError"
    )
}

#[test]
fn initialization() {
    let jvm = make_jvm();
    let method = jvm
        .resolve_method("Initialization", "check_init", [], Some(Typ::Boolean))
        .unwrap();
    assert_eq!(method.invoke(&[]).unwrap(), true);
}

#[test]
fn init_lock() {
    let jvm = make_jvm();
    let class = Jvm::resolve_class(&jvm, "InitLock").unwrap();
    std::thread::scope(|s| {
        for _ in 0..100 {
            s.spawn(|| {
                let method = class.method("check", [], Some(Typ::Boolean)).unwrap();
                assert_eq!(method.invoke(&[]).unwrap(), true);
            });
        }
    });
}

#[test]
fn control_flow() {
    let jvm = make_jvm();
    let is_even = jvm
        .resolve_method("ControlFlow", "is_even", [Typ::Int], Some(Typ::Boolean))
        .unwrap();
    let even_10 = is_even.invoke(&[10.into()]).unwrap();
    assert_eq!(even_10, true);
    let even_13 = is_even.invoke(&[13.into()]).unwrap();
    assert_eq!(even_13, false);
}

#[test]
fn field_access_simple() {
    let jvm = make_jvm();
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let class = jvm.resolve_class("FieldAccess").unwrap();
    let set_method = class.method("set", [Typ::Int], None).unwrap();
    let get_method = class.method("get", [], Some(Typ::Int)).unwrap();
    assert_eq!(set_method.invoke(&[42.into()]).unwrap(), ());
    assert_eq!(get_method.invoke(&[]).unwrap(), 42);
}

#[test]
fn field_access_inheritance() {
    let jvm = make_jvm();
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
    let jvm = make_jvm();
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let method = jvm
        .resolve_method(
            "InvokeStatic",
            "test",
            [Typ::Boolean, Typ::Boolean],
            Some(Typ::Boolean),
        )
        .unwrap();
    assert_eq!(method.invoke(&[true.into(), false.into()]).unwrap(), true);
}

#[test]
fn invoke_virtual() {
    let jvm = make_jvm();
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let method = jvm
        .resolve_method("InvokeVirtual", "test", [], Some(Typ::Boolean))
        .unwrap();
    assert_eq!(method.invoke(&[]).unwrap(), true);
}

#[test]
fn arrays() {
    let jvm = make_jvm();
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let method = jvm
        .resolve_method("Arrays", "test", [], Some(Typ::Boolean))
        .unwrap();
    assert_eq!(method.invoke(&[]).unwrap(), true);
}

/// Lazy resolution isn't mandatory for the spec, but I still want it
#[test]
fn lazy_resolve() {
    let jvm = make_jvm();
    jvm.resolve_class("LazyResolve").unwrap();
}

#[test]
fn lazy_init() {
    let jvm = make_jvm();
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let method = jvm
        .resolve_method("LazyInit", "test", [Typ::Boolean], Some(Typ::Int))
        .unwrap();
    assert_eq!(method.invoke(&[false.into()]).unwrap(), 1);

    let jvm = make_jvm();
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let method = jvm
        .resolve_method("LazyInit", "test", [Typ::Boolean], Some(Typ::Int))
        .unwrap();
    assert_eq!(method.invoke(&[true.into()]).unwrap(), 2);
}

#[test]
fn many_allocs() {
    let jvm = make_jvm();
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let method = jvm.resolve_method("ManyAllocs", "test", [], None).unwrap();
    assert_eq!(method.invoke(&[]).unwrap(), ());
}

#[test]
fn exceptions() {
    let jvm = make_jvm();
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let method = jvm
        .resolve_method("Exceptions", "test", [Typ::Boolean], Some(Typ::Boolean))
        .unwrap();
    assert_eq!(method.invoke(&[false.into()]).unwrap(), false);
    assert_eq!(method.invoke(&[true.into()]).unwrap(), true);
}

#[test]
fn strings() {
    let jvm = make_jvm();
    unsafe {
        // TODO: implement enough verification
        jvm.disable_verification_by_type_checking()
    }
    let class = jvm.resolve_class("Strings").unwrap();
    class.ensure_init().unwrap();
    let Value::Ref(Some(string)) = class
        .field("foo", Typ::Ref("java.lang.String".into()))
        .unwrap()
        .static_get()
        else { panic!() };
    let backing_field = jvm
        .resolve_class("java.lang.String")
        .unwrap()
        .field("chars", Typ::Ref("char[]".into()))
        .unwrap();
    let Value::Ref(Some(chars)) = backing_field.instance_get(string)
        else { panic!() };
    assert_eq!(format!("{:?}", chars), "\"bar\"");
}

#[test]
#[should_panic]
fn cross_jvms_field() {
    let jvm_1 = make_jvm();
    let jvm_2 = make_jvm();

    let object_from_1 = jvm_1
        .resolve_class("java.lang.Object")
        .unwrap()
        .create_object_uninit();

    let class_from_2 = jvm_2.resolve_class("CrossJvm").unwrap();
    let field = class_from_2
        .field("field", Typ::Ref("java.lang.Object".into()))
        .unwrap();
    field.static_set(Some(object_from_1));
}

#[test]
#[should_panic]
fn cross_jvms_method() {
    let jvm_1 = make_jvm();
    let jvm_2 = make_jvm();

    let object_from_1 = jvm_1
        .resolve_class("java.lang.Object")
        .unwrap()
        .create_object_uninit();

    let class_from_2 = jvm_2.resolve_class("CrossJvm").unwrap();
    let method = class_from_2
        .method("method", [Typ::Ref("java.lang.Object".into())], None)
        .unwrap();
    _ = method.invoke(&[Some(object_from_1).into()]);
}
