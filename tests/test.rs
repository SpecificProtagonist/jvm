#![feature(closure_lifetime_binder)]
use std::sync::atomic::{AtomicBool, Ordering};

use jvm::*;

fn make_jvm() -> Jvm {
    Jvm::new(FolderClassLoader::new_boxed(["classes", "tests/classes"]))
}

#[test]
fn circular_loading() {
    let jvm = make_jvm();
    assert_eq!(
        format!("{:?}", jvm.resolve_class("CircularA").unwrap_err()),
        "java.lang.ClassCircularityError{message: \"ClassCircularityError: CircularA\"}"
    )
}

#[test]
fn initialization() {
    let jvm = make_jvm();
    let method = jvm
        .resolve_method("Initialization", "check_init", [], Some(Type::Boolean))
        .unwrap();
    assert_eq!(method.invoke(&jvm, &[]).unwrap().unwrap(), true);
}

#[test]
fn init_lock() {
    let jvm = make_jvm();
    let class = Jvm::resolve_class(&jvm, "InitLock").unwrap();
    std::thread::scope(|s| {
        for _ in 0..100 {
            s.spawn(|| {
                let method = class.method("check", [], Some(Type::Boolean)).unwrap();
                assert_eq!(method.invoke(&jvm, &[]).unwrap().unwrap(), true);
            });
        }
    });
}

#[test]
fn control_flow() {
    let jvm = make_jvm();
    let is_even = jvm
        .resolve_method("ControlFlow", "is_even", [Type::Int], Some(Type::Boolean))
        .unwrap();
    let even_10 = is_even.invoke(&jvm, &[10.into()]).unwrap();
    assert_eq!(even_10.unwrap(), true);
    let even_13 = is_even.invoke(&jvm, &[13.into()]).unwrap();
    assert_eq!(even_13.unwrap(), false);
}

#[test]
fn field_access_simple() {
    let jvm = make_jvm();
    let class = jvm.resolve_class("FieldAccess").unwrap();
    let set_method = class.method("set", [Type::Int], None).unwrap();
    let get_method = class.method("get", [], Some(Type::Int)).unwrap();
    assert_eq!(set_method.invoke(&jvm, &[42.into()]).unwrap(), None);
    assert_eq!(get_method.invoke(&jvm, &[]).unwrap().unwrap(), 42);
}

#[test]
fn field_access_inheritance() {
    let jvm = make_jvm();
    let class = jvm.resolve_class("FieldAccessInheritance").unwrap();
    let super_class = class.super_class().unwrap();
    let a = super_class.field("a", Type::Int).unwrap();
    let b = class.field("b", Type::Int).unwrap();
    let instance = class.create_object_uninit(&jvm);
    a.instance_set(instance, 1);
    b.instance_set(instance, 2);
    assert_eq!(a.instance_get(instance), 1);
    assert_eq!(b.instance_get(instance), 2);
}

#[test]
fn invoke_static() {
    let jvm = make_jvm();
    let method = jvm
        .resolve_method(
            "InvokeStatic",
            "test",
            [Type::Boolean, Type::Boolean],
            Some(Type::Boolean),
        )
        .unwrap();
    assert_eq!(
        method
            .invoke(&jvm, &[true.into(), false.into()])
            .unwrap()
            .unwrap(),
        true
    );
}

#[test]
fn invoke_virtual() {
    let jvm = make_jvm();
    let method = jvm
        .resolve_method("InvokeVirtual", "test", [], Some(Type::Boolean))
        .unwrap();
    assert_eq!(method.invoke(&jvm, &[]).unwrap().unwrap(), true);
}

#[test]
fn invoke_native() {
    let jvm = make_jvm();
    jvm.resolve_method(
        "Native",
        "call_static",
        [Type::Int, Type::Int],
        Some(Type::Long),
    )
    .unwrap()
    .bind_native_code(|a: i32, b: i32| Ok(a as i64 + b as i64))
    .unwrap();

    jvm.resolve_method("Native", "call_instance", [], None)
        .unwrap()
        .bind_native_code(|this: Object| {
            let field = this.class().field("value", Type::Long).unwrap();
            assert_eq!(field.instance_get(this), 1i64);
            Ok(())
        })
        .unwrap();

    let static_method = jvm
        .resolve_method(
            "Native",
            "call_static",
            [Type::Int, Type::Int],
            Some(Type::Long),
        )
        .unwrap();
    assert_eq!(
        static_method
            .invoke(&jvm, &[1.into(), 2.into()])
            .unwrap()
            .unwrap(),
        3i64
    );

    let object = jvm.create_object("Native", [], vec![]).unwrap();
    let instance_method = jvm
        .resolve_method("Native", "call_instance", [], None)
        .unwrap();
    instance_method
        .invoke(&jvm, &[Value::Ref(Some(object))])
        .unwrap();
}

#[test]
fn arrays() {
    let jvm = make_jvm();

    let method = jvm
        .resolve_method("Arrays", "test", [], Some(Type::Boolean))
        .unwrap();
    assert_eq!(method.invoke(&jvm, &[]).unwrap().unwrap(), true);

    let method = jvm
        .resolve_method("Arrays", "multianewarray", [], Some(Type::Boolean))
        .unwrap();
    assert_eq!(method.invoke(&jvm, &[]).unwrap().unwrap(), true);
}

/// Lazy resolution isn't mandatory for the spec, but I still want it
#[test]
fn lazy_resolve() {
    let jvm = make_jvm();
    // LazyResolve references LazyUnresolved, which does not exist
    jvm.resolve_class("LazyResolve").unwrap();
    assert!(jvm.resolve_class("LazyUnresolved").is_err());
}

#[test]
fn lazy_init() {
    let jvm = make_jvm();
    let method = jvm
        .resolve_method("LazyInit", "test", [Type::Boolean], Some(Type::Int))
        .unwrap();
    assert_eq!(method.invoke(&jvm, &[false.into()]).unwrap().unwrap(), 1);

    let jvm = make_jvm();
    let method = jvm
        .resolve_method("LazyInit", "test", [Type::Boolean], Some(Type::Int))
        .unwrap();
    assert_eq!(method.invoke(&jvm, &[true.into()]).unwrap().unwrap(), 2);
}

#[test]
fn many_allocs() {
    let jvm = make_jvm();
    let method = jvm.resolve_method("ManyAllocs", "test", [], None).unwrap();
    assert_eq!(method.invoke(&jvm, &[]).unwrap(), None);
}

#[test]
fn exceptions() {
    let jvm = make_jvm();
    let method = jvm
        .resolve_method("Exceptions", "test", [Type::Boolean], Some(Type::Boolean))
        .unwrap();
    assert_eq!(
        method.invoke(&jvm, &[false.into()]).unwrap().unwrap(),
        false
    );
    assert_eq!(method.invoke(&jvm, &[true.into()]).unwrap().unwrap(), true);
}

#[test]
fn strings() {
    let jvm = make_jvm();
    let class = jvm.resolve_class("Strings").unwrap();
    class.ensure_init(&jvm).unwrap();
    let Value::Ref(Some(string)) = class
        .field("foo", Type::Ref("java.lang.String".into()))
        .unwrap()
        .static_get(&jvm)
    else {
        panic!()
    };
    assert_eq!(format!("{string:?}"), "\"bar\"");
}

#[test]
#[should_panic = "Wrong JVM accessed"]
fn cross_jvms_field() {
    let jvm_1 = make_jvm();
    let jvm_2 = make_jvm();

    let object_from_1 = jvm_1
        .resolve_class("java.lang.Object")
        .unwrap()
        .create_object_uninit(&jvm_1);

    let class_from_2 = jvm_2.resolve_class("CrossJvm").unwrap();
    let field = class_from_2
        .field("field", Type::Ref("java.lang.Object".into()))
        .unwrap();
    field.static_set(Some(object_from_1));
}

#[test]
#[should_panic = "Wrong JVM accessed"]
fn cross_jvms_method() {
    let jvm_1 = make_jvm();
    let jvm_2 = make_jvm();

    let object_from_1 = jvm_1
        .resolve_class("java.lang.Object")
        .unwrap()
        .create_object_uninit(&jvm_1);

    let class_from_2 = jvm_2.resolve_class("CrossJvm").unwrap();
    let method = class_from_2
        .method("method", [Type::Ref("java.lang.Object".into())], None)
        .unwrap();
    _ = method.invoke(&jvm_2, &[Some(object_from_1).into()]);
}

#[test]
fn hot_reloading() {
    static RELOADED: AtomicBool = AtomicBool::new(false);
    struct TestClassloader;
    impl ClassLoader for TestClassloader {
        fn load(&self, mut name: &str) -> Option<Vec<u8>> {
            if name == "HotReloading" {
                if RELOADED.load(Ordering::SeqCst) {
                    name = "HotReloading2";
                } else {
                    name = "HotReloading1"
                }
            }
            let loader = FolderClassLoader::new_boxed(["classes", "tests/classes"]);
            loader.load(name)
        }
    }
    let jvm = Jvm::new(Box::new(TestClassloader));

    let class = jvm.resolve_class("HotReloading").unwrap();
    let field = class.field("static_field", Type::Int).unwrap();
    field.static_set(1);
    let method = class.method("hot_reloadable", [], Some(Type::Int)).unwrap();
    assert_eq!(method.invoke(&jvm, &[]).unwrap().unwrap(), 1);

    RELOADED.store(true, Ordering::SeqCst);
    jvm.reload_method_bytecodes_for_class("HotReloading")
        .unwrap();
    assert_eq!(method.invoke(&jvm, &[]).unwrap().unwrap(), 2);
    assert_eq!(field.static_get(&jvm), 1);
}
