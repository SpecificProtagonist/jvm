#![feature(closure_lifetime_binder)]
use core::sync::atomic::{AtomicBool, Ordering::*};
use std::thread::scope;

use class_loader::file_loaders;
use vm::*;

fn make_jvm() -> Jvm {
    Jvm::new(
        file_loaders(["classes", "tests/classes"]),
        unsafe { classpath_support::get_integration() },
        2_000_000,
    )
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
    assert_eq!(method.invoke([]).unwrap().unwrap(), true);
}

#[test]
fn init_lock() {
    let jvm = make_jvm();
    let class = Jvm::resolve_class(&jvm, "InitLock").unwrap();
    std::thread::scope(|s| {
        for _ in 0..100 {
            s.spawn(|| {
                let method = class.method("check", [], Some(Type::Boolean)).unwrap();
                assert_eq!(method.invoke([]).unwrap().unwrap(), true);
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
    let even_10 = is_even.invoke([10.into()]).unwrap();
    assert_eq!(even_10.unwrap(), true);
    let even_13 = is_even.invoke([13.into()]).unwrap();
    assert_eq!(even_13.unwrap(), false);
}

#[test]
fn field_access_simple() {
    let jvm = make_jvm();
    let class = jvm.resolve_class("FieldAccess").unwrap();
    let set_method = class.method("set", [Type::Int], None).unwrap();
    let get_method = class.method("get", [], Some(Type::Int)).unwrap();
    assert_eq!(set_method.invoke([42.into()]).unwrap(), None);
    assert_eq!(get_method.invoke([]).unwrap().unwrap(), 42);
}

#[test]
fn field_access_inheritance() {
    let jvm = make_jvm();
    let class = jvm.resolve_class("FieldAccessInheritance").unwrap();
    let super_class = class.super_class().unwrap();
    let a = super_class.field("a", Type::Int).unwrap();
    let b = class.field("b", Type::Int).unwrap();
    let instance = class.create_object_no_constructor().unwrap();
    a.instance_set(&instance, 1);
    b.instance_set(&instance, 2);
    assert_eq!(a.instance_get(&instance), 1);
    assert_eq!(b.instance_get(&instance), 2);
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
        method.invoke([true.into(), false.into()]).unwrap().unwrap(),
        true
    );
}

#[test]
fn invoke_virtual() {
    let jvm = make_jvm();
    let method = jvm
        .resolve_method("InvokeVirtual", "test", [], Some(Type::Boolean))
        .unwrap();
    assert_eq!(method.invoke([]).unwrap().unwrap(), true);
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
    .bind_native_code(|a: i32, b: i32| a as i64 + b as i64)
    .unwrap();

    jvm.resolve_method("Native", "call_instance", [], None)
        .unwrap()
        .bind_native_code(|this: Object| {
            let field = this.class().field("value", Type::Long).unwrap();
            assert_eq!(field.instance_get(&this), 1i64);
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
        static_method.invoke([1.into(), 2.into()]).unwrap().unwrap(),
        3i64
    );

    let object = jvm.create_object("Native", [], vec![]).unwrap();
    let instance_method = jvm
        .resolve_method("Native", "call_instance", [], None)
        .unwrap();
    instance_method
        .invoke([Value::Object(Some(object))])
        .unwrap();
}

#[test]
fn access_control() {
    let jvm = make_jvm();
    let object = jvm.resolve_class("java.lang.Object").unwrap();
    let object_array = jvm.resolve_class("java.lang.Object[]").unwrap();
    let other_package = jvm.resolve_class("Arrays").unwrap();

    assert!(other_package.can_access_class(&object));
    assert!(other_package.can_access_class(&object_array));

    // TODO: more checks
}

#[test]
fn arrays() {
    let jvm = make_jvm();

    let method = jvm
        .resolve_method("Arrays", "char_array", [], Some(Type::Boolean))
        .unwrap();
    assert_eq!(method.invoke([]).unwrap().unwrap(), true);

    let method = jvm
        .resolve_method("Arrays", "double_array", [], Some(Type::Boolean))
        .unwrap();
    assert_eq!(method.invoke([]).unwrap().unwrap(), true);

    let method = jvm
        .resolve_method("Arrays", "object_array", [], Some(Type::Boolean))
        .unwrap();
    assert_eq!(method.invoke([]).unwrap().unwrap(), true);

    let method = jvm
        .resolve_method("Arrays", "multianewarray", [], Some(Type::Boolean))
        .unwrap();
    assert_eq!(method.invoke([]).unwrap().unwrap(), true);
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
    assert_eq!(method.invoke([false.into()]).unwrap().unwrap(), 1);

    let jvm = make_jvm();
    let method = jvm
        .resolve_method("LazyInit", "test", [Type::Boolean], Some(Type::Int))
        .unwrap();
    assert_eq!(method.invoke([true.into()]).unwrap().unwrap(), 2);
}

#[cfg(not(miri))]
#[test]
fn many_allocs() {
    let jvm = make_jvm();
    let method = jvm.resolve_method("ManyAllocs", "test", [], None).unwrap();
    assert_eq!(method.invoke([]).unwrap(), None);
}

#[test]
fn exceptions() {
    let jvm = make_jvm();
    let method = jvm
        .resolve_method("Exceptions", "test", [Type::Boolean], Some(Type::Boolean))
        .unwrap();
    assert_eq!(method.invoke([false.into()]).unwrap().unwrap(), false);
    assert_eq!(method.invoke([true.into()]).unwrap().unwrap(), true);
}

#[test]
#[should_panic = "Wrong JVM accessed"]
fn cross_jvms_field() {
    let jvm_1 = make_jvm();
    let jvm_2 = make_jvm();

    let object_from_1 = jvm_1
        .resolve_class("java.lang.Object")
        .unwrap()
        .create_object_no_constructor()
        .unwrap();

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
        .create_object_no_constructor()
        .unwrap();

    let class_from_2 = jvm_2.resolve_class("CrossJvm").unwrap();
    let method = class_from_2
        .method("method", [Type::Ref("java.lang.Object".into())], None)
        .unwrap();
    _ = method.invoke([Some(object_from_1).into()]);
}

#[test]
fn hot_reloading() {
    static RELOADED: AtomicBool = AtomicBool::new(false);
    struct TestClassloader;
    impl ClassLoader for TestClassloader {
        fn load(&self, mut name: &str) -> Option<Vec<u8>> {
            if name == "HotReloading" {
                if RELOADED.load(SeqCst) {
                    name = "HotReloading2";
                } else {
                    name = "HotReloading1"
                }
            }
            let loader = file_loaders(["classes", "tests/classes"]);
            loader.load(name)
        }
    }
    let jvm = Jvm::new(
        Box::new(TestClassloader),
        unsafe { classpath_support::get_integration() },
        1_000_000,
    );

    let class = jvm.resolve_class("HotReloading").unwrap();
    let field = class.field("static_field", Type::Int).unwrap();
    field.static_set(1);
    let method = class.method("hot_reloadable", [], Some(Type::Int)).unwrap();
    assert_eq!(method.invoke([]).unwrap().unwrap(), 1);

    RELOADED.store(true, SeqCst);
    jvm.reload_method_bytecodes_for_class("HotReloading")
        .unwrap();
    assert_eq!(method.invoke([]).unwrap().unwrap(), 2);
    assert_eq!(field.static_get(), 1);
}

#[cfg(not(miri))]
#[test]
fn stackoverflow() {
    let jvm = make_jvm();
    let err = jvm
        .resolve_method("Stackoverflow", "overflow", [], None)
        .unwrap()
        .invoke([])
        .unwrap_err();
    assert_eq!(err.class().name(), "java.lang.StackOverflowError");
}

#[cfg(miri)]
#[test]
fn guard_obj_lifetime() {
    use std::hash::{DefaultHasher, Hash};

    let jvm = make_jvm();
    let class = jvm.resolve_class("CrossJvm").unwrap();
    let field = class
        .field("field", Type::Ref("java.lang.Object".into()))
        .unwrap();

    drop(jvm);
    assert_eq!(class, class);
    assert_eq!(field, field);
    class.hash(&mut DefaultHasher::new());
    field.hash(&mut DefaultHasher::new());
}

#[test]
fn sync() {
    let jvm = make_jvm();
    let class = jvm.resolve_class("Sync").unwrap();

    let threads = 8;
    let per_thread = 50;

    let static_method = class.method("static_sync", None, None).unwrap();
    let instance_method = class.method("instance_sync", None, None).unwrap();
    let instance = class.create_object_no_constructor().unwrap();

    scope(|scope| {
        for _ in 0..threads {
            scope.spawn(|| {
                for _ in 0..per_thread {
                    static_method.invoke([]).unwrap();
                    instance_method.invoke([instance.clone().into()]).unwrap();
                }
            });
        }
    });

    assert_eq!(
        class
            .field("static_counter", Type::Int)
            .unwrap()
            .static_get(),
        threads * per_thread
    );
    assert_eq!(
        class
            .field("instance_counter", Type::Int)
            .unwrap()
            .instance_get(&instance),
        threads * per_thread
    );
}

#[test]
fn identity_hash_code() {
    let jvm = make_jvm();
    let class = jvm.resolve_class("java.lang.Object").unwrap();
    let hash = class.method("hashCode", [], Some(Type::Int)).unwrap();

    let obj_1 = class.create_object_no_constructor().unwrap();
    let obj_2 = class.create_object_no_constructor().unwrap();
    let hash_1 = hash.invoke([obj_1.into()]).unwrap();
    let hash_2 = hash.invoke([obj_2.into()]).unwrap();

    assert_ne!(hash_1, hash_2);
}

#[test]
fn monitor_tmp() {
    let jvm = make_jvm();
    let class = jvm.resolve_class("java.lang.Object").unwrap();

    let obj = class.create_object_no_constructor().unwrap();
    obj.enter_monitor();
    obj.exit_monitor().unwrap();
}

#[test]
fn string_interning() {
    let jvm = make_jvm();

    let class = jvm.resolve_class("Strings").unwrap();
    class.ensure_init().unwrap();
    let interned_1 = class
        .field("string", Type::Ref("java.lang.String".into()))
        .unwrap()
        .static_get()
        .to_ref()
        .unwrap()
        .unwrap();

    let interned_2 = jvm.intern_string(jvm.create_string("ABC"));
    let interned_3 = jvm.create_interned_string("ABC");
    let not_interned = jvm.create_string("ABC");

    assert_eq!(interned_1, interned_2);
    assert_eq!(interned_1, interned_3);
    assert_ne!(interned_1, not_interned);
}

#[test]
fn declared_exceptions() {
    let jvm = make_jvm();

    let class = jvm
        .resolve_method("DeclaredExceptions", "method", [], None)
        .unwrap();
    let exceptions = class.declared_exceptions().unwrap();
    assert_eq!(exceptions.len(), 1);
    assert_eq!(exceptions[0].name(), "java.lang.Throwable");
}
