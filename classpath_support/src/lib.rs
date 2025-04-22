extern crate alloc;

use alloc::alloc::Layout;
use std::collections::HashMap;

use vm::{Class, ClassLibraryIntegration, Jvm, NativeFn, NativeMethod, Object, Type, Value};

/// # Safety
/// Must not call any of the native functions without the metadata layout set.
pub unsafe fn get_integration() -> ClassLibraryIntegration {
    fn make_class_object(jvm: &Jvm, class: Class) -> Object {
        let class_class = jvm.resolve_class("java.lang.Class").unwrap();
        let object = class_class.create_object_no_constructor(jvm).unwrap();
        unsafe { core::ptr::write(class.get_metadata(jvm, object) as *mut Class, class) };
        object
    }
    fn get_class_from_class_object(jvm: &Jvm, class: Object) -> Class {
        let class_class = jvm.resolve_class("java.lang.Class").unwrap();
        unsafe { core::ptr::read(class_class.get_metadata(jvm, class) as *mut Class) }
    }
    fn make_string(jvm: &Jvm, string: &str) -> Object {
        let length = string.encode_utf16().count();
        let backing_array = jvm.create_array(&Type::Char, length as i32);
        // TODO: Efficient array creation method on Jvm
        for (index, char) in string.encode_utf16().enumerate() {
            backing_array.array_write(jvm, index as i32, Value::Short(char as i16));
        }
        let string_class = jvm.resolve_class("java.lang.String").unwrap();
        // TODO: Switch to constructor
        let obj = string_class.create_object_no_constructor(jvm).unwrap();
        let field =
            string_class.field(jvm, "value", Type::Ref("char[]".into())).expect("API requirement: java.lang.String must contain an instance field `chars` of type `char[]`");
        field.instance_set(jvm, obj, Value::Ref(Some(backing_array)));
        obj
    }
    ClassLibraryIntegration {
        class_object_constructor: Box::new(make_class_object),
        get_class_from_class_object: Box::new(get_class_from_class_object),
        string_constructor: Box::new(make_string),
        metatada_layout: metadata_layout(),
        // SAFETY:
        // Same function preconditions
        native_methods: unsafe { native_methods() },
    }
}

#[allow(unused)]
macro_rules! throw {
    ($jvm:expr, $class:expr) => {
        return Err(Object::new($jvm.id, throwable($jvm, $class)));
    };
}

fn metadata_layout() -> HashMap<String, Layout> {
    HashMap::from([("java.lang.Class".to_owned(), Layout::new::<Class>())])
}

unsafe fn native_methods() -> Vec<NativeMethod> {
    fn def<N>(
        method: impl Into<String>,
        args: impl Into<Vec<Type>>,
        returns: impl Into<Option<Type>>,
        native: impl NativeFn<N> + Copy,
    ) -> NativeMethod {
        NativeMethod::new("", method, args, returns, native)
    }

    let mut list = Vec::new();
    let mut with_class = |class: &str, methods: Vec<NativeMethod>| {
        for mut method in methods {
            method.class = class.to_owned();
            list.push(method);
        }
    };

    let object = Type::Ref("java.lang.Object".into());
    let string = Type::Ref("java.lang.String".into());
    let class = Type::Ref("java.lang.Class".into());

    with_class(
        "java.lang.Object",
        vec![
            def("registerNatives", [], None, || ()),
            def("hashCode", [], Type::Int, |jvm: &Jvm, this: Object| {
                Ok(this.identity_hash(jvm))
            }),
            def("hashCode", [], Type::Int, |jvm: &Jvm, this: Object| {
                Ok(this.identity_hash(jvm))
            }),
            def("wait", [], None, |jvm: &Jvm, this: Object| this.wait(jvm)),
            def("notify", [], None, |jvm: &Jvm, this: Object| {
                this.notify(jvm)
            }),
            def("notifyAll", [], None, |jvm: &Jvm, this: Object| {
                this.notify_all(jvm)
            }),
        ],
    );

    with_class(
        "java.lang.System",
        vec![def(
            "identityHashCode",
            [object.clone()],
            Type::Int,
            |jvm: &Jvm, x: Option<Object>| Ok(x.map(|x| x.identity_hash(jvm)).unwrap_or(0)),
        )],
    );

    with_class(
        "java.lang.Class",
        vec![
            def("registerNatives", [], None, || ()),
            def(
                "getName0",
                [],
                Some(string.clone()),
                |jvm: &Jvm, this: Object| jvm.create_string(this.class(jvm).name(jvm)),
            ),
            def(
                "getSuperclass",
                [],
                Some(class.clone()),
                |jvm: &Jvm, this: Object| {
                    jvm.get_class_from_class_object(this)
                        .super_class(jvm)
                        .map(|class| class.class_object(jvm))
                },
            ),
            def(
                "isInterface",
                [],
                Some(Type::Boolean),
                |jvm: &Jvm, this: Object| jvm.get_class_from_class_object(this).is_interface(jvm),
            ),
            def(
                "isArray",
                [],
                Some(Type::Boolean),
                |jvm: &Jvm, this: Object| {
                    jvm.get_class_from_class_object(this)
                        .element_type(jvm)
                        .is_some()
                },
            ),
        ],
    );

    // TODO: Actually precise implementation (so far only sqrt)
    with_class(
        "java.lang.StrictMath",
        vec![
            def("sin", [Type::Double], Some(Type::Double), f64::sin),
            def("cos", [Type::Double], Some(Type::Double), f64::cos),
            def("tan", [Type::Double], Some(Type::Double), f64::tan),
            def("asin", [Type::Double], Some(Type::Double), f64::asin),
            def("acos", [Type::Double], Some(Type::Double), f64::acos),
            def("atan", [Type::Double], Some(Type::Double), f64::atan),
            def("exp", [Type::Double], Some(Type::Double), f64::exp),
            def("log", [Type::Double], Some(Type::Double), f64::log),
            def("log10", [Type::Double], Some(Type::Double), f64::log10),
            def("sqrt", [Type::Double], Some(Type::Double), f64::sqrt),
            def("cbrt", [Type::Double], Some(Type::Double), f64::cbrt),
            def(
                "atan2",
                [Type::Double, Type::Double],
                Some(Type::Double),
                f64::atan2,
            ),
            def(
                "pow",
                [Type::Double, Type::Double],
                Some(Type::Double),
                f64::powf,
            ),
            def("sinh", [Type::Double], Some(Type::Double), f64::sinh),
            def("cosh", [Type::Double], Some(Type::Double), f64::cosh),
            def("tanh", [Type::Double], Some(Type::Double), f64::tanh),
            def(
                "hypot",
                [Type::Double, Type::Double],
                Some(Type::Double),
                f64::hypot,
            ),
            def("expm1", [Type::Double], Some(Type::Double), f64::exp_m1),
            def("log1p", [Type::Double], Some(Type::Double), f64::ln_1p),
        ],
    );

    list
}
