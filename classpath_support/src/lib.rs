extern crate alloc;

use alloc::alloc::Layout;
use std::collections::HashMap;

use vm::{Class, ClassLibraryIntegration, Jvm, NativeFn, NativeMethod, Object, Type, Value};

/// # Safety
/// Must not call any of the native functions without the metadata layout set.
pub unsafe fn get_integration() -> ClassLibraryIntegration {
    fn make_class_object(jvm: &Jvm, _class: Class) -> Object {
        let class_class = jvm.resolve_class("java.lang.Class").unwrap();
        class_class.create_object_no_constructor(jvm).unwrap()
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
    HashMap::default()
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
            ["java.lang.Object".into()],
            Type::Int,
            |jvm: &Jvm, x: Option<Object>| Ok(x.map(|x| x.identity_hash(jvm)).unwrap_or(0)),
        )],
    );

    list
}
