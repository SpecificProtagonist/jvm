use std::collections::HashMap;

use vm::{Jvm, NativeFn, NativeMethod, Object, Type};

/// Make all native methods provided by this crate available.
pub fn make_available(jvm: &Jvm) {
    for method in get_provided_methods().into_values().flatten() {
        method.make_available(jvm);
    }
}

#[allow(unused)]
macro_rules! throw {
    ($jvm:expr, $class:expr) => {
        return Err(Object::new($jvm.id, throwable($jvm, $class)));
    };
}

fn def<N>(
    method: impl Into<String>,
    args: impl Into<Vec<Type>>,
    returns: impl Into<Option<Type>>,
    native: impl NativeFn<N> + Copy,
) -> NativeMethod {
    NativeMethod::new("", method, args, returns, native)
}

/// Get all native methods provided by this crate,
/// keyed by their class to let you filter which ones you want.
pub fn get_provided_methods() -> HashMap<&'static str, Vec<NativeMethod>> {
    let mut map = HashMap::new();
    map.insert(
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

    map.insert(
        "java.lang.System",
        vec![def(
            "identityHashCode",
            ["java.lang.Object".into()],
            Type::Int,
            |jvm: &Jvm, x: Option<Object>| Ok(x.map(|x| x.identity_hash(jvm)).unwrap_or(0)),
        )],
    );

    map.into_iter()
        .map(|(class, mut methods)| {
            for method in &mut methods {
                method.class = class.to_owned();
            }
            (class, methods)
        })
        .collect()
}
