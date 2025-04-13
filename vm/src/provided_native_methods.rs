use hashbrown::HashMap;

use crate::{Jvm, Method, Object, Type, fake_variadics::NativeBindError, jvm};

/// A native method provided by this crate
pub struct ProvidedMethod {
    pub class: String,
    pub method: String,
    pub args: Vec<Type>,
    pub returns: Option<Type>,
    #[allow(clippy::type_complexity)]
    bind: Box<dyn Fn(&Jvm, Method) -> Result<(), NativeBindError>>,
}

impl ProvidedMethod {
    /// Binds the method, panicking if it doesn't match.
    pub fn bind(&self, jvm: &Jvm) {
        let fail = || {
            panic!(
                "Failed to bind native method {}:{}",
                self.class, self.method
            )
        };
        let method = jvm
            .resolve_method(
                &self.class,
                &self.method,
                self.args.iter().cloned(),
                self.returns.clone(),
            )
            .unwrap_or_else(|_| fail());
        if (self.bind)(jvm, method).is_err() {
            fail();
        }
    }
}

macro_rules! provide {
    ($method:literal, [$($args:expr),*], $returns:expr, $native:expr) => {
        ProvidedMethod {
            class: String::default(),
            method: $method.into(),
            args: vec![$($args),*],
            returns: $returns,
            bind: Box::new(|jvm, method| {
                method.bind_native_code(jvm, $native)
            })
        }
    };
}

#[allow(unused)]
macro_rules! throw {
    ($jvm:expr, $class:expr) => {
        return Err(Object::new($jvm.id, throwable($jvm, $class)));
    };
}

pub fn get_provided_methods() -> HashMap<&'static str, Vec<ProvidedMethod>> {
    let mut map = HashMap::new();
    map.insert(
        "java.lang.Object",
        vec![
            provide!(
                "hashCode",
                [],
                Some(Type::Int),
                |jvm: &Jvm, this: Object| Ok(this.identity_hash(jvm))
            ),
            provide!(
                "hashCode",
                [],
                Some(Type::Int),
                |jvm: &Jvm, this: Object| Ok(this.identity_hash(jvm))
            ),
            provide!("wait", [], None, |jvm: &Jvm, this: Object| this.wait(jvm)),
            provide!("notify", [], None, |jvm: &Jvm, this: Object| this
                .notify(jvm)),
            provide!("notifyAll", [], None, |jvm: &Jvm, this: Object| this
                .notify_all(jvm)),
        ],
    );

    map.insert(
        "java.lang.System",
        vec![provide!(
            "identityHashCode",
            ["java.lang.Object".into()],
            Some(Type::Int),
            |jvm: &Jvm, x: Option<Object>| Ok(x.map(|x| x.identity_hash(jvm)).unwrap_or(0))
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

fn _throwable<'a>(jvm: &Jvm, name: &str, msg: impl Into<Option<&'a str>>) -> Object {
    Object::new(jvm.id, jvm::throw(jvm, name, msg))
}
