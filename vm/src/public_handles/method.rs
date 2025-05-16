use alloc::sync::Arc;

use crate::*;

define_guard!(
    /// A method. Does not necessarily have an implementation.
    Method,
    method::Method
);

impl Method {
    /// The class this method is defined by.
    pub fn class(&self) -> Class {
        Class::new(self.jvm.clone(), self.value.class())
    }

    /// The name of this method. There may be multiple methods of the same
    /// name but differing types in the same class.
    pub fn name(&self) -> &str {
        &self.value.nat.name
    }

    /// Argument and return types of this method.
    pub fn typ(&self) -> &MethodDescriptor {
        &self.value.nat.typ
    }

    /// Access flags of this method.
    pub fn access_flags(&self) -> AccessFlags {
        self.value.access_flags
    }

    /// Get all declared exceptions.
    ///
    /// This also initializes this method's class;
    /// returns `Err` if this fails.
    ///
    /// Note that the method may throw arbitrary throwables:
    /// Java's restriction around declared exceptions are enforced by the compiler.
    pub fn declared_exceptions(&self) -> JvmResult<Vec<Class>> {
        self.value
            .class()
            .ensure_init(self.jvm.deref())
            .map_err(|err| Object::new(self.jvm.deref(), err))?;
        let const_pool = self.value.class().const_pool.read().unwrap();
        Ok(self
            .value
            .exceptions
            .iter()
            .map(|&index| {
                Class::new(
                    self.jvm.deref(),
                    const_pool.get_class(self.jvm.deref(), index).unwrap(),
                )
            })
            .collect())
    }

    /// Java method signature. See JVMS §4.7.9.1 for details.
    pub fn signature(&self) -> Option<Arc<str>> {
        self.value.signature.clone()
    }

    /// Whether this method is marked as deprecated (does not affect semantics).
    pub fn deprecated(&self) -> bool {
        self.value.deprecated
    }

    /// Attributes not recognized by this Jvm.
    pub fn extra_attributes(&self) -> &HashMap<String, Box<[u8]>> {
        &self.value.extra_attributes
    }

    /// Calls the method, initializing its class if necessary.
    /// Returns either the function's return value or the exception that was thrown.
    ///
    /// # Panics
    /// Panics if any of the [`Value`]s is an object created by another JVM.
    pub fn invoke(&self, args: impl IntoIterator<Item = Value>) -> JvmResult<Option<Value>> {
        self.jvm.deref().stackoverflow_error.get_or_init(|| {
            jvm::new_throwable(self.jvm.deref(), "java.lang.StackOverflowError", None)
        });

        interpreter::invoke(
            self.jvm.deref(),
            self.value,
            args.into_iter().map(|arg| arg.into_inner(&self.jvm)),
        )
        .map(|returned| {
            returned.map(|value| Value::new(&self.jvm, value, self.typ().returns.as_ref().unwrap()))
        })
        .map_err(|e| Object::new(self.jvm.clone(), e))
    }

    /// Register code for a native method.
    ///
    /// The method must be marked as native and match the signature of `code`.
    /// If it is an instance method, its first argument will be the object it is invoked on.
    /// If called multiple times for the same method, the previous registration will be replaced.
    ///
    /// The callback returning a value of wrong type will result in a panic.
    ///
    /// # Example
    /// ```no_run
    /// # use vm::*;
    /// # let method: Method = todo!();
    /// method.bind_native_code(f32::powf)
    /// # .unwrap()
    /// ```
    // TODO: Note guarantees about available stack size
    #[allow(private_bounds)]
    pub fn bind_native_code<F: NativeFn<T>, T>(&self, code: F) -> Result<(), NativeBindError> {
        fake_variadics::bind_native_code(self.value, code)
    }
}
