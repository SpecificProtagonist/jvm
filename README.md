# A JVM written in Rust

Goals:
- embeddable (e.g. sandboxing; multiple JVMs in the same process)
- non-terrible performance
- multithreading-support
- most importantly: Having fun developing this

Non-goals:
- providing a standard library
- loading pre-Java 8 classes

This JVM is unsuitable for serious use because, aside from still missing features, it is severely undertested! It also uses large amounts of unsafe scoped to the entire crate (though the interface it provides is safe) – for example, operating on an object pointer requires that it points to a valid object, but the object pointer may come from runtime-typed memory depending on the loaded classes, which get verified in a seperate verification step. Oh, and it is also lacking in comments.

```rust,ignore
// Create a new JVM that loads classes from a folder called `classes`
let jvm = Jvm::new(FolderClassLoader::new_boxed(["classes"]));
// Load class `Foo`
let class = jvm.resolve_class("Foo")?;
// Resolve its method `void bar(int, String)`
let method = class.method(&jvm, "bar", [Typ::Int, "java.lang.String".into()], None)?;
// And call it: `Foo.bar(0, null)`
method.invoke(&jvm, &[Value::Int(0), Value::Ref(None)])?;
// Resolve field `Foo.baz` of type bool
let field = class.field(&jvm, "baz", Typ::Boolean)?;
// And read its value (panicking if it's not a static field)
let value = field.static_get(&jvm);
// Bind a native method
class.method(&jvm, "add", [Typ::Float, Typ::Float], Some(Typ::Float))?
  .bind_native_code(&jvm, f32::add).unwrap();
```

Some current features:
- verification by type checking
- lazy loading of classes
- monitors
- native methods (`Method::bind_native_code`)
- method body hot reloading (`Jvm::reload_method_bytecodes_for_class`)
- compressed pointers & object headers

Some missing features:
- garbage collection
- jit-compilation
- `invokedynamic`
- stack traces
- string interning
- jar class loader
- reliable protection from stackoverflow, memory exhaustion and deadlocking
- wasm support
- make multithreading support optional
- debug adapter protocol support

## Class Library
This JVM doesn't come with a class library, so you will have to provide your own suitable for your usecase – e.g. you might provide a library that does not include I/O functionality. Unfortunately the Java Virtual Machine Specification does not define what the JVM and the class library require of each other. Part of the class library provided by HotSpot will work, but a lot of functionality such as reflection will be missing.

At minimum, the JVM requires the following classes and interfaces to exist:
<details>
<summary>Click to show</summary>

```text
java
├── io
│   └── Serializable
└── lang
    ├── AbstractMethodError
    ├── ArithmeticException
    ├── ArrayIndexOutOfBoundsException
    ├── ArrayStoreException
    ├── ClassCastException
    ├── ClassCircularityError
    ├── ClassFormatError
    ├── Cloneable
    ├── IllegalAccessError
    ├── IllegalMonitorStateException
    ├── IncompatibleClassChangeError
    ├── NegativeArraySizeException
    ├── NoClassDefFoundError
    ├── NoSuchFieldError
    ├── NoSuchMethodError
    ├── NullPointerException
    ├── Object
    ├── StackOverflowError
    ├── String
    │   ◦ char[] chars
    ├── Throwable
    ├── UnsatisfiedLinkError
    ├── UnsupportedClassVersionError
    └── VerifyError
```
</details>

Optional implementations for some native methods are provided. You can get a list via `provided_native_methods::get_provided_methods` or bind all of them via `Jvm::bind_provided_native_methods`.

## Architecture
The core type is the `Jvm` struct, which contains all data relevant to the VM, such as the heap and the list of loaded classes. Internally handles to this data are not lifetimed, however these handles are not obtainable by users of this crate directly but are wrapped in guard types that require a reference to the JVM in order to access data. The design of these handles will change when garbage collection is implemented.

To invoke a method on a fresh JVM, the following process takes place:
- the user-provided classloader loads the bytes representing the method's class
- the class and its superclasses/-interfaces get parsed and loaded into the JVM
- they have their constant pool resolved to make accessing const pool items faster, which includes resolving any references to classes
- they get intialized, which includes running class initialization methods
- they get verified, which includes type checking the bytecode of its methods
- the method is invoked by interpreting its bytecode or calling its native code

If any errors are encountered during this, an object representing a Java `Throwable` is returned.

On the heap, each object contains a pointer to its class, followed by its monitor, followed by its fields or array length and items.

Multiple threads may access the JVM simultaneously; locks are used to protect the class and method lifecycle and access to fields/array items is atomic.

Also check out [Futexes at Home](https://specificprotagonist.net/jvm-futex.html) for a blog post about how this crate's monitors work :)
