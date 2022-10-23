This is a toy embeddable JVM in writen in Rust. It is very much WIP and aims to eventually 
implement most of the Java 8 JVM specification (except for the loading of classes with version <50.0).
As this is intended as a learning experience, the choice of which features to implement is
not determined by practicality but by how interested I'm in doing so.

```rust
// Create a new JVM that loads classes from a folder called `classes`
let jvm = Jvm::new(FolderClassLoader::new_boxed(["classes""]));
// Load class `Foo`
let class = jvm.resolve_class("Foo").unwrap();
// Resolve its method `void bar(int _, String _)`
let method = class.method("bar", vec![Typ::Int, Typ::Ref("java/lang/String".into())], None).unwrap();
// And call it: `Foo.bar(0, null)`
method.invoke(&[Value::from(0i32), Value::from(None)]).expect("bar threw an exception");
// Resolve field `Foo.baz` of type bool
let field = class.field("baz", Typ::Bool).unwrap();
// And read its value (panicking if it's not a static field)
let field_value = field.static_get();
```

Currently implemented:
- thread-safe access
- class loading/initialization
- verification by type checking (not all instructions covered yet)
- operations on primitives
- intra-function control flow
- exceptions
- calling static & instance functions (except via interfaces)
- creating objects, including arrays
- accessing fields & array elements
- compressed pointers on x86-64 Linux

Some mayor missing features:
- garbage collection
- interfaces
- some bytecodes
- private/protected
- a way to call native code

Rough overview:
- a `Jvm` contains classes and objects
- internally, a lot of static references are used that only live as long as the `Jvm` exists,
  which Rust can't express. These are never directly made available to the library consumer.
- instead proxies which also contain a reference to the VM are provided
- errors returned by fallible operations are Exception objects
- classes are resolved lazily by `Jvm::resolve_class` and parsed in `parse.rs`
- before a method is executed, it's class will be verified (once) in verification.rs
- methods are executed using a simple interpreter loop in `interp.rs`
- one `FieldStorage` per class/object compactly stores the static/instance fields 
- objects contain a pointer to their class, followed by their fields
- objects are currently not garbage collected

This uses copious amounts of unsafe, such as
- to manage the lifecycle of classes, objects, …
- when casting type-erased local/stack values to objects
- when accessing fields (in `fields_storage.rs` and when dereferencing pointers to objects)
Unfortunately, for most of this the safety boundary is at the crate level.

This is only a JVM, not including a proper class library. The problem is that the JVM
specification depends on a number of classes, but never bothers to specify what it requires
of the class library. For example, each `CONSTANT_String` entry of a constant pool points
to an instance of the class `String` that contains the codepoints. Leaving aside the fact
that the correct class is `java.lang.String`, how does the JVM pass the data to the string /
read it back? Not specified. As a result, JVMs and class libraries are tied together. There's
a stub in the `java` directory but it's deliberately kept minimal to the point of uselessness,
serving only to describe the API the JVM depends on. Violating said API may cause exceptions, 
panics or infinite loops.