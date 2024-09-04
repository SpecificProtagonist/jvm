A toy embeddable JVM writen in Rust. It is very much WIP and aims to eventually 
implement most of the Java 8 JVM specification, except for the loading of classes with version <50.0
and the Java SE class libraries. As this is intended as a toy / a learning experience, the choice of which features to implement is
not determined by practicality but by how interested I'm in doing so. It also uses large amounts of *unsafe*
(the safety boundary essentially being the entire crate), is badly tested, doesn't have any benchmarks, and doesn't do any garbage collection.

```rust,ignore
use jvm::*;
// Create a new JVM that loads classes from a folder called `classes`
let jvm = Jvm::new(FolderClassLoader::new_boxed(["classes"]));
// Load class `Foo`
let class = jvm.resolve_class("Foo").unwrap();
// Resolve its method `void bar(int, String)`
let method = class.method("bar", [Typ::Int, "java.lang.String".into()], None)?;
// And call it: `Foo.bar(0, null)`
method.invoke(&[Value::Int(0), Value::Ref(None)])?;
// Resolve field `Foo.baz` of type bool
let field = class.field("baz", Typ::Boolean).unwrap();
// And read its value (panicking if it's not a static field)
let field_value = field.static_get();
// Bind a native method
fn add(_: &Jvm, a: f32, b: f32) -> JvmResult<f32> {
    Ok(a + b)
}
class.method("add", [Typ::Float, Typ::Float], Some(Typ::Float))?
  .bind_native_code(add).unwrap();
```

Currently implemented:
- classes, arrays, interfaces
- loading/initialization
- verification by type checking
- almost all instructions (see `src/opcodes.rs`)
- exceptions
- compressed pointers on x86-64 Linux
- multi-threaded access
- hot method body reloading

Some mayor missing features:
- garbage collection
- method references
- private/protected
- locking
- string interning
- protection from stack overflow & memory exhaustion

This is only a JVM, not including a proper class library. The problem is that the JVM
specification and class library depend on each other. For example, each `CONSTANT_String` entry of a constant pool points
to an instance of the class `String` that contains the codepoints. Leaving aside the fact
that the correct class is `java.lang.String`, how does the JVM pass the data to the string /
read it back? As a result, JVMs and class libraries are tied together. There's a stub in the `java`
directory but it's deliberately kept minimal to the point of uselessness, serving only to describe
the API this JVM depends on. Violating said API may cause exceptions, panics or infinite loops.
