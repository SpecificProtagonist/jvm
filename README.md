This is a toy embeddable JVM in writen in Rust. It is very much WIP and aims to eventually 
implement most of the Java 8 JVM specification (except for the loading of classes with version <50.0).
As this is intended as a learning experience, the choice of which features to implement is
not determined by practicality but by how interested I'm in doing so.

Usage:


Currently implemented:
- class loading/initialization
- verification by type checking (not all instructions covered yet)
- operations on primitives
- intra-function control flow
- exceptions
- calling static & object functions (except via interfaces)
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
- one `FieldStorage` per class/object stores the static/instance fields 
- objects contain a pointer to their class, followed by their fields
- objects are currently not garbage collected

This uses copious amounts of unsafe, namely
- to allocate the heap
- when casting type-erased local/stack values to objects
- when accessing fields (in `fields_storage.rs` and when dereferencing pointers to objects)
- to drop Classes, Strings, … when the JVM is dropped

This is only a JVM, not including a proper class library. The problem is that the JVM
specification depends on a number of classes, but never bothers to specify what it requires
of the class library. For example, each `CONSTANT_String` entry of a constant pool points
to an instance of the class `String` that contains the codepoints. Leaving aside the fact
that the correct class is `java.lang.String`, how does the JVM pass the data to the string /
read it back? Not specified. As a result, JVMs and class libraries are tied together. There's
a stub in the `java` directory but it's deliberately kept minimal to the point of uselessness,
serving only to describe the API the JVM depends on. Violating said API may cause exceptions, 
panics or infinite loops.