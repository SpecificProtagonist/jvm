This is a toy embeddable JVM in writen in Rust. It is very much WIP and aims to eventually 
implement the Java 8 JVM specification (except for the loading of classes with version <50.0).

It currently allows
- class loading/initialization
- operations on primitives
- intra-function control flow (except exceptions)
- calling functions (except via interfaces)
- creating objects, including arrays
- accessing fields & array elements

Rough overview:
- a `JVM` contains classes as well as interned strings
- methods are executed using a simple interpreter loop in `interp.rs`
- classes are resolved lazily by `JVM::resolve_class` and parsed in `parse.rs`
- one `FieldStorage` per class/object stores the static/instance fields 
- objects contain a pointer to their class, followed by their fields
- objects are currently are `Box::leak`ed

Unsafe is currently used in the following ways:
- when accessing fields (in `fields_storage.rs` and when dereferencing pointers to objects)
- to drop Classes, Strings, … when the JVM is dropped
