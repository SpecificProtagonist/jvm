This is a toy embeddable JVM in writen in Rust. It is very much WIP and aims to eventually 
implement the Java 8 JVM specification (except for the loading of classes with version <50.0).
As this is intended as a learning experience, the choice of which features to implement is
not determined by practicality but by how interested I'm in doing so.  

It currently has
- class loading/initialization
- verification by type checking
- operations on primitives
- intra-function control flow (except exceptions)
- calling functions (except via interfaces)
- creating objects, including arrays
- accessing fields & array elements
- compressed pointers on x86-64 Linux

Rough overview:
- a `JVM` contains classes as well as interned strings
- classes are resolved lazily by `JVM::resolve_class` and parsed in `parse.rs`
- methods are executed using a simple interpreter loop in `interp.rs`
- one `FieldStorage` per class/object stores the static/instance fields 
- objects contain a pointer to their class, followed by their fields
- objects are currently are `Box::leak`ed

This uses quite a bit of unsafe, namely
- when casting type-erased local/stack values to objects
- when accessing fields (in `fields_storage.rs` and when dereferencing pointers to objects)
- to drop Classes, Strings, … when the JVM is dropped
