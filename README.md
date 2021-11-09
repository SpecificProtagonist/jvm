This is a toy JVM in writen in Rust. It is very much WIP and aims to eventually implement the Java 8 JVM specification.

It currently allows
- class initialization
- operations on primitives
- intra-function control flow (except exceptions)
- calling static functions
- accessing fields

Rough overview:
- a `JVM` contains classes as well as interned strings & types
- methods are executed using a simple interpreter loop in `interp.rs`
- classes are resolved lazily by `JVM::resolve_class` and parsed in `parse.rs`
- one `FieldStorage` per class/object stores the static/instance fields 
- objects contain a pointer to their class, followed by their fields
