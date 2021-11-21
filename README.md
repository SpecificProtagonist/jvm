This is a toy JVM in writen in Rust. It is very much WIP and aims to eventually implement the Java 8 JVM specification.

It currently allows
- class loading/initialization
- operations on primitives
- intra-function control flow (except exceptions)
- calling functions (except via interfaces)
- accessing fields

Rough overview:
- a `JVM` contains classes as well as interned strings & types
- methods are executed using a simple interpreter loop in `interp.rs`
- classes are resolved lazily by `JVM::resolve_class` and parsed in `parse.rs`
- one `FieldStorage` per class/object stores the static/instance fields 
- objects contain a pointer to their class, followed by their fields
- objects are currently are `Box::leak`ed

Unsafe is currently used in the following ways:
- when accessing fields, as correct alignment is required 
- to cast the lifetime of Classes, Strings, … to the lifetime of the JVM,
  as they are never reallocated and are allocated in `TypedArena`s 
  which are only dropped when the JVM is dropped
