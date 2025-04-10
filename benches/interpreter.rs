use criterion::{criterion_group, criterion_main};
use criterion::{Bencher, Criterion};
use jvm::*;

criterion_group!(interpreter, benches);
criterion_main!(interpreter);

pub fn benches(c: &mut Criterion) {
    c.bench_function("interpreter-fibonacci", fibonacci);
    c.bench_function("interpreter-nbody", nbody);
}

fn fibonacci(b: &mut Bencher) {
    let jvm = Jvm::new(
        FolderClassLoader::new_boxed(["classes", "benches/classes"]),
        20_000,
    );
    let class = jvm.resolve_class("Interpreter").unwrap();
    let method = class.method(&jvm, "fibonacci", [Type::Int], None).unwrap();
    b.iter(|| {
        method.invoke(&jvm, &[Value::Int(10_000)]).unwrap();
    });
}

fn nbody(b: &mut Bencher) {
    let jvm = Jvm::new(
        FolderClassLoader::new_boxed(["classes", "benches/classes"]),
        200_000,
    );
    jvm.bind_provided_native_methods();
    let class = jvm.resolve_class("nbody").unwrap();
    let method = class
        .method(&jvm, "nbody", [Type::Int], Some(Type::Double))
        .unwrap();
    b.iter(|| {
        method.invoke(&jvm, &[Value::Int(10_000)]).unwrap();
    });
}
