use class_loader::file_loaders;
use criterion::{Bencher, Criterion};
use criterion::{criterion_group, criterion_main};
use vm::*;

criterion_group!(interpreter, benches);
criterion_main!(interpreter);

pub fn benches(c: &mut Criterion) {
    c.bench_function("interpreter-fibonacci", fibonacci);
    c.bench_function("interpreter-nbody", nbody);
}

fn fibonacci(b: &mut Bencher) {
    let jvm = Jvm::new(file_loaders(["classes", "benches/classes"]), 20_000);
    let class = jvm.resolve_class("Interpreter").unwrap();
    let method = class.method(&jvm, "fibonacci", [Type::Int], None).unwrap();
    b.iter(|| {
        method.invoke(&jvm, &[Value::Int(10_000)]).unwrap();
    });
}

fn nbody(b: &mut Bencher) {
    let jvm = Jvm::new(file_loaders(["classes", "benches/classes"]), 200_000);
    let class = jvm.resolve_class("nbody").unwrap();
    class
        .method(&jvm, "sqrt", [Type::Double], Some(Type::Double))
        .unwrap()
        .bind_native_code(&jvm, f64::sqrt)
        .unwrap();
    let method = class
        .method(&jvm, "nbody", [Type::Int], Some(Type::Double))
        .unwrap();
    b.iter(|| {
        method.invoke(&jvm, &[Value::Int(10_000)]).unwrap();
    });
}
