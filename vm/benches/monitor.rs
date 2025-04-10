use std::thread;

use criterion::{criterion_group, criterion_main, Bencher, Criterion};
use vm::*;

criterion_group!(monitor, benches);
criterion_main!(monitor);

pub fn benches(c: &mut Criterion) {
    c.bench_function("with_1_thread", |b| bench(b, 1));
    c.bench_function("with_2_threads", |b| bench(b, 2));
    c.bench_function("with_4_threads", |b| bench(b, 4));
}

fn make_jvm() -> Jvm {
    Jvm::new(
        FolderClassLoader::new_boxed(["classes", "benches/classes"]),
        20_000,
    )
}

fn bench(b: &mut Bencher, threads: i32) {
    let iterations = 100000 / threads;

    let jvm = make_jvm();
    let class = jvm.resolve_class("Monitor").unwrap();
    let method = class
        .method(&jvm, "stress", [Type::Int, Type::Int], None)
        .unwrap();
    let instance = class.create_object_uninitialized(&jvm);
    b.iter(|| {
        thread::scope(|scope| {
            for _ in 0..threads {
                scope.spawn(|| {
                    method
                        .invoke(&jvm, &[instance.into(), iterations.into(), Value::Int(10)])
                        .unwrap();
                });
            }
        });
    });
}
