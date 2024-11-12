use std::thread;

use criterion::{criterion_group, criterion_main, Bencher, Criterion};
use jvm::*;

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("with_1_thread", |b| bench(b, 1));
    c.bench_function("with_4_threads", |b| bench(b, 4));
    c.bench_function("with_32_threads", |b| bench(b, 32));
}

fn make_jvm() -> Jvm {
    Jvm::new(FolderClassLoader::new_boxed(["classes", "benches/classes"]))
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
