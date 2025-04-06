use std::thread;

use jvm::{FolderClassLoader, Jvm, Type, Value};

fn main() {
    let jvm = Jvm::new(
        FolderClassLoader::new_boxed(["classes", "benches/classes"]),
        20_000,
    );
    let threads = 4;
    let iterations = 1_000_000 / threads;
    let delay = 10;

    let class = jvm.resolve_class("Monitor").unwrap();
    let method = class
        .method(&jvm, "stress", [Type::Int, Type::Int], None)
        .unwrap();
    let instance = class.create_object_uninitialized(&jvm);
    thread::scope(|scope| {
        for _ in 0..threads {
            scope.spawn(|| {
                method
                    .invoke(
                        &jvm,
                        &[instance.into(), iterations.into(), Value::Int(delay)],
                    )
                    .unwrap();
            });
        }
    });
}
