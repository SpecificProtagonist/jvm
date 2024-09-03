use std::sync::atomic::Ordering;

use crate::{
    class::{Class, ClassState},
    jvm::{error, JvmResult},
    method::Code,
    parse::read_class_file,
    verification::verify_method,
    Jvm,
};

#[derive(Debug)]
pub enum ReloadErr<'a> {
    Exception(crate::Object<'a>),
    ClassStructureChanged,
}

pub(crate) fn reload_method_bytecodes_for_class<'a>(
    jvm: &'a Jvm,
    name: &str,
) -> Result<(), ReloadErr<'a>> {
    let class = jvm.resolve_class(name).map_err(ReloadErr::Exception)?.value;

    // Load new class
    let map_exception = |e| ReloadErr::Exception(crate::Object::new(jvm, e));
    let name = name.replace('/', ".");
    let bytes = jvm
        .bootstrap_class_loader
        .load(&name)
        .ok_or_else(|| map_exception(error(jvm, "NoClassDefFoundError", &name)))?;
    let mut new_class = read_class_file(&bytes, jvm).map_err(map_exception)?;

    // Check for incompatibilities
    if (class.name != new_class.name)
        || (class.access_flags != new_class.access_flags)
        || (class.super_class.map(|s| s.name.clone()) != new_class.super_class)
        || (class.interfaces.len() != new_class.interfaces.len())
        || class.interfaces.iter().any(|i| {
            new_class
                .interfaces
                .iter()
                .any(|new| new == i.name.as_ref())
        })
        || (class.fields.len() != new_class.fields.len())
        || class.fields.values().any(|f| {
            !new_class
                .fields
                .iter()
                .any(|new| (f.nat == new.nat) & (f.access_flags == new.access_flags))
        })
        || (class
            .methods
            .values()
            .filter(|m| m.class() == class)
            .count()
            != new_class.methods.len())
        || class.methods.values().any(|m| {
            !new_class
                .methods
                .values()
                .any(|new| (m.nat == new.nat) & (m.access_flags == new.access_flags))
        })
    {
        return Err(ReloadErr::ClassStructureChanged);
    }

    // Set class pointers
    for method in new_class.methods.values() {
        method
            .class
            .store(class as *const Class as *mut Class, Ordering::SeqCst);
    }
    for field in new_class.fields {
        field
            .class
            .store(class as *const Class as *mut Class, Ordering::SeqCst);
    }

    let mut guard = class.init.lock();
    loop {
        match *guard {
            ClassState::InProgress(_) => class.init_waiter.wait(&mut guard),
            ClassState::Error(_) => {
                // Reset in case the error was caused by bytecode verification
                *guard = ClassState::Unverified;
                break;
            }
            ClassState::Unverified => break,
            ClassState::Verified | ClassState::Initialized => {
                // Verification needed
                if let Err(e) = || -> JvmResult<()> {
                    // Resolution
                    new_class.const_pool.resolve(jvm)?;
                    // Verification – only bytecode has changed
                    for method in new_class.methods.values() {
                        verify_method(jvm, &new_class.const_pool, method)?
                    }
                    Ok(())
                }() {
                    *guard = ClassState::Error(e);
                    return Err(map_exception(e));
                }
                break;
            }
        }
    }

    // Alright, swap out code
    *class.const_pool.write() = new_class.const_pool;
    for (nat, method) in &class.methods {
        let mut old = method.code.write();
        let mut new = new_class.methods.get(nat).unwrap().code.write();
        if let (Code::Bytecode(old), Code::Bytecode(new)) = (&mut *old, &mut *new) {
            std::mem::swap(old, new);
        }
    }
    // Skip preparation & initialization to keep existing state

    class.init_waiter.notify_all();
    Ok(())
}
