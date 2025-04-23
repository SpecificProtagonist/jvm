use core::sync::atomic::Ordering;

use crate::{
    class::{Class, ClassState},
    jvm::{self, JvmResult, throw},
    method::Code,
    parse::read_class_file,
    verification::verify_method,
};

#[derive(Debug)]
pub enum ReloadErr {
    Exception(crate::Object),
    ClassStructureChanged,
}

pub(crate) fn reload_method_bytecodes_for_class(
    jvm: &jvm::Jvm,
    name: &str,
) -> Result<(), ReloadErr> {
    let class = crate::Jvm::from(jvm)
        .resolve_class(name)
        .map_err(ReloadErr::Exception)?;
    let handle = class.jvm;
    let class = class.value;

    // Load new class
    let map_exception = |e| ReloadErr::Exception(crate::Object::new(handle.clone(), e));
    let name = name.replace('/', ".");
    let bytes = jvm
        .bootstrap_class_loader
        .load(&name)
        .ok_or_else(|| map_exception(throw(jvm, "NoClassDefFoundError", &*name)))?;
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
        || new_class.methods.values().any(|m| {
            !class
                .methods
                .values()
                .any(|old| (m.nat == old.nat) & (m.access_flags == old.access_flags))
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

    let mut guard = class.init.lock().unwrap();
    loop {
        match *guard {
            ClassState::VerificationInProgress(_) | ClassState::InitializationInProgress(_) => {
                guard = class.init_waiter.wait(guard).unwrap();
            }
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
                    new_class.const_pool.resolve(jvm, class)?;
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
    // TODO: Attributes. Let's just swap entire class.
    *class.const_pool.write().unwrap() = new_class.const_pool;
    for (nat, method) in &new_class.methods {
        let mut new = method.code.write().unwrap();
        let mut old = class.methods.get(nat).unwrap().code.write().unwrap();
        if let (Code::Bytecode(old), Code::Bytecode(new)) = (&mut *old, &mut *new) {
            core::mem::swap(old, new);
        }
    }
    // Skip preparation & initialization to keep existing state

    class.init_waiter.notify_all();
    Ok(())
}
