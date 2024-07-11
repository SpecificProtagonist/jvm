use crate::{
    class::ClassInitState, jvm::error, method::Code, parse::read_class_file, verification::verify,
    Jvm,
};

#[derive(Debug)]
pub enum ReloadErr<'a> {
    Exception(crate::Object<'a>),
    ClassLayoutChanged,
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
    let new_class = read_class_file(&bytes, jvm).map_err(map_exception)?;

    // Check for incompatibilities
    if (class.name != new_class.name)
        || (class.access_flags != new_class.access_flags)
        || (class.super_class.map(|s| s.name.clone()) != new_class.super_class)
        || (class.interfaces.len() != new_class.interfaces.len())
        || class
            .interfaces
            .iter()
            .any(|i| new_class.interfaces.contains(&i.name))
        || (class.fields.len() != new_class.fields.len())
        || class.fields.values().any(|f| {
            !new_class
                .fields
                .iter()
                .any(|new| (f.nat == new.nat) & (f.access_flags == new.access_flags))
        })
        || (class.methods.len() != new_class.methods.len())
        || class.methods.values().any(|m| {
            !new_class
                .methods
                .values()
                .any(|new| (m.nat == new.nat) & (m.access_flags == new.access_flags))
        })
    {
        return Err(ReloadErr::ClassLayoutChanged);
    }

    // Alright, swap out code
    let mut const_pool = class.const_pool.write();
    *const_pool = new_class.const_pool;
    for (nat, method) in &class.methods {
        match (&method.code, &new_class.methods.get(nat).unwrap().code) {
            (Code::None | Code::Native(_), Code::None) => {}
            (Code::Bytecode(old), Code::Bytecode(new)) => {
                std::mem::swap(&mut *old.write(), &mut *new.write());
            }
            _ => return Err(ReloadErr::ClassLayoutChanged),
        }
    }

    let mut guard = class.init.lock();
    match *guard {
        ClassInitState::Uninit => {}
        ClassInitState::InProgress(_) | ClassInitState::Done => {
            // Resolution
            const_pool.resolve(jvm).map_err(map_exception)?;
            drop(const_pool);

            // Verification
            verify(jvm, class).map_err(map_exception)?;

            // Skip preparation & initialization to keep existing state
        }
        ClassInitState::Error(_) => *guard = ClassInitState::Uninit,
    }

    Ok(())
}
