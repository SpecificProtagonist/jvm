use alloc::alloc::Layout;
use alloc::sync::Arc;
use core::sync::atomic::Ordering::*;
use hashbrown::HashMap;
use std::sync::{Condvar, Mutex, RwLock};
use std::thread::ThreadId;

use crate::jvm::{UnboundNativeMethods, throw};
use crate::monitor::maybe_loom;
use crate::object::ObjectHolder;
use crate::{
    AccessFlags,
    const_pool::ConstPool,
    field::{Field, FieldNaT, FieldNaTBorrow},
    jvm::Jvm,
    jvm::JvmResult,
    method::{MethodDescriptor, MethodNaT, MethodNaTBorrow, MethodRef},
    object::{self, FieldStorage, Object},
    typ::Type,
    verification::verify,
};

/// Represents an interface, regular class, array class or enum.
/// Does not store its defining loader as the Jvm currently may only have one.
pub(crate) struct Class {
    /// Fully qualified name, e.g. "java.lang.Object[]"
    pub(crate) name: Arc<str>,
    pub(crate) package: Arc<str>,
    pub(crate) super_class: Option<ClassRef>,
    /// For array classes, this is the type of the array elements; None for normal classes
    pub(crate) element_type: Option<Type>,
    /// Write access is required for resolution and hot reloading.
    /// Otherwise only read access is required.
    pub(crate) const_pool: RwLock<ConstPool>,
    pub(crate) access_flags: AccessFlags,
    /// Name of the source file this class was compiled from
    pub(crate) source_file_name: Option<Arc<str>>,
    pub(crate) signature: Option<Arc<str>>,
    pub(crate) deprecated: bool,
    pub(crate) extra_attributes: std::collections::HashMap<String, Box<[u8]>>,
    pub(crate) interfaces: Vec<ClassRef>,
    /// Does not contain inherited fields.
    /// Note: JVM supports field overloading.
    pub(crate) fields: HashMap<FieldNaT, Field>,
    /// Includes inherited methods.
    pub(crate) methods: HashMap<MethodNaT, MethodRef>,
    /// Data of all static fields
    pub(crate) static_storage: FieldStorage,
    /// Monitor used for static synchronized methods.
    // TODO: We only need a statically allocated reentrant mutex here, which would be faster.
    pub(crate) static_monitor: maybe_loom::AtomicU32,
    /// Combined size of instance fields in normal class, 0 if array
    pub(crate) object_size: usize,
    pub(crate) metadata_offset: usize,
    pub(crate) init: Mutex<ClassState>,
    pub(crate) init_waiter: Condvar,
    pub(crate) class_object: ObjectHolder,
}

#[derive(Debug)]
pub(crate) enum ClassState {
    Unverified,
    Verified,
    VerificationInProgress(ThreadId),
    InitializationInProgress(ThreadId),
    /// Initialization succeeded.
    Initialized,
    /// Initialization failed; holds the exception thrown.
    Error(Object),
}

pub(crate) type ClassRef = &'static Class;

impl Eq for ClassRef {}
impl PartialEq for ClassRef {
    fn eq(&self, other: &Self) -> bool {
        core::ptr::eq(*self, *other)
    }
}

impl core::hash::Hash for ClassRef {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        core::hash::Hash::hash(&(*self as *const Class as usize), state)
    }
}

pub(crate) struct ClassLayout {
    pub fields: HashMap<FieldNaT, Field>,
    pub static_size: usize,
    pub object_size: usize,
    pub metadata_offset: usize,
}

impl Class {
    pub(crate) fn set_layouts(
        jvm: &Jvm,
        name: &str,
        super_class: Option<&Class>,
        fields: Vec<Field>,
        metadata: Layout,
    ) -> ClassLayout {
        let mut static_fields = Vec::new();
        let mut object_fields = Vec::new();
        for field in fields {
            if field.access_flags.contains(AccessFlags::STATIC) {
                static_fields.push(field);
            } else {
                object_fields.push(field);
            }
        }

        let mut fields = HashMap::default();

        let mut set_layouts = |storage_size: &mut usize, mut add_fields: Vec<Field>| {
            add_fields.sort_by_key(|field| field.nat.typ.size());
            'outer: while !add_fields.is_empty() {
                // Try to find the biggest field we can append without padding
                let log_align = storage_size.trailing_zeros();
                for i in 0..add_fields.len() {
                    if add_fields[i].nat.typ.size().trailing_zeros() <= log_align {
                        let mut field = add_fields.remove(i);
                        field.byte_offset = *storage_size;
                        *storage_size += field.nat.typ.size();
                        fields.insert(field.nat.clone(), field);
                        continue 'outer;
                    }
                }

                // Alright, we need padding, so continue with the smallest field
                let mut field = add_fields.pop().unwrap();
                while storage_size.trailing_zeros() < field.nat.typ.size().trailing_zeros() {
                    *storage_size += 1;
                }
                field.byte_offset = *storage_size;
                *storage_size += field.nat.typ.size();
                fields.insert(field.nat.clone(), field);
            }
        };

        let mut static_size = 0;
        set_layouts(&mut static_size, static_fields);

        let mut object_size = match super_class {
            Some(super_class) => super_class.object_size,
            None => object::OFFSET_FIELDS,
        };
        set_layouts(&mut object_size, object_fields);
        let (mut object_layout, metadata_offset) = Layout::from_size_align(object_size, 8)
            .unwrap()
            .extend(metadata)
            .unwrap();

        if name == "java.lang.Class" {
            let (new_object_layout, class_ptr_offset) = Layout::from_size_align(object_size, 8)
                .unwrap()
                .extend(metadata)
                .unwrap();
            object_layout = new_object_layout;
            jvm.class_class_ptr_offset.store(class_ptr_offset, Release);
        }

        let object_size = object_layout.size();

        ClassLayout {
            fields,
            static_size,
            object_size,
            metadata_offset,
        }
    }

    pub(crate) fn class_object(&'static self, jvm: &Jvm) -> Object {
        self.class_object.get(|| {
            let class_class = jvm
                .resolve_class_internal("java.lang.Class")
                .expect("Failed to resolve java.lang.Class");
            let obj = jvm.create_object_skipping_constructor(class_class);
            let offset = jvm.class_class_ptr_offset.load(Acquire);
            let ptr = self as *const Class as usize;
            obj.fields().write(jvm.heap.ptrs(), offset, ptr, false);
            obj
        })
    }

    /// # Safety
    /// - object must be an instance of this class
    pub(crate) unsafe fn get_metadata(&self, object: Object) -> *const () {
        (object.0.get() + self.metadata_offset) as *const ()
    }

    pub(crate) fn is_interface(&self) -> bool {
        self.access_flags.contains(AccessFlags::INTERFACE)
    }

    /// Convenience method for getting this class' type
    pub(crate) fn typ(&self) -> Type {
        Type::Ref(self.name.clone())
    }

    pub(crate) fn method(
        &self,
        name: &str,
        args: impl IntoIterator<Item = Type>,
        ret: Option<Type>,
    ) -> Option<MethodRef> {
        self.methods
            .get(&MethodNaTBorrow {
                name,
                typ: &MethodDescriptor {
                    args: args.into_iter().collect(),
                    returns: ret,
                },
            })
            .copied()
    }

    pub(crate) fn iter_fields(&'static self) -> impl Iterator<Item = &'static Field> {
        struct FieldIterator {
            fields: hashbrown::hash_map::Values<'static, FieldNaT, Field>,
            class: &'static Class,
        }
        impl Iterator for FieldIterator {
            type Item = &'static Field;

            fn next(&mut self) -> Option<Self::Item> {
                if let Some(field) = self.fields.next() {
                    Some(field)
                } else if let Some(super_class) = self.class.super_class {
                    self.fields = super_class.fields.values();
                    self.class = super_class;
                    self.next()
                } else {
                    None
                }
            }
        }
        FieldIterator {
            fields: self.fields.values(),
            class: self,
        }
    }

    pub(crate) fn field(&self, name: &str, typ: Type) -> Option<&Field> {
        self.field_by_nat(FieldNaTBorrow { name, typ })
    }

    pub(crate) fn field_by_nat(&self, nat: FieldNaTBorrow) -> Option<&Field> {
        self.fields
            .get(&nat)
            .or_else(|| self.super_class.and_then(|c| c.field_by_nat(nat)))
    }

    // TODO: provide fields() and methods()
    // figure out how to present distinguished between inherited and not

    fn subclass_of(&'static self, other: ClassRef) -> bool {
        (self == other)
            || self
                .super_class
                .map(|sc| sc.subclass_of(other))
                .unwrap_or(false)
    }

    pub fn true_subclass_of(&self, other: &str) -> bool {
        self.super_class
            .map(|sc| sc.assignable_to(other))
            .unwrap_or(false)
    }

    pub fn assignable_to(&self, other: &str) -> bool {
        (self.name.as_ref() == other)
            || self.true_subclass_of(other)
            || self.interfaces.iter().any(|i| other == &*i.name)
    }

    /// Ensures this class is initialized. Includes linking, verification, preparation & resolution.
    /// This happens automatically when a method belonging to this class is called.
    // May only be called by the library consumer, Jvm::invoke, the instuctions getstatic, putstatic,
    // and invokestatic, or anything that creates a class instance.
    pub fn ensure_init(&'static self, jvm: &Jvm) -> JvmResult<()> {
        loop {
            let mut guard = self.init.lock().unwrap();
            match *guard {
                ClassState::VerificationInProgress(thread)
                    if thread == std::thread::current().id() =>
                {
                    // TODO: Can this happen?
                    todo!()
                }
                ClassState::InitializationInProgress(thread)
                    if thread == std::thread::current().id() =>
                {
                    return Ok(());
                }
                ClassState::VerificationInProgress(_) | ClassState::InitializationInProgress(_) => {
                    drop(self.init_waiter.wait(guard).unwrap());
                }
                ClassState::Unverified => {
                    drop(guard);
                    self.verify(jvm)?;
                    self.init_waiter.notify_all();
                }
                ClassState::Verified => {
                    *guard = ClassState::InitializationInProgress(std::thread::current().id());
                    drop(guard);

                    if let Some(super_class) = self.super_class {
                        super_class.ensure_init(jvm)?;
                    }

                    for interface in &self.interfaces {
                        interface.ensure_init(jvm)?;
                    }

                    let result = self.init(jvm);
                    *self.init.lock().unwrap() = match result {
                        Ok(()) => ClassState::Initialized,
                        Err(err) => ClassState::Error(err),
                    };
                    self.init_waiter.notify_all();
                    return result;
                }
                ClassState::Initialized => {
                    return Ok(());
                }
                ClassState::Error(err) => return Err(err),
            }
        }
    }

    pub(crate) fn verify(&'static self, jvm: &Jvm) -> JvmResult<()> {
        let mut guard = self.init.lock().unwrap();
        loop {
            match *guard {
                ClassState::VerificationInProgress(thread)
                    if thread == std::thread::current().id() =>
                {
                    unreachable!("recursive verification");
                }
                // Run verification
                ClassState::Unverified => {
                    *guard = ClassState::VerificationInProgress(std::thread::current().id());
                    drop(guard);
                    let result = self.verify_impl(jvm);
                    *self.init.lock().unwrap() = match result {
                        Ok(()) => ClassState::Verified,
                        Err(err) => ClassState::Error(err),
                    };
                    return result;
                }
                // Wait for class to be verified
                ClassState::VerificationInProgress(_) => {
                    guard = self.init_waiter.wait(guard).unwrap();
                }
                // Already verified
                ClassState::Verified
                | ClassState::InitializationInProgress(_)
                | ClassState::Initialized => return Ok(()),
                ClassState::Error(err) => return Err(err),
            }
        }
    }

    pub(crate) fn verify_impl(&'static self, jvm: &Jvm) -> JvmResult<()> {
        let mut const_pool = self.const_pool.write().unwrap();

        // Resolution
        const_pool.resolve(jvm, self)?;
        drop(const_pool);

        // Verification
        verify(jvm, self)?;

        Ok(())
    }

    fn prepare(&self, jvm: &Jvm) -> JvmResult<()> {
        ConstPool::create_objects(&self.const_pool, jvm)?;

        let const_pool = self.const_pool.read().unwrap();

        let ptrs = jvm.heap.ptrs();
        // Preparation
        for field in self.fields.values() {
            if let Some(index) = field.const_value_index {
                match &field.nat.typ {
                    Type::Boolean | Type::Byte => self.static_storage.write::<i8>(
                        ptrs,
                        field.byte_offset,
                        const_pool.get_int(jvm, index)? as i8,
                        true,
                    ),
                    Type::Short | Type::Char => self.static_storage.write::<i16>(
                        ptrs,
                        field.byte_offset,
                        const_pool.get_int(jvm, index)? as i16,
                        true,
                    ),
                    Type::Int => self.static_storage.write::<i32>(
                        ptrs,
                        field.byte_offset,
                        const_pool.get_int(jvm, index)?,
                        true,
                    ),
                    Type::Float => self.static_storage.write::<f32>(
                        ptrs,
                        field.byte_offset,
                        const_pool.get_float(jvm, index)?,
                        true,
                    ),
                    Type::Double => self.static_storage.write::<f64>(
                        ptrs,
                        field.byte_offset,
                        const_pool.get_double(jvm, index)?,
                        true,
                    ),
                    Type::Long => self.static_storage.write::<i64>(
                        ptrs,
                        field.byte_offset,
                        const_pool.get_long(jvm, index)?,
                        true,
                    ),
                    Type::Ref(name) => {
                        if name.as_ref() == "java.lang.String" {
                            self.static_storage.write::<usize>(
                                ptrs,
                                field.byte_offset,
                                const_pool.get_string(jvm, index)?.0.get(),
                                true,
                            )
                        } else {
                            return Err(throw(
                                jvm,
                                "ClassFormatError",
                                &*format!("{}: preparation failed", self.name),
                            ));
                        }
                    }
                }
            }
        }
        Ok(())
    }

    /// Requires class to be verified, may only be called from self.ensure_init
    fn init(&'static self, jvm: &Jvm) -> JvmResult<()> {
        // Bind available native methods
        let mut unbound = jvm.unbound_native_methods.lock().unwrap();
        if let Some(UnboundNativeMethods::Available(available)) = unbound.remove(&*self.name) {
            for native in available {
                if let Some(method) = self.method(&native.method, native.args, native.returns) {
                    let _ = (native.bind)(method);
                }
            }
        }
        unbound.insert(
            self.name.to_string(),
            UnboundNativeMethods::ClassAlreadyInitialized,
        );
        drop(unbound);

        // Preparation
        self.prepare(jvm)?;

        // Initialization
        if let Some(initializer) = self.method("<clinit>", vec![], None) {
            crate::interpreter::invoke_initializer(jvm, initializer)?;
        }

        Ok(())
    }

    pub fn can_access_class(&self, other: ClassRef) -> bool {
        other.access_flags.contains(AccessFlags::PUBLIC) || (*self.package == *other.package)
    }

    pub fn is_member_accessible_from(
        &'static self,
        member_flags: AccessFlags,
        source: ClassRef,
    ) -> bool {
        member_flags.contains(AccessFlags::PUBLIC)
            || member_flags.contains(AccessFlags::PROTECTED) & source.subclass_of(self) // TODO full check (5.4.4)
            || (member_flags.contains(AccessFlags::PROTECTED)
                | !member_flags.intersects(AccessFlags::PUBLIC | AccessFlags::PRIVATE))
                & (*self.package == *source.package)
            || (self == source)
    }
}

impl core::fmt::Display for Class {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl core::fmt::Debug for Class {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.name)
    }
}
