use core::hash::{Hash, Hasher};

use hashbrown::{Equivalent, HashSet};

use crate::{Type, jvm::Jvm, object::Object, value::Value};

#[derive(Default)]
pub(crate) struct StringInterner {
    map: HashSet<InternedString>,
}

impl StringInterner {
    pub(crate) fn intern_string(&mut self, jvm: &Jvm, string: Object) -> Object {
        let class = string.class(jvm.heap.ptrs());
        assert_eq!(class.name.as_ref(), "java.lang.String");
        let Value::Object(chars) = class
            .field("value", Type::Ref("char[]".into()))
            .expect("java.lang.String missing field char[] value")
            .instance_get(jvm.heap.ptrs(), string)
        else {
            unreachable!()
        };
        let chars = chars.expect("String chars array set to null");
        self.map
            .get_or_insert(InternedString { string, chars })
            .string
    }

    pub(crate) fn create_interned_string(&mut self, jvm: &Jvm, str: &str) -> Object {
        if let Some(interned) = self.map.get(&StringHash { str }) {
            interned.string
        } else {
            let object =
                (jvm.class_library_integration.string_constructor)(&crate::Jvm::from(jvm), str)
                    .as_inner(&crate::Jvm::from(jvm));
            let Value::Object(chars) = jvm
                .resolve_class_internal("java.lang.String")
                .expect("Failed to resolve java.lang.String")
                .field("value", Type::Ref("char[]".into()))
                .expect("java.lang.String missing field char[] value")
                .instance_get(jvm.heap.ptrs(), object)
            else {
                unreachable!()
            };
            let chars = chars.expect("String chars array set to null");
            self.map.insert(InternedString {
                string: object,
                chars,
            });
            object
        }
    }
}

#[derive(Eq)]
struct InternedString {
    string: Object,
    chars: Object,
}

impl Hash for InternedString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for index in 0..self.chars.array_length() {
            state.write_u16(self.chars.array_read_char(index));
        }
    }
}

impl PartialEq for InternedString {
    fn eq(&self, other: &Self) -> bool {
        if self.chars.array_length() != other.chars.array_length() {
            return false;
        }
        (0..self.chars.array_length())
            .all(|index| self.chars.array_read_char(index) == other.chars.array_read_char(index))
    }
}

struct StringHash<'a> {
    str: &'a str,
}

impl Hash for StringHash<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for char in self.str.encode_utf16() {
            state.write_u16(char);
        }
    }
}

impl Equivalent<InternedString> for StringHash<'_> {
    fn equivalent(&self, key: &InternedString) -> bool {
        let mut iter = self.str.encode_utf16();
        (0..key.chars.array_length())
            .all(|index| Some(key.chars.array_read_char(index)) == iter.next())
            && iter.next().is_none()
    }
}
