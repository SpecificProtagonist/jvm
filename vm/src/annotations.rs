use alloc::sync::Arc;

#[allow(dead_code)]
pub(crate) struct Annotation {
    pub typ: Arc<str>,
    pub element_values: Vec<(Arc<str>, AnnotationElementValue)>,
}

#[allow(dead_code)]
pub(crate) enum AnnotationElementValue {
    Boolean(i32),
    Byte(i32),
    Char(i32),
    Short(i32),
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
    String(Arc<str>),
    Enum { typ: Arc<str>, variant: Arc<str> },
    Class(Arc<str>),
    Annotation(Annotation),
    Array(Vec<AnnotationElementValue>),
}
