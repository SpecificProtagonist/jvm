pub(crate) struct UnverifiedCode {
    bytecode: Vec<u8>,
    stack_map_table: Vec<StackMapFrame>,
}

pub(crate) struct StackMapFrame {
    pub offset_delta: u16,
    pub info: StackMapFrameInfo,
}

pub(crate) enum StackMapFrameInfo {
    SameFrame,
    SameLocals1StackItem(VerificationType),
    ChopFrame(u8),
    AppendFrame([Option<VerificationType>; 3]),
    FullFrame {
        locals: Box<[VerificationType]>,
        stack: Box<[VerificationType]>,
    },
}

pub(crate) enum VerificationType {
    Top,
    Integer,
    Float,
    Null,
    UninitializedThis,
    ObjectVariable { index: u16 },
    UninitializedVariable { offset: u16 },
    Long,
    Double,
}

// Verification by type checking (TODO: by type inferrence)
