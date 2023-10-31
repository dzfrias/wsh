use shwasi_parser::RefType;

/// A reference.
pub type Ref = usize;

/// A WebAssembly value.
#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Value {
    I32(u32),
    I64(u64),
    F32(f32),
    F64(f64),
    NullRef(RefType),
    /// A reference to a function.
    Ref(Ref),
}

impl Value {
    /// Attempt to convert the [`Value`] into a [`u32`].
    ///
    /// This function will return [`None`] if it is not of the variant [`Value::I32`].
    pub fn to_u32(self) -> Option<u32> {
        match self {
            Self::I32(i32) => Some(i32),
            _ => None,
        }
    }

    /// Attempt to convert the [`Value`] into a [`Ref`].
    ///
    /// This function will return [`None`] if it is not of the variant [`Value::Ref`].
    pub fn to_ref(self) -> Option<Ref> {
        match self {
            Self::Ref(ref_) => Some(ref_),
            _ => None,
        }
    }
}
