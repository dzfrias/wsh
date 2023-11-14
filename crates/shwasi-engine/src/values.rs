use std::mem;

use shwasi_parser::{RefType, ValType};

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
    /// A reference to an extern.
    ExternRef(Ref),
}

impl Value {
    #[inline]
    pub fn as_u32(self) -> u32 {
        match self {
            Self::I32(i32) => i32,
            _ => panic!("Expected i32, found {:?}", self),
        }
    }

    #[inline]
    pub fn as_i32(self) -> i32 {
        match self {
            Self::I32(i32) => unsafe { std::mem::transmute(i32) },
            _ => panic!("Expected i32, found {:?}", self),
        }
    }

    #[inline]
    pub fn as_u64(self) -> u64 {
        match self {
            Self::I64(i64) => i64,
            _ => panic!("Expected i64, found {:?}", self),
        }
    }

    #[inline]
    pub fn as_f32(self) -> f32 {
        match self {
            Self::F32(f32) => f32,
            _ => panic!("Expected f32, found {:?}", self),
        }
    }

    #[inline]
    pub fn as_f64(self) -> f64 {
        match self {
            Self::F64(f64) => f64,
            _ => panic!("Expected f64, found {:?}", self),
        }
    }

    #[inline]
    pub fn as_i64(self) -> i64 {
        match self {
            Self::I64(i64) => unsafe { std::mem::transmute(i64) },
            _ => panic!("Expected i64, found {:?}", self),
        }
    }

    #[inline]
    pub fn as_ref(self) -> Ref {
        match self {
            Self::Ref(ref_) => ref_,
            _ => panic!("Expected Ref, found {:?}", self),
        }
    }

    #[inline]
    pub fn is_true(&self) -> bool {
        match self {
            Self::I32(i32) => *i32 != 0,
            _ => panic!("Expected i32, found {:?}", self),
        }
    }

    #[inline]
    pub fn is_false(&self) -> bool {
        match self {
            Self::I32(i32) => *i32 == 0,
            _ => panic!("Expected i32, found {:?}", self),
        }
    }

    pub fn is_null(&self) -> bool {
        matches!(self, Self::NullRef(_))
    }

    #[inline]
    pub fn type_default(ty: ValType) -> Self {
        match ty {
            ValType::I32 => Self::I32(0),
            ValType::I64 => Self::I64(0),
            ValType::F32 => Self::F32(0.0),
            ValType::F64 => Self::F64(0.0),
            ValType::Func => Self::Ref(0),
            ValType::Extern => Self::ExternRef(0),
        }
    }
}

impl From<bool> for Value {
    #[inline]
    fn from(b: bool) -> Self {
        Self::I32(if b { 1 } else { 0 })
    }
}

impl From<i32> for Value {
    #[inline]
    fn from(i32: i32) -> Self {
        Self::I32(unsafe { mem::transmute(i32) })
    }
}

impl From<i64> for Value {
    #[inline]
    fn from(i64: i64) -> Self {
        Self::I64(unsafe { mem::transmute(i64) })
    }
}

impl From<u64> for Value {
    #[inline]
    fn from(u64: u64) -> Self {
        Self::I64(u64)
    }
}

impl From<u32> for Value {
    #[inline]
    fn from(u32: u32) -> Self {
        Self::I32(u32)
    }
}

impl From<f32> for Value {
    #[inline]
    fn from(f32: f32) -> Self {
        Self::F32(f32)
    }
}

impl From<f64> for Value {
    #[inline]
    fn from(f64: f64) -> Self {
        Self::F64(f64)
    }
}
