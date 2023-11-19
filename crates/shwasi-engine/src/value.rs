use std::mem;

use shwasi_parser::ValType;

/// A typed WebAssembly value.
#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Value {
    I32(u32),
    I64(u64),
    F32(f32),
    F64(f64),
    /// A reference to a function.
    Ref(Ref),
    /// A reference to an extern.
    ExternRef(Ref),
}

impl Value {
    pub const fn ty(&self) -> ValType {
        match self {
            Self::I32(_) => ValType::I32,
            Self::I64(_) => ValType::I64,
            Self::F32(_) => ValType::F32,
            Self::F64(_) => ValType::F64,
            Self::Ref(_) => ValType::Func,
            Self::ExternRef(_) => ValType::Extern,
        }
    }
}

/// A reference.
pub type Ref = Option<u32>;

/// An untyped WebAssembly value.
///
/// Note that because of [validation](https://webassembly.github.io/spec/core/valid/index.html), it
/// is always correct to cast a `ValueUntyped` into a real type, so long as the caller casts to the
/// same type as the validation requires.
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct ValueUntyped(u64);

impl ValueUntyped {
    pub fn as_u32(self) -> u32 {
        self.0 as u32
    }

    pub fn as_i32(self) -> i32 {
        self.0 as i32
    }

    pub fn as_u64(self) -> u64 {
        self.0
    }

    pub fn as_f32(self) -> f32 {
        f32::from_bits(self.0 as u32)
    }

    pub fn as_f64(self) -> f64 {
        f64::from_bits(self.0)
    }

    pub fn as_i64(self) -> i64 {
        self.0 as i64
    }

    pub fn as_ref(self) -> Ref {
        unsafe { mem::transmute(self.0) }
    }

    pub fn is_true(self) -> bool {
        self.0 != 0
    }

    pub fn is_false(self) -> bool {
        !self.is_true()
    }

    pub fn is_null(self) -> bool {
        self.as_ref().is_none()
    }

    pub fn type_default(ty: ValType) -> Self {
        match ty {
            ValType::I32 | ValType::I64 | ValType::F32 | ValType::F64 => Self(0),
            ValType::Func | ValType::Extern => Self(unsafe { mem::transmute::<Ref, _>(None) }),
        }
    }

    pub fn into_typed(self, ty: ValType) -> Value {
        match ty {
            ValType::I32 => Value::I32(self.0 as u32),
            ValType::I64 => Value::I64(self.0),
            ValType::F32 => Value::F32(f32::from_bits(self.0 as u32)),
            ValType::F64 => Value::F64(f64::from_bits(self.0)),
            ValType::Func => Value::Ref(self.as_ref()),
            ValType::Extern => Value::ExternRef(self.as_ref()),
        }
    }
}

impl From<Value> for ValueUntyped {
    fn from(value: Value) -> Self {
        match value {
            Value::I32(n) => Self(n as u64),
            Value::I64(n) => Self(n),
            Value::F32(f) => Self(f.to_bits() as u64),
            Value::F64(f) => Self(f.to_bits()),
            Value::Ref(r) | Value::ExternRef(r) => Self(unsafe { mem::transmute(r) }),
        }
    }
}

impl From<u32> for ValueUntyped {
    #[inline]
    fn from(u32: u32) -> Self {
        Self(u32 as u64)
    }
}

impl From<i32> for ValueUntyped {
    #[inline]
    fn from(i32: i32) -> Self {
        Self(i32 as u64)
    }
}

impl From<u64> for ValueUntyped {
    #[inline]
    fn from(u64: u64) -> Self {
        Self(u64)
    }
}

impl From<i64> for ValueUntyped {
    #[inline]
    fn from(i64: i64) -> Self {
        Self(i64 as u64)
    }
}

impl From<f32> for ValueUntyped {
    #[inline]
    fn from(f32: f32) -> Self {
        Self(f32.to_bits() as u64)
    }
}

impl From<f64> for ValueUntyped {
    #[inline]
    fn from(f64: f64) -> Self {
        Self(f64.to_bits())
    }
}

impl From<bool> for ValueUntyped {
    #[inline]
    fn from(b: bool) -> Self {
        Self(if b { 1 } else { 0 })
    }
}

impl From<Ref> for ValueUntyped {
    #[inline]
    fn from(ref_: Ref) -> Self {
        Self(unsafe { mem::transmute(ref_) })
    }
}

impl From<ValueUntyped> for u32 {
    #[inline]
    fn from(value: ValueUntyped) -> Self {
        value.as_u32()
    }
}

impl From<ValueUntyped> for i32 {
    #[inline]
    fn from(value: ValueUntyped) -> Self {
        value.as_i32()
    }
}

impl From<ValueUntyped> for u64 {
    #[inline]
    fn from(value: ValueUntyped) -> Self {
        value.as_u64()
    }
}

impl From<ValueUntyped> for i64 {
    #[inline]
    fn from(value: ValueUntyped) -> Self {
        value.as_i64()
    }
}

impl From<ValueUntyped> for f32 {
    #[inline]
    fn from(value: ValueUntyped) -> Self {
        Self::from_bits(value.as_u32())
    }
}

impl From<ValueUntyped> for f64 {
    #[inline]
    fn from(value: ValueUntyped) -> Self {
        Self::from_bits(value.as_u64())
    }
}

impl From<ValueUntyped> for bool {
    #[inline]
    fn from(value: ValueUntyped) -> Self {
        value.is_true()
    }
}

impl From<ValueUntyped> for Ref {
    #[inline]
    fn from(value: ValueUntyped) -> Self {
        value.as_ref()
    }
}

impl From<ValueUntyped> for u8 {
    #[inline]
    fn from(value: ValueUntyped) -> Self {
        value.as_u32() as u8
    }
}

impl From<ValueUntyped> for u16 {
    #[inline]
    fn from(value: ValueUntyped) -> Self {
        value.as_u32() as u16
    }
}
