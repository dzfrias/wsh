use core::fmt;
use std::{ops::Deref, slice};

#[derive(Clone, Copy)]
pub struct Value(u64);

impl Value {
    pub const TRUE: Value = Value(Self::QNAN | Self::TAG_TRUE);
    pub const FALSE: Value = Value(Self::QNAN | Self::TAG_FALSE);
    pub const NULL: Value = Value(Self::QNAN | Self::TAG_NULL);

    const QNAN: u64 = 0x7ffc000000000000;
    const TAG_NULL: u64 = 1;
    const TAG_FALSE: u64 = 2;
    const TAG_TRUE: u64 = 3;
    const SIGN_BIT: u64 = 1 << 63;

    pub fn type_(self) -> ValueType {
        if (self.0 & Self::QNAN) != Self::QNAN {
            return ValueType::Number;
        }
        if self.0 & Self::SIGN_BIT == Self::SIGN_BIT {
            let ty = unsafe { self.as_obj() }.type_();
            return ValueType::Object(ty);
        }
        // Tag
        match self.0 & 0b11 {
            Self::TAG_FALSE | Self::TAG_TRUE => ValueType::Boolean,
            Self::TAG_NULL => ValueType::Null,
            _ => unreachable!(),
        }
    }

    pub fn bool(b: bool) -> Self {
        if b {
            Self::TRUE
        } else {
            Self::FALSE
        }
    }

    pub fn number(f: f64) -> Self {
        Value(f.to_bits())
    }

    pub fn obj(ptr: ObjPtr) -> Self {
        Self(Self::SIGN_BIT | Self::QNAN | ptr.0 as u64)
    }

    pub unsafe fn as_num(self) -> f64 {
        f64::from_bits(self.0)
    }

    pub unsafe fn as_bool(self) -> bool {
        (self.0 & 0b11) == Self::TAG_TRUE
    }

    pub unsafe fn is_null(self) -> bool {
        (self.0 & 1) == 1
    }

    pub unsafe fn as_obj(self) -> ObjPtr {
        ObjPtr((self.0 & !(Self::SIGN_BIT | Self::QNAN)) as *mut Obj)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // SAFETY: these types can be safely casted
        unsafe {
            match self.type_() {
                ValueType::Number => write!(f, "{}", self.as_num()),
                ValueType::Null => write!(f, "null"),
                ValueType::Boolean => write!(f, "{}", self.as_bool()),
                ValueType::Object(_) => write!(f, "{}", &*self.as_obj()),
            }
        }
    }
}

#[derive(Clone, Copy)]
pub struct ObjPtr(*mut Obj);

impl ObjPtr {
    pub unsafe fn free(self) {
        let _ = Box::from_raw(self.0);
    }
}

impl Deref for ObjPtr {
    type Target = Obj;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.0 as &Self::Target }
    }
}

pub enum Obj {
    String(StringObj),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ObjType {
    String,
}

impl fmt::Display for Obj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Obj::String(s) => write!(f, "{s}"),
        }
    }
}

impl Obj {
    pub fn coerce_number(&self) -> Option<f64> {
        match self {
            Obj::String(s) => s.as_str().parse::<f64>().ok(),
        }
    }

    pub fn string(s: &str) -> ObjPtr {
        let s_obj = StringObj {
            len: s.len(),
            chars: s.as_bytes().as_ptr(),
        };
        let inner = Obj::String(s_obj);
        let ptr = Box::into_raw(Box::new(inner));
        ObjPtr(ptr)
    }

    pub fn type_(&self) -> ObjType {
        match self {
            Obj::String(_) => ObjType::String,
        }
    }
}

pub struct StringObj {
    len: usize,
    chars: *const u8,
}

impl StringObj {
    pub fn as_str(&self) -> &str {
        // SAFETY: it is an invariant of this type to be a valid string slice
        unsafe { std::str::from_utf8(slice::from_raw_parts(self.chars, self.len)).unwrap() }
    }

    pub fn concat(&self, other: &Self) -> ObjPtr {
        let mut s = self.as_ref().to_owned();
        s.push_str(other.as_str());
        // We leak our string so that we don't clean it up when the function ends. This is okay
        // because wsh has automaic memory management, and the returned pointer should be cleaned
        // up later.
        let leak = Box::leak(s.into_boxed_str());
        Obj::string(leak)
    }
}

impl AsRef<str> for StringObj {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Display for StringObj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl PartialEq for StringObj {
    fn eq(&self, other: &Self) -> bool {
        self.as_str().eq(other.as_str())
    }
}

impl Eq for StringObj {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueType {
    Number,
    Null,
    Boolean,
    Object(ObjType),
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueType::Number => write!(f, "number"),
            ValueType::Null => write!(f, "null"),
            ValueType::Boolean => write!(f, "bool"),
            ValueType::Object(ty) => write!(f, "object [{ty}]"),
        }
    }
}

impl fmt::Display for ObjType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ObjType::String => write!(f, "string"),
        }
    }
}
