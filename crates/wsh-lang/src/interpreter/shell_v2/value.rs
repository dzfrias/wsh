use std::{
    borrow::Cow,
    fmt,
    fs::File,
    io::{self, Write},
    path::Path,
    rc::Rc,
};

use filedescriptor::{AsRawFileDescriptor, FromRawFileDescriptor, IntoRawFileDescriptor};
use path_absolutize::Absolutize;

use crate::interpreter::memfs::{self, MemFs};

#[derive(Clone)]
pub enum Value {
    Number(f64),
    String(Box<str>),
    Boolean(bool),
    MemFile(MemFile),
}

macro_rules! number_binop {
    ($name:ident, $op:tt) => {
        number_binop!(@Number $name, $op);
    };
    (@$kind:ident $name:ident, $op:tt) => {
        pub fn $name(self, other: Self) -> Option<Self> {
            if let (Value::Number(n), Value::Number(m)) = (self.coerce_number(), other.coerce_number())
            {
                Some(Self::$kind(n $op m))
            } else {
                None
            }
        }
    };
}

impl Value {
    pub fn type_(&self) -> ValueType {
        match self {
            Value::Number(_) => ValueType::Number,
            Value::String(_) => ValueType::String,
            Value::Boolean(_) => ValueType::Boolean,
            Value::MemFile(_) => ValueType::MemFile,
        }
    }

    pub fn add(self, other: Self) -> Option<Self> {
        let a = self.coerce_number();
        let b = other.coerce_number();
        let val = match (a, b) {
            (Value::Number(n), Value::Number(m)) => Self::Number(n + m),
            (Value::Number(n), Value::String(s)) => Self::String(format!("{n}{s}").into()),
            (Value::String(s), Value::Number(n)) => Self::String(format!("{s}{n}").into()),
            (Value::String(a), Value::String(b)) => Self::String(format!("{a}{b}").into()),
            _ => return None,
        };
        Some(val)
    }

    number_binop!(sub, -);
    number_binop!(mul, *);
    number_binop!(div, /);
    number_binop!(@Boolean lt, <);
    number_binop!(@Boolean le, <=);
    number_binop!(@Boolean gt, >);
    number_binop!(@Boolean ge, >=);

    pub fn eq(self, other: Self) -> Option<Self> {
        let val = match (self.coerce_number(), other.coerce_number()) {
            (Value::Number(a), Value::Number(b)) => Value::Boolean(a == b),
            (Value::String(a), Value::String(b)) => Value::Boolean(a == b),
            (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(a == b),
            _ => return None,
        };
        Some(val)
    }

    pub fn ne(self, other: Self) -> Option<Self> {
        self.eq(other).and_then(|val| val.bang())
    }

    pub fn sign(self) -> Option<Self> {
        match self.coerce_number() {
            Value::Number(a) => Some(Value::Number(a)),
            Value::Boolean(b) => Some(Value::Number(b as u8 as f64)),
            _ => None,
        }
    }

    pub fn bang(self) -> Option<Self> {
        if let Value::Boolean(b) = self {
            Some(Value::Boolean(!b))
        } else {
            None
        }
    }

    pub fn neg(self) -> Option<Self> {
        if let Value::Number(a) = self.coerce_number() {
            Some(Value::Number(-a))
        } else {
            None
        }
    }

    pub fn coerce_number(self) -> Self {
        if let Value::String(ref s) = self {
            if let Ok(n) = s.parse::<f64>() {
                return Value::Number(n);
            }
        }
        self
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueType {
    Number,
    String,
    Boolean,
    MemFile,
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueType::Number => write!(f, "number"),
            ValueType::String => write!(f, "string"),
            ValueType::Boolean => write!(f, "boolean"),
            ValueType::MemFile => write!(f, "memfile"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::String(s) => write!(f, "{s}"),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::MemFile(_) => write!(f, "<memfile>"),
        }
    }
}

/// A file that exists in memory.
#[derive(Clone)]
pub struct MemFile(Rc<File>);

impl MemFile {
    /// Open a new in-memory file at the given path.
    pub fn open(mem_fs: MemFs, path: impl AsRef<Path>) -> io::Result<Self> {
        let path = path.as_ref();
        let (reader, mut writer) = os_pipe::pipe()?;
        let path = std::env::current_dir()
            .map(|dir| {
                Path::new(path)
                    .absolutize_from(dir)
                    .expect("this method should never fail")
            })
            .unwrap_or(Cow::Borrowed(Path::new(path)));
        let Some(memfs::Entry::File(memfs_file)) = mem_fs.entry(path) else {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                "in-memory file not found",
            ));
        };
        writer.write_all(memfs_file.borrow().data())?;
        // SAFETY: `reader` is a valid file descriptor open for reading
        Ok(Self(unsafe {
            File::from_raw_file_descriptor(reader.into_raw_file_descriptor()).into()
        }))
    }

    /// Dup the inner `File`, returning it.
    pub fn try_clone_inner(&self) -> io::Result<File> {
        self.0.try_clone()
    }
}

impl AsRawFileDescriptor for MemFile {
    fn as_raw_file_descriptor(&self) -> filedescriptor::RawFileDescriptor {
        self.0.as_raw_file_descriptor()
    }
}
