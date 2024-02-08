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

impl Value {
    pub fn type_(&self) -> ValueType {
        match self {
            Value::Number(_) => ValueType::Number,
            Value::String(_) => ValueType::String,
            Value::Boolean(_) => ValueType::Boolean,
            Value::MemFile(_) => ValueType::MemFile,
        }
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
