use std::{
    borrow::Cow,
    env, fmt,
    fs::{self, File},
    io::{self, Write},
    path::{Path, PathBuf},
    rc::Rc,
};

use filedescriptor::{AsRawFileDescriptor, FromRawFileDescriptor, IntoRawFileDescriptor};
use path_absolutize::Absolutize;
use rand::{distributions::Alphanumeric, Rng};

use crate::interpreter::memfs::{self, MemFs};

/// A file that exists in memory.
#[derive(Clone)]
pub struct MemFile {
    inner: Rc<File>,
    path: PathBuf,
}

impl MemFile {
    /// Open a new in-memory file at the given path.
    pub fn open(mem_fs: MemFs, path: impl AsRef<Path>) -> io::Result<Self> {
        let path = env::current_dir()
            .and_then(fs::canonicalize)
            .map(|dir| {
                Path::new(path.as_ref())
                    .absolutize_from(dir)
                    .expect("this method should never fail")
            })
            .unwrap_or(Cow::Borrowed(Path::new(path.as_ref())));
        let Some(memfs::Entry::File(memfs_file)) = mem_fs.entry(&path) else {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("in-memory file `{}` not found", path.display()),
            ));
        };
        if cfg!(unix) {
            let (reader, mut writer) = os_pipe::pipe()?;
            writer.write_all(memfs_file.borrow().data())?;
            // SAFETY: `reader` is a valid file descriptor open for reading
            Ok(Self {
                inner: unsafe {
                    File::from_raw_file_descriptor(reader.into_raw_file_descriptor()).into()
                },
                path: path.into_owned(),
            })
        } else {
            // Right now, the implementation on non-Unix platforms uses real temporary files. Is
            // there a good way to do this on Windows? Named pipes are an option, but they have to
            // be specially treated by the programs that use them (unlike /dev/fd files).
            let rand_name = rand::thread_rng()
                .sample_iter(&Alphanumeric)
                .take(16)
                .map(char::from)
                .collect::<String>();
            let path = env::temp_dir().join(rand_name);
            let mut file = File::create(&path)?;
            file.write_all(memfs_file.borrow().data())?;
            Ok(Self {
                inner: Rc::new(file),
                path,
            })
        }
    }

    /// Return the inner `File` of the in-memory file, or [`None`] if there is another reference to
    /// it.
    pub fn into_inner(self) -> Option<File> {
        Rc::try_unwrap(self.inner).ok()
    }

    /// Return the owned path to the file.
    #[allow(dead_code)]
    pub fn into_path(self) -> PathBuf {
        self.path
    }

    /// Return the path of the file.
    #[allow(dead_code)]
    pub fn path(&self) -> &Path {
        &self.path
    }
}

impl fmt::Display for MemFile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #[cfg(unix)]
        {
            let fd = self.inner.as_raw_file_descriptor();
            write!(f, "/dev/fd/{fd}")
        }
        #[cfg(windows)]
        write!(f, "{}", self.path().display())
    }
}
