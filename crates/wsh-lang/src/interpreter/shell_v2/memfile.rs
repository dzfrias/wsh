use std::{
    borrow::Cow,
    env, fmt,
    fs::{self, File},
    io::{self, Write},
    path::{Path, PathBuf},
    rc::Rc,
};

#[cfg(unix)]
use filedescriptor::AsRawFileDescriptor;
use filedescriptor::{FromRawFileDescriptor, IntoRawFileDescriptor};
use path_absolutize::Absolutize;
use rand::{distributions::Alphanumeric, Rng};

use crate::interpreter::memfs::{self, MemFs};

/// A file that exists in memory.
#[derive(Debug, Clone)]
pub struct MemFile {
    // This is an Option so we can take the inner file without running the destructor.
    inner: Option<Rc<File>>,
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
                    Some(File::from_raw_file_descriptor(reader.into_raw_file_descriptor()).into())
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
                inner: Some(Rc::new(file)),
                path,
            })
        }
    }

    /// Return the inner `File` of the in-memory file, or a duplicated version if an outstanding
    /// reference exists to this `MemFile`.
    ///
    /// On Windows, the caller is responsible for removing the temporary file stored at `path`.
    #[allow(dead_code)]
    pub fn into_inner(mut self) -> io::Result<File> {
        let inner = self.inner.take().unwrap();
        match Rc::try_unwrap(inner) {
            Ok(file) => Ok(file),
            Err(r) => r.try_clone(),
        }
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
            let fd = self.inner.as_ref().unwrap().as_raw_file_descriptor();
            write!(f, "/dev/fd/{fd}")
        }
        #[cfg(windows)]
        write!(f, "{}", self.path().display())
    }
}

#[cfg(windows)]
impl Drop for MemFile {
    fn drop(&mut self) {
        let Some(ref inner) = self.inner else {
            // `into_inner` was called, do not try to remove file
            return;
        };
        if Rc::strong_count(inner) > 1 {
            // There are other things that point to this file, do not remove the file.
            return;
        }
        let _ = fs::remove_file(self.path());
    }
}
