#![allow(dead_code)]

mod error;

use std::{
    any::Any,
    collections::HashMap,
    io::{self, Read, Seek, Write},
    path::{Path, PathBuf},
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard, Weak},
};

use shwasi_wasi::{FdFlags, FileType, OFlags, OpenResult, WasiDir, WasiError, WasiFile};
use system_interface::io::{Peek, ReadReady};

pub use error::MemFsError;

/// An in-memory file system.
///
/// Internally, this uses a reference-counted pointer for cheap cloning.
#[derive(Debug, Clone)]
pub struct MemFs(Arc<RwLock<MemFsInner>>);

impl MemFs {
    /// Create a new in-memory file system.
    pub fn new() -> Self {
        Self(Arc::new(RwLock::new(MemFsInner::default())))
    }

    /// Obtain an `Entry` at `path`, returning `None` if it does not exist.
    pub fn entry(&self, path: impl AsRef<Path>) -> Option<Entry> {
        self.borrow().path_table.get(path.as_ref()).cloned()
    }

    /// Create a new directory at the **absolute** `path`.
    ///
    /// This will return:
    /// - MemFsError::AlreadyExists if the directory already exists
    pub fn create_dir(&self, path: impl AsRef<Path>) -> Result<MemDir, MemFsError> {
        if path.as_ref().is_relative() {
            panic!("should not provide a relative path to a MemDir");
        }
        if self.entry(path.as_ref()).is_some() {
            return Err(MemFsError::AlreadyExists);
        }

        let mut borrow = self.borrow_mut();
        let dir = MemDir::new(self.downgrade(), path.as_ref());
        borrow
            .path_table
            .insert(path.as_ref().to_path_buf(), Entry::Directory(dir.clone()));
        // Drop here because `try_unremove` will make a mutable borrow
        drop(borrow);
        self.try_unremove(path);

        Ok(dir)
    }

    /// Create a new file at the **absolute** `path`. This will overwrite the previous file, unless
    /// `excl` is passed.
    ///
    /// This will return:
    /// - MemFsError::IsDir if the path is already taken by a directory.
    /// - MemFsError::AlreadyExists if `excl` is true and the file already exists.
    pub fn create_file(&self, path: impl AsRef<Path>, excl: bool) -> Result<MemFile, MemFsError> {
        if path.as_ref().is_relative() {
            panic!("should not provide a relative path to a MemFile");
        }

        if let Some(entry) = self.entry(path.as_ref()) {
            return match entry {
                Entry::Directory(_) => Err(MemFsError::IsDir),
                Entry::File(file) if !excl => {
                    file.truncate();
                    Ok(file)
                }
                Entry::File(_) => Err(MemFsError::AlreadyExists),
            };
        }

        let mut borrow = self.borrow_mut();
        let file = MemFile::empty(self.downgrade(), path.as_ref());
        borrow
            .path_table
            .insert(path.as_ref().to_path_buf(), Entry::File(file.clone()));
        drop(borrow);
        self.try_unremove(path);

        Ok(file)
    }

    /// Run `f` for each file system entry (in no particular order).
    pub fn for_each<F>(&self, f: F)
    where
        F: FnMut(Entry),
    {
        let borrow = self.borrow();
        borrow.path_table.values().cloned().for_each(f);
    }

    /// Run `f` for each file removed in the file system.
    pub fn for_each_removal<F>(&self, f: F)
    where
        F: FnMut(&Path),
    {
        self.borrow()
            .removals
            .iter()
            .map(PathBuf::as_path)
            .for_each(f);
    }

    /// Get the number of total entries in the file system.
    pub fn entry_count(&self) -> usize {
        self.borrow().path_table.len()
    }

    /// Check if there are no entries in the file system.
    pub fn is_empty(&self) -> bool {
        self.entry_count() == 0
    }

    /// Check if a path exists.
    pub fn exists(&self, path: impl AsRef<Path>) -> bool {
        self.borrow().path_table.get(path.as_ref()).is_some()
    }

    /// Unlink the file from the file system.
    ///
    /// This returns
    /// - MemFsError::IsDir if the handle points to a directory.
    /// - MemFsError::Noent if the file does not exist.
    pub fn unlink(&self, path: impl AsRef<Path>) -> Result<(), MemFsError> {
        let path = path.as_ref();
        let Some(entry) = self.entry(path) else {
            if path.exists() {
                if path.is_dir() {
                    return Err(MemFsError::IsDir);
                }
                self.did_remove(path);
                return Ok(());
            }
            return Err(MemFsError::Noent);
        };
        let Entry::File(file) = entry else {
            return Err(MemFsError::IsDir);
        };
        let mut borrow = self.borrow_mut();
        let file_borrow = file.borrow();
        let path = file_borrow.path();
        borrow.path_table.remove(path);
        drop(borrow);
        self.did_remove(path);

        Ok(())
    }

    /// Remove a directory from the file system.
    ///
    /// Returns:
    /// - MemFsError::Notdir if the entry is not a directory
    /// - MemFsError::NotEmpty if the directory is not empty
    /// - MemFsError::Noent if the directory does not exist
    pub fn remove_dir(&self, path: impl AsRef<Path>) -> Result<(), MemFsError> {
        let path = path.as_ref();
        let Some(entry) = self.entry(path) else {
            if path.exists() {
                if path.is_file() {
                    return Err(MemFsError::Notdir);
                }
                self.did_remove(path);
                return Ok(());
            }
            return Err(MemFsError::Noent);
        };
        let Entry::Directory(dir) = entry else {
            return Err(MemFsError::Notdir);
        };
        if !dir.is_empty() {
            return Err(MemFsError::NotEmpty);
        }
        let mut borrow = self.borrow_mut();
        let dir_borrow = dir.borrow();
        let path = dir_borrow.path();
        borrow.path_table.remove(path);
        drop(borrow);
        self.did_remove(path);

        Ok(())
    }

    fn try_unremove(&self, path: impl AsRef<Path>) {
        let mut borrow = self.borrow_mut();
        let Some(pos) = borrow
            .removals
            .iter()
            .position(|removed| path.as_ref() == removed)
        else {
            return;
        };
        borrow.removals.swap_remove(pos);
    }

    fn did_remove(&self, path: impl AsRef<Path>) {
        self.borrow_mut().removals.push(path.as_ref().to_path_buf());
    }

    fn downgrade(&self) -> WeakMemFs {
        WeakMemFs(Arc::downgrade(&self.0))
    }

    fn borrow(&self) -> RwLockReadGuard<MemFsInner> {
        self.0.read().unwrap()
    }

    fn borrow_mut(&self) -> RwLockWriteGuard<MemFsInner> {
        self.0.write().unwrap()
    }
}

/// An entry into the file system. This can either be a MemFile or a MemDir.
#[derive(Debug, Clone)]
pub enum Entry {
    Directory(MemDir),
    File(MemFile),
}

/// A weak pointer to a `MemFs`.
///
/// This is used to prevent reference cycles in the file system. Note that as of right now, all
/// methods of `WeakMemFs` will panic if the originating `MemFs` is ever dropped.
#[derive(Debug)]
struct WeakMemFs(Weak<RwLock<MemFsInner>>);

macro_rules! impl_weak {
    ($(fn $name:ident($($arg_name:ident: $arg_ty:ty),*) $(-> $return:ty)?);* $(;)?) => {
        impl WeakMemFs {
            $(
                pub fn $name(&self, $($arg_name: $arg_ty),*) $(-> $return)? {
                    self.upgrade().$name($($arg_name),*)
                }
            )*
        }
    };
}

impl_weak! {
    fn entry(path: impl AsRef<Path>) -> Option<Entry>;
    fn create_dir(path: impl AsRef<Path>) -> Result<MemDir, MemFsError>;
    fn create_file(path: impl AsRef<Path>, excl: bool) -> Result<MemFile, MemFsError>;
    fn entry_count() -> usize;
    fn is_empty() -> bool;
    fn exists(path: impl AsRef<Path>) -> bool;
    fn unlink(handle: impl AsRef<Path>) -> Result<(), MemFsError>;
    fn remove_dir(handle: impl AsRef<Path>) -> Result<(), MemFsError>;
}

impl WeakMemFs {
    pub fn for_each<F>(&self, f: F)
    where
        F: FnMut(Entry),
    {
        self.upgrade().for_each(f);
    }

    pub fn for_each_removal<F>(&self, f: F)
    where
        F: FnMut(&Path),
    {
        self.upgrade().for_each_removal(f);
    }

    fn upgrade(&self) -> MemFs {
        MemFs(self.0.upgrade().expect("MemFs went out of scope! If you are using WeakMemFs, make sure that the originating MemFs lives longer!"))
    }
}

#[derive(Debug, Default)]
struct MemFsInner {
    removals: Vec<PathBuf>,
    path_table: HashMap<PathBuf, Entry>,
}

/// An in-memory directory in the file system.
///
/// This uses a reference-counted pointer internally, so cloning is cheap.
#[derive(Debug, Clone)]
pub struct MemDir(Arc<RwLock<MemDirInner>>);

#[derive(Debug)]
pub struct MemDirInner {
    fs: WeakMemFs,
    path: PathBuf,
}

impl MemDirInner {
    pub fn path(&self) -> &Path {
        &self.path
    }
}

impl MemDir {
    /// Create a new `MemDir` at the provided (**absolute**) path.
    fn new(fs: WeakMemFs, path: impl AsRef<Path>) -> Self {
        Self(Arc::new(RwLock::new(MemDirInner {
            fs,
            path: path.as_ref().to_path_buf(),
        })))
    }

    /// Iterate through each file in the in-memory directory.
    ///
    /// A direct `Iterator` cannot be provided as a result of RwLock guards.
    pub fn for_each<F>(&self, mut f: F)
    where
        F: FnMut(Entry),
    {
        let borrow = self.borrow();
        borrow.fs.for_each(|entry| match entry {
            Entry::Directory(ref dir) if self.is_child(dir.borrow().path()) => f(entry),
            Entry::File(ref file) if self.is_child(file.borrow().path()) => f(entry),
            _ => {}
        });
    }

    /// Get the number of entries in the directory,
    pub fn num_entries(&self) -> usize {
        let mut total = 0;
        self.for_each(|_| {
            total += 1;
        });
        total
    }

    /// Check if the directory is empty.
    pub fn is_empty(&self) -> bool {
        self.num_entries() == 0
    }

    /// Unlink a file at `path`.
    pub fn unlink(&self, path: &str) -> Result<(), MemFsError> {
        let path = self.borrow().path.join(path);
        self.borrow().fs.unlink(path)?;
        Ok(())
    }

    /// Create a directory at `path`.
    pub fn create_dir(&self, path: &str) -> Result<MemDir, MemFsError> {
        let path = self.borrow().path.join(path);
        let dir = self.borrow().fs.create_dir(path)?;
        Ok(dir)
    }

    /// Remove a directory at `path`.
    pub fn remove_dir(&self, path: &str) -> Result<(), MemFsError> {
        let path = self.borrow().path.join(path);
        self.borrow().fs.remove_dir(path)?;

        Ok(())
    }

    /// Open an entry at `path`.
    pub fn open(
        &self,
        path: &str,
        oflags: OFlags,
        read: bool,
        write: bool,
        fd_flags: FdFlags,
    ) -> Result<Entry, MemFsError> {
        if oflags.contains(OFlags::DIRECTORY)
            && oflags.intersects(OFlags::CREATE | OFlags::EXCLUSIVE | OFlags::TRUNCATE)
        {
            return Err(MemFsError::IsDir);
        }
        if fd_flags.intersects(FdFlags::DSYNC | FdFlags::SYNC | FdFlags::RSYNC) {
            return Err(MemFsError::Inval);
        }
        if !write
            && !fd_flags.contains(FdFlags::APPEND)
            && oflags.intersects(OFlags::CREATE | OFlags::EXCLUSIVE)
        {
            return Err(MemFsError::Acces);
        }
        if !write && oflags.contains(OFlags::TRUNCATE) {
            return Err(MemFsError::Acces);
        }

        if oflags.intersects(OFlags::CREATE | OFlags::EXCLUSIVE) {
            return Ok(Entry::File(self.create(path, fd_flags, oflags, read)?));
        }

        let path = self.borrow().path.join(path);
        // If it's on our real filesystem, and but not in our in-memory one, we should load it into
        // memory.
        if path.exists() && !self.borrow().fs.exists(&path) {
            let contents = std::fs::read(&path).map_err(|err| MemFsError::from(err.kind()))?;
            let file = self.borrow().fs.create_file(&path, false)?;
            file.borrow_mut()
                .data
                .write_all(&contents)
                .map_err(|err| MemFsError::from(err.kind()))?;
        }

        // Files should not have to be created from this point onwards
        // TODO: what happens if a file is created with an absolute path, but then is retrieved
        // with a relative path?
        //   CREATE /Users/dzfrias/code/shwasi/test.txt
        //   GET ../test.txt FROM /Users/dzfrias/code/shwasi/tests/test.txt
        // Would error. In order to solve this, all .. and . components would have to be resolved
        // in the file system, which is a simple fix but still important.
        let handle = self.borrow();
        let file = match handle.fs.entry(path).ok_or(MemFsError::Noent)? {
            Entry::File(file) => {
                if oflags.contains(OFlags::DIRECTORY) {
                    return Err(MemFsError::Notdir);
                }
                file
            }
            Entry::Directory(dir) => {
                return Ok(Entry::Directory(dir));
            }
        };
        file.open(fd_flags, write, read);
        if oflags.contains(OFlags::TRUNCATE) {
            file.truncate();
        }

        Ok(Entry::File(file.clone()))
    }

    fn is_child(&self, path: &Path) -> bool {
        path.starts_with(self.borrow().path())
            && path.components().count() - 1 == self.borrow().path().components().count()
    }

    fn create(
        &self,
        path: &str,
        fd_flags: FdFlags,
        oflags: OFlags,
        read: bool,
    ) -> Result<MemFile, MemFsError> {
        let path = self.borrow().path.join(path);
        let f = self
            .borrow()
            .fs
            .create_file(path, oflags.contains(OFlags::EXCLUSIVE))?;

        // All OFlags::CREATE opens have write or append already set
        f.open(fd_flags, true, read);
        if oflags.contains(OFlags::TRUNCATE) {
            f.truncate();
        }
        Ok(f.clone())
    }

    pub fn borrow(&self) -> RwLockReadGuard<MemDirInner> {
        self.0.read().unwrap()
    }
}

/// An in-memory file.
///
/// This uses a reference-counted pointer internally, so cloning is cheap.
#[derive(Debug, Clone)]
pub struct MemFile(Arc<RwLock<MemFileInner>>);

#[derive(Debug)]
pub struct MemFileInner {
    // We hold on to a weak pointer to prevent an Arc cycle
    fs: WeakMemFs,
    data: io::Cursor<Vec<u8>>,
    write: bool,
    read: bool,
    fd_flags: FdFlags,
    path: PathBuf,
}

impl io::Write for MemFile {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.borrow_mut().data.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.borrow_mut().data.flush()
    }
}

impl MemFile {
    /// Clear the file buffer.
    pub fn truncate(&self) {
        self.borrow_mut().data.get_mut().clear();
    }

    /// Returns the length of the buffer.
    pub fn len(&self) -> usize {
        self.borrow().data.get_ref().len()
    }

    /// Returns `true` if the buffer is empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Open the file with the given flags.
    pub fn open(&self, fd_flags: FdFlags, write: bool, read: bool) {
        if fd_flags.contains(FdFlags::APPEND) {
            self.append();
        }
        let mut borrow = self.borrow_mut();
        borrow.data.set_position(0);
        borrow.read = read;
        borrow.write = write || fd_flags.contains(FdFlags::APPEND);
    }

    /// Returns `true` if the file was opened in read mode.
    pub fn can_read(&self) -> bool {
        self.borrow().read
    }

    /// Returns `true` if the file was opened in write mode.
    pub fn can_write(&self) -> bool {
        self.borrow().write
    }

    pub fn borrow(&self) -> RwLockReadGuard<MemFileInner> {
        self.0.read().unwrap()
    }

    fn with_contents(fs: WeakMemFs, path: impl AsRef<Path>, contents: Vec<u8>) -> Self {
        Self(Arc::new(RwLock::new(MemFileInner {
            fs,
            data: io::Cursor::new(contents),
            write: false,
            read: false,
            fd_flags: FdFlags::empty(),
            path: path.as_ref().to_path_buf(),
        })))
    }

    fn empty(fs: WeakMemFs, path: impl AsRef<Path>) -> Self {
        Self::with_contents(fs, path, vec![])
    }

    fn append(&self) {
        let mut borrow = self.borrow_mut();
        let last = borrow.data.get_ref().len().saturating_sub(1);
        borrow.data.set_position(last as u64);
    }

    fn borrow_mut(&self) -> RwLockWriteGuard<MemFileInner> {
        self.0.write().unwrap()
    }
}

impl MemFileInner {
    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn data(&self) -> &[u8] {
        self.data.get_ref()
    }
}

#[wiggle::async_trait]
impl WasiFile for MemFile {
    fn as_any(&self) -> &dyn Any {
        self
    }

    async fn get_filetype(&self) -> Result<FileType, WasiError> {
        Ok(FileType::RegularFile)
    }

    async fn set_fdflags(&mut self, fdflags: FdFlags) -> Result<(), WasiError> {
        if fdflags.intersects(FdFlags::DSYNC | FdFlags::SYNC | FdFlags::RSYNC) {
            return Err(WasiError::trap(anyhow::anyhow!(
                "cannot set the SYNC family of flags"
            )));
        }
        self.borrow_mut().fd_flags = fdflags;
        Ok(())
    }

    async fn get_fdflags(&self) -> Result<FdFlags, WasiError> {
        Ok(self.borrow().fd_flags)
    }

    async fn read_vectored<'a>(&self, bufs: &mut [io::IoSliceMut<'a>]) -> Result<u64, WasiError> {
        if !self.can_read() {
            return Err(WasiError::trap(anyhow::anyhow!("read not supported")));
        }

        let n = self.borrow_mut().data.read_vectored(bufs)?;
        Ok(n.try_into()?)
    }

    async fn read_vectored_at<'a>(
        &self,
        bufs: &mut [io::IoSliceMut<'a>],
        offset: u64,
    ) -> Result<u64, WasiError> {
        if !self.can_read() {
            return Err(WasiError::trap(anyhow::anyhow!("read not supported")));
        }

        let mut handle = self.borrow_mut();
        let old = handle.data.position();
        handle.data.set_position(offset);
        let n = handle.data.read_vectored(bufs)?;
        handle.data.set_position(old);
        Ok(n.try_into()?)
    }

    async fn write_vectored<'a>(&self, bufs: &[io::IoSlice<'a>]) -> Result<u64, WasiError> {
        if !self.can_write() {
            return Err(WasiError::trap(anyhow::anyhow!("write not supported")));
        }

        let n = self.borrow_mut().data.write_vectored(bufs)?;
        Ok(n.try_into()?)
    }

    async fn write_vectored_at<'a>(
        &self,
        bufs: &[io::IoSlice<'a>],
        offset: u64,
    ) -> Result<u64, WasiError> {
        if !self.can_write() {
            return Err(WasiError::trap(anyhow::anyhow!("write not supported")));
        }

        let mut handle = self.borrow_mut();
        let old = handle.data.position();
        handle.data.set_position(offset);
        let n = handle.data.write_vectored(bufs)?;
        handle.data.set_position(old);
        Ok(n.try_into()?)
    }

    async fn seek(&self, pos: std::io::SeekFrom) -> Result<u64, WasiError> {
        Ok(self.borrow_mut().data.seek(pos)?)
    }

    async fn peek(&self, buf: &mut [u8]) -> Result<u64, WasiError> {
        if !self.can_read() {
            return Err(WasiError::trap(anyhow::anyhow!("read not supported")));
        }

        let n = self.borrow_mut().data.peek(buf)?;
        Ok(n.try_into()?)
    }

    fn num_ready_bytes(&self) -> Result<u64, WasiError> {
        Ok(self.borrow().data.num_ready_bytes()?)
    }

    fn isatty(&self) -> bool {
        false
    }
}

#[wiggle::async_trait]
impl WasiDir for MemDir {
    fn as_any(&self) -> &dyn Any {
        self
    }

    async fn open_file(
        &self,
        _symlink_follow: bool,
        path: &str,
        oflags: OFlags,
        read: bool,
        write: bool,
        fdflags: FdFlags,
    ) -> Result<OpenResult, WasiError> {
        let entry = self.open(path, oflags, read, write, fdflags)?;
        Ok(match entry {
            Entry::Directory(dir) => OpenResult::Dir(Box::new(dir)),
            Entry::File(file) => OpenResult::File(Box::new(file)),
        })
    }

    async fn unlink_file(&self, path: &str) -> Result<(), WasiError> {
        self.unlink(path).map_err(Into::into)
    }

    async fn create_dir(&self, path: &str) -> Result<(), WasiError> {
        self.create_dir(path)?;
        Ok(())
    }

    async fn remove_dir(&self, path: &str) -> Result<(), WasiError> {
        self.remove_dir(path)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_requires_write() {
        let fs = MemFs::new();
        let dir = fs.create_dir("/test").unwrap();
        assert!(dir
            .open("hello.txt", OFlags::CREATE, false, true, FdFlags::empty())
            .is_ok());
        assert!(dir
            .open("hello.txt", OFlags::CREATE, false, false, FdFlags::APPEND)
            .is_ok());
        assert!(dir
            .open("hello.txt", OFlags::CREATE, false, false, FdFlags::empty())
            .is_err());
        assert!(dir
            .open(
                "hello.txt",
                OFlags::EXCLUSIVE,
                false,
                false,
                FdFlags::empty()
            )
            .is_err());
        assert_eq!(1, dir.num_entries());
    }

    #[test]
    fn truncate_requires_write() {
        let fs = MemFs::new();
        let dir = fs.create_dir("/test").unwrap();
        dir.open("hello.txt", OFlags::CREATE, false, true, FdFlags::empty())
            .unwrap();
        assert!(dir
            .open("hello.txt", OFlags::TRUNCATE, false, true, FdFlags::empty())
            .is_ok());
        assert!(dir
            .open("hello.txt", OFlags::TRUNCATE, false, false, FdFlags::APPEND)
            .is_err());
        assert_eq!(1, dir.num_entries());
    }

    #[test]
    #[should_panic]
    fn panic_on_relative_path() {
        let fs = MemFs::new();
        fs.create_dir("relative").unwrap();
    }

    #[test]
    fn create_exclusive() {
        let fs = MemFs::new();
        let dir = fs.create_dir("/test").unwrap();
        dir.open("hello.txt", OFlags::CREATE, false, true, FdFlags::empty())
            .unwrap();
        assert!(dir
            .open(
                "hello.txt",
                OFlags::EXCLUSIVE,
                false,
                true,
                FdFlags::empty()
            )
            .is_err());
        assert!(dir
            .open("new.txt", OFlags::EXCLUSIVE, false, false, FdFlags::APPEND)
            .is_ok());
        assert_eq!(2, dir.num_entries());
    }

    #[test]
    fn directory_oflags_are_exclusive() {
        let fs = MemFs::new();
        let dir = fs.create_dir("/test").unwrap();
        assert!(dir
            .open(
                "hi",
                OFlags::CREATE | OFlags::DIRECTORY,
                false,
                false,
                FdFlags::empty()
            )
            .is_err());
        assert!(dir
            .open(
                "hi",
                OFlags::CREATE | OFlags::EXCLUSIVE,
                false,
                false,
                FdFlags::empty()
            )
            .is_err());
        assert!(dir.is_empty());
    }

    #[test]
    fn cannot_create_entry_over_directory() {
        let fs = MemFs::new();
        fs.create_dir("/test").unwrap();
        assert!(matches!(
            fs.create_file("/test", false),
            Err(MemFsError::IsDir)
        ));
        assert!(matches!(
            fs.create_dir("/test"),
            Err(MemFsError::AlreadyExists)
        ));
        assert_eq!(1, fs.entry_count());
    }

    #[test]
    fn create_file_overwrites_old() {
        let fs = MemFs::new();
        let mut f = fs.create_file("/hello.txt", false).unwrap();
        f.write(b"hi").unwrap();
        let f = fs.create_file("/hello.txt", false).unwrap();
        assert!(f.is_empty());
    }

    #[test]
    fn open_directory_but_got_file_fails() {
        let fs = MemFs::new();
        let dir = fs.create_dir("/test").unwrap();
        dir.open("hello", OFlags::CREATE, false, true, FdFlags::empty())
            .unwrap();
        assert!(matches!(
            dir.open("hello", OFlags::DIRECTORY, false, false, FdFlags::empty()),
            Err(MemFsError::Notdir)
        ));
        assert_eq!(1, dir.num_entries());
    }

    #[test]
    fn unlink() {
        let fs = MemFs::new();
        fs.create_file("/hello.txt", false).unwrap();
        assert!(fs.unlink("/hello.txt").is_ok());
        assert!(fs.is_empty());
        fs.create_dir("/nice").unwrap();
        assert!(matches!(fs.unlink("/nice"), Err(MemFsError::IsDir)));
        assert_eq!(1, fs.entry_count());
    }

    #[test]
    fn dir_create_dir() {
        let fs = MemFs::new();
        let dir = fs.create_dir("/hello").unwrap();
        assert!(dir.create_dir("nice").is_ok());
        assert!(matches!(
            dir.create_dir("nice"),
            Err(MemFsError::AlreadyExists)
        ));
        assert_eq!(1, dir.num_entries());
        assert_eq!(2, fs.entry_count());
    }

    #[test]
    fn remove_dir() {
        let fs = MemFs::new();
        let dir = fs.create_dir("/hello").unwrap();
        dir.create_dir("nice").unwrap();
        let sub_dir = dir.create_dir("woah").unwrap();
        sub_dir
            .open(
                "new_file.txt",
                OFlags::CREATE,
                false,
                true,
                FdFlags::empty(),
            )
            .unwrap();
        dir.open("hello.txt", OFlags::CREATE, false, true, FdFlags::empty())
            .unwrap();
        assert!(dir.remove_dir("nice").is_ok());
        assert!(matches!(dir.remove_dir("nice"), Err(MemFsError::Noent)));
        assert!(matches!(dir.remove_dir("woah"), Err(MemFsError::NotEmpty)));
        assert!(matches!(
            dir.remove_dir("hello.txt"),
            Err(MemFsError::Notdir)
        ));
        assert_eq!(4, fs.entry_count());
    }
}
