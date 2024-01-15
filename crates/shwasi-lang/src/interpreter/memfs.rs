#![allow(dead_code)]

use std::{
    any::Any,
    collections::HashMap,
    io::{self, Read, Seek, Write},
    path::{Path, PathBuf},
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard, Weak},
};
use system_interface::io::{Peek, ReadReady};
use thiserror::Error;

use shwasi_wasi::{Errno, FdFlags, FileType, OFlags, OpenResult, WasiDir, WasiError, WasiFile};

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

    /// Obtain an `FsHandle` at `path`, returning `None` if it does not exist.
    pub fn entry(&self, path: impl AsRef<Path>) -> Option<FsHandle> {
        self.borrow().path_table.get(path.as_ref()).copied()
    }

    /// Get the file system entry at `handle`
    pub fn get(&self, handle: FsHandle) -> Entry {
        if handle.is_file() {
            self.borrow()
                .files
                .get(handle.as_raw() as usize)
                .map(|file| Entry::File(file.clone()))
                .expect("fs handle should always be valid")
        } else {
            self.borrow()
                .dirs
                .get(handle.as_raw() as usize)
                .map(|dir| Entry::Directory(dir.clone()))
                .expect("fs handle should always be valid")
        }
    }

    /// Create a new directory at the **absolute** `path`.
    ///
    /// This will return:
    /// - MemFsError::AlreadyExists if the directory already exists
    /// - MemFsError::OutOfHandles if there are no more fs handles available.
    pub fn create_dir(&self, path: impl AsRef<Path>) -> Result<MemDir, MemFsError> {
        if path.as_ref().is_relative() {
            panic!("should not provide a relative path to a MemDir");
        }
        if self.entry(path.as_ref()).is_some() {
            return Err(MemFsError::AlreadyExists);
        }

        let mut borrow = self.borrow_mut();
        let idx = borrow.dirs.len();
        let dir = MemDir::new(self.clone().downgrade(), path.as_ref());
        borrow.dirs.push(dir.clone());
        borrow.path_table.insert(
            path.as_ref().to_path_buf(),
            FsHandle::raw(
                idx.try_into().map_err(|_err| MemFsError::OutOfHandles)?,
                false,
            ),
        );
        // Drop here because `try_unremove` will make a mutable borrow
        drop(borrow);
        self.try_unremove(path);

        Ok(dir)
    }

    /// Create a new file at the **absolute** `path`. This will overwrite the previous file, unless
    /// `excl` is passed. If excl is passed, an error will be returned instead.
    ///
    /// This will return:
    /// - MemFsError::OutOfHandles if there are no more fs handles available.
    /// - MemFsError::IsDir if the path is already taken by a directory.
    pub fn create_file(&self, path: impl AsRef<Path>, excl: bool) -> Result<MemFile, MemFsError> {
        if path.as_ref().is_relative() {
            panic!("should not provide a relative path to a MemFile");
        }

        if let Some(handle) = self.entry(path.as_ref()) {
            return match self.get(handle) {
                Entry::Directory(_) => Err(MemFsError::IsDir),
                Entry::File(file) if !excl => {
                    file.truncate();
                    Ok(file)
                }
                Entry::File(_) => Err(MemFsError::AlreadyExists),
            };
        }

        let mut borrow = self.borrow_mut();
        let idx = borrow.files.len();
        let file = MemFile::empty(self.clone().downgrade(), path.as_ref());
        borrow.files.push(file.clone());
        borrow.path_table.insert(
            path.as_ref().to_path_buf(),
            FsHandle::raw(
                idx.try_into().map_err(|_err| MemFsError::OutOfHandles)?,
                true,
            ),
        );
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
        borrow
            .files
            .iter()
            .map(|file| Entry::File(file.clone()))
            .chain(borrow.dirs.iter().map(|dir| Entry::Directory(dir.clone())))
            .for_each(f);
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
        self.borrow().dirs.len() + self.borrow().files.len()
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
    pub fn unlink(&self, handle: FsHandle) -> Result<(), MemFsError> {
        let mut borrow = self.borrow_mut();
        if handle.is_dir() {
            return Err(MemFsError::IsDir);
        }
        let file = borrow.files.swap_remove(handle.as_raw() as usize);
        let path = file.borrow().path.clone();
        borrow.path_table.remove(&path);
        drop(borrow);
        self.did_remove(path);

        Ok(())
    }

    fn did_remove(&self, path: impl AsRef<Path>) {
        self.borrow_mut().removals.push(path.as_ref().to_path_buf());
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

/// A weak pointer to a `MemFs`.
///
/// This is used to prevent reference cycles in the file system. Note that as of right now, all
/// methods of `WeakMemFs` will panic if the originating `MemFs` is ever dropped.
#[derive(Debug)]
struct WeakMemFs(Weak<RwLock<MemFsInner>>);

impl WeakMemFs {
    pub fn entry(&self, path: impl AsRef<Path>) -> Option<FsHandle> {
        self.upgrade().entry(path)
    }

    pub fn get(&self, handle: FsHandle) -> Entry {
        self.upgrade().get(handle)
    }

    pub fn create_dir(&self, path: impl AsRef<Path>) -> Result<MemDir, MemFsError> {
        self.upgrade().create_dir(path)
    }

    pub fn create_file(&self, path: impl AsRef<Path>, excl: bool) -> Result<MemFile, MemFsError> {
        self.upgrade().create_file(path, excl)
    }

    pub fn for_each<F>(&self, f: F)
    where
        F: FnMut(Entry),
    {
        self.upgrade().for_each(f);
    }

    pub fn entry_count(&self) -> usize {
        self.upgrade().entry_count()
    }

    pub fn is_empty(&self) -> bool {
        self.upgrade().is_empty()
    }

    pub fn exists(&self, path: impl AsRef<Path>) -> bool {
        self.upgrade().exists(path)
    }

    pub fn unlink(&self, handle: FsHandle) -> Result<(), MemFsError> {
        self.upgrade().unlink(handle)
    }

    pub fn did_remove(&self, path: impl AsRef<Path>) {
        self.upgrade().did_remove(path);
    }

    pub fn try_unremove(&self, path: impl AsRef<Path>) {
        self.upgrade().try_unremove(path);
    }

    fn upgrade(&self) -> MemFs {
        MemFs(self.0.upgrade().expect("MemFs went out of scope! If you are using WeakMemFs, make sure that the originating MemFs lives longer!"))
    }
}

#[derive(Debug, Default)]
struct MemFsInner {
    files: Vec<MemFile>,
    dirs: Vec<MemDir>,
    removals: Vec<PathBuf>,
    path_table: HashMap<PathBuf, FsHandle>,
}

/// An error type representing possible failures while creating a new entry in the file system.
#[derive(Debug, Clone, Error)]
pub enum MemFsError {
    #[error("is directory")]
    IsDir,
    #[error("out of handles")]
    OutOfHandles,
    #[error("already exists")]
    AlreadyExists,
    #[error("no entry")]
    Noent,
    #[error("invalid")]
    Inval,
    #[error("invalid access")]
    Acces,
    #[error("not dir")]
    Notdir,
    #[error("out of memory")]
    OutOfMemory,
    #[error("address in use")]
    AddrInUse,
    #[error("operation unsupported")]
    Nosys,
    #[error("broken pipe")]
    BrokenPipe,
    #[error("operation timed out")]
    TimedOut,
    #[error("interrupted")]
    Interrupted,
    #[error("other io error")]
    Io,
}

impl From<MemFsError> for WasiError {
    fn from(value: MemFsError) -> Self {
        let errno = match value {
            MemFsError::IsDir => Errno::Isdir,
            MemFsError::OutOfHandles => Errno::Nospc,
            MemFsError::AlreadyExists => Errno::Exist,
            MemFsError::Noent => Errno::Noent,
            MemFsError::Inval => Errno::Inval,
            MemFsError::Acces => Errno::Acces,
            MemFsError::Notdir => Errno::Notdir,
            MemFsError::OutOfMemory => Errno::Nomem,
            MemFsError::AddrInUse => Errno::Addrinuse,
            MemFsError::Nosys => Errno::Nosys,
            MemFsError::BrokenPipe => Errno::Pipe,
            MemFsError::TimedOut => Errno::Timedout,
            MemFsError::Interrupted => Errno::Intr,
            MemFsError::Io => Errno::Io,
        };
        WasiError::from(errno)
    }
}

impl From<io::ErrorKind> for MemFsError {
    fn from(value: io::ErrorKind) -> Self {
        match value {
            io::ErrorKind::NotFound => MemFsError::Noent,
            io::ErrorKind::PermissionDenied => MemFsError::Acces,
            io::ErrorKind::AlreadyExists => MemFsError::AlreadyExists,
            io::ErrorKind::OutOfMemory => MemFsError::OutOfMemory,
            io::ErrorKind::AddrInUse => MemFsError::AddrInUse,
            io::ErrorKind::BrokenPipe => MemFsError::BrokenPipe,
            io::ErrorKind::InvalidInput | io::ErrorKind::InvalidData => MemFsError::Inval,
            io::ErrorKind::TimedOut => MemFsError::TimedOut,
            io::ErrorKind::Interrupted => MemFsError::Interrupted,
            io::ErrorKind::Unsupported => MemFsError::Nosys,
            io::ErrorKind::Other => MemFsError::Io,
            _ => todo!(),
        }
    }
}

/// An entry into the file system. This can either be a MemFile or a MemDir.
#[derive(Debug, Clone)]
pub enum Entry {
    Directory(MemDir),
    File(MemFile),
}

/// A handle into the in-memory file system. Note that it also encodes data about the entry being
/// pointed to.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct FsHandle(u32);

impl FsHandle {
    pub fn raw(raw: u32, file: bool) -> Self {
        if raw > (1 << 31) - 1 {
            panic!("cannot provide fs handle greater than 2^31 - 1");
        }

        Self(raw | (file as u32) << 31)
    }

    pub fn as_raw(&self) -> u32 {
        self.0 & 0x7fffffff
    }

    pub fn is_file(&self) -> bool {
        (self.0 & (1 << 31)) > 0
    }

    pub fn is_dir(&self) -> bool {
        !self.is_file()
    }
}

/// An in-memory directory in the file system.
///
/// This uses a reference-counted pointer internally, so cloning is cheap.
#[derive(Debug, Clone)]
pub struct MemDir(Arc<RwLock<MemDirInner>>);

#[derive(Debug)]
struct MemDirInner {
    fs: WeakMemFs,
    path: PathBuf,
    entries: Vec<FsHandle>,
}

impl MemDir {
    /// Create a new `MemDir` at the provided (**absolute**) path.
    fn new(fs: WeakMemFs, path: impl AsRef<Path>) -> Self {
        Self(Arc::new(RwLock::new(MemDirInner {
            fs,
            path: path.as_ref().to_path_buf(),
            entries: vec![],
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
        for file in borrow.entries.iter() {
            f(borrow.fs.get(*file));
        }
    }

    /// Get the number of entries in the directory,
    pub fn num_entries(&self) -> usize {
        self.borrow().entries.len()
    }

    /// Check if the directory is empty.
    pub fn is_empty(&self) -> bool {
        self.num_entries() == 0
    }

    /// Get the corresponding handle into the file system.
    pub fn handle(&self) -> FsHandle {
        let borrow = self.borrow();
        borrow.fs.entry(&borrow.path).unwrap()
    }

    pub fn remove(&self, path: &str) -> Result<(), MemFsError> {
        let mut borrow = self.borrow_mut();
        let path = borrow.path.join(path);
        // TODO: right now this is an O(n) check. Could get unwieldy?
        let Some(idx) = borrow
            .entries
            .iter()
            .position(|handle| match borrow.fs.get(*handle) {
                Entry::File(file) => path == file.borrow().path(),
                Entry::Directory(_) => false,
            })
        else {
            if path.exists() {
                borrow.fs.did_remove(path);
                return Ok(());
            }

            return Err(MemFsError::Noent);
        };
        let handle = borrow.entries[idx];
        borrow.entries.swap_remove(idx);
        borrow.fs.unlink(handle)?;

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
            let mut borrow = self.borrow_mut();
            let contents = std::fs::read(&path).map_err(|err| MemFsError::from(err.kind()))?;
            let file = borrow.fs.create_file(&path, false)?;
            file.borrow_mut()
                .data
                .write_all(&contents)
                .map_err(|err| MemFsError::from(err.kind()))?;
            borrow.entries.push(file.handle());
        }

        // Files should not have to be created from this point onwards
        // TODO: what happens if a file is created with an absolute path, but then is retrieved
        // with a relative path?
        //   CREATE /Users/dzfrias/code/shwasi/test.txt
        //   GET ../test.txt FROM /Users/dzfrias/code/shwasi/tests/test.txt
        // Would error. In order to solve this, all .. and . components would have to be resolved
        // in the file system, which is a simple fix but still important.
        let handle = self.borrow();
        let file = match handle
            .fs
            .get(handle.fs.entry(path).ok_or(MemFsError::Noent)?)
        {
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

    fn create(
        &self,
        path: &str,
        fd_flags: FdFlags,
        oflags: OFlags,
        read: bool,
    ) -> Result<MemFile, MemFsError> {
        let mut handle = self.borrow_mut();
        let path = handle.path.join(path);

        let f = handle
            .fs
            .create_file(path, oflags.contains(OFlags::EXCLUSIVE))?;
        if !handle.entries.contains(&f.handle()) {
            handle.entries.push(f.handle());
        }

        // All OFlags::CREATE opens have write or append already set
        f.open(fd_flags, true, read);
        if oflags.contains(OFlags::TRUNCATE) {
            f.truncate();
        }
        Ok(f.clone())
    }

    fn borrow(&self) -> RwLockReadGuard<MemDirInner> {
        self.0.read().unwrap()
    }

    fn borrow_mut(&self) -> RwLockWriteGuard<MemDirInner> {
        self.0.write().unwrap()
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

    /// Return the corresponding file-system handle to the file.
    pub fn handle(&self) -> FsHandle {
        let borrow = self.borrow();
        borrow
            .fs
            .entry(&borrow.path)
            .expect("file handle should exist")
    }

    pub fn borrow(&self) -> RwLockReadGuard<MemFileInner> {
        self.0.read().unwrap()
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
        self.remove(path).map_err(Into::into)
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
        assert!(dir
            .open("hello", OFlags::DIRECTORY, false, false, FdFlags::empty())
            .is_err());
        assert_eq!(1, dir.num_entries());
    }

    #[test]
    fn unlink() {
        let fs = MemFs::new();
        let file = fs.create_file("/hello.txt", false).unwrap();
        assert!(fs.unlink(file.handle()).is_ok());
        assert!(fs.is_empty());
        let dir = fs.create_dir("/nice").unwrap();
        assert!(fs.unlink(dir.handle()).is_err());
        assert_eq!(1, fs.entry_count());
    }
}
