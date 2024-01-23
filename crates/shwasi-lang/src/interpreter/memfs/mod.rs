//! # memfs
//!
//! A WASI-compatible in-memory file system.
//!
//! This is meant to be a _layer_ over the disk file system, not as a standlone file manager.
//! As such, disk memory is a "fallback" for the file system. For example, if a path is not found
//! in main memory, it will be checked for its presence on disk. If it exists on disk, it'll be
//! loaded into memfs (and then accessed). In this way, memfs can be thought of as a diff over the
//! real file system. All disk fallbacks are lazy; memfs will **never** load a file into memory
//! extraneously.
//!
//! memfs's MemDir and MemFile respectively implement the two core traits in `wasi_common`:
//! 1. `WasiFile`
//! 2. `WasiDir`
//! While support for all preview1 APIs are still in-progress, common functionality for most
//! programs should work as expected.

#![allow(dead_code)]

mod error;

use std::{
    any::Any,
    collections::HashMap,
    io::{self, Read, Seek, Write},
    path::{Path, PathBuf},
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard, Weak},
};

use itertools::Itertools;
use path_absolutize::Absolutize;
use shwasi_wasi::{
    Errno, ErrorExt, FdFlags, FileType, OFlags, OpenResult, ReaddirCursor, ReaddirEntity, WasiDir,
    WasiError, WasiFile,
};
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

        let dir = MemDir::new(self.downgrade(), path.as_ref(), self.new_inode());
        self.borrow_mut()
            .path_table
            .insert(path.as_ref().to_path_buf(), Entry::Directory(dir.clone()));
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

        let file = MemFile::empty(self.downgrade(), path.as_ref(), self.new_inode());
        self.borrow_mut()
            .path_table
            .insert(path.as_ref().to_path_buf(), Entry::File(file.clone()));
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
    pub fn for_each_removal<F>(&self, mut f: F)
    where
        F: FnMut(&Path, EntryType),
    {
        self.borrow()
            .removals
            .iter()
            .map(|(p, ty)| (p.as_path(), ty))
            .for_each(|(p, ty)| f(p, *ty));
    }

    /// Get the number of total entries in the file system.
    pub fn entry_count(&self) -> usize {
        self.borrow().path_table.len()
    }

    /// Check if there are no entries in the file system.
    pub fn is_empty(&self) -> bool {
        self.entry_count() == 0
    }

    /// Check if a path exists (no fallback to disk, only checks memfs files).
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
                self.did_remove(path, EntryType::File);
                return Ok(());
            }
            return Err(MemFsError::Noent);
        };
        let Entry::File(file) = entry else {
            return Err(MemFsError::IsDir);
        };
        let file_borrow = file.borrow();
        let path = file_borrow.path();
        self.borrow_mut().path_table.remove(path);
        self.did_remove(path, EntryType::File);

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
                self.did_remove(path, EntryType::Directory);
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
        let dir_borrow = dir.borrow();
        let path = dir_borrow.path();
        self.borrow_mut().path_table.remove(path);
        self.did_remove(path, EntryType::Directory);

        Ok(())
    }

    /// Load an entry from the **disk** file system to the in-memory file system. This will **not**
    /// recursively load the entire subtree into memroy in the case of a directory!
    ///
    /// Returns:
    /// - MemFsError::AlreadyExists if the path already exists in memory
    /// - MemFsError::Noent if the path could not be retrieved from disk
    /// - Any errors from `create_file` or `create_dir`
    /// - Any disk io errors encountered
    pub fn load(&self, path: impl AsRef<Path>) -> Result<Entry, MemFsError> {
        let path = path.as_ref();
        if self.exists(path) {
            return Err(MemFsError::AlreadyExists);
        }
        // If it's already been removed in memfs, it shouldn't exist
        if self.borrow().removals.iter().any(|(p, _)| p == path) || !path.try_exists()? {
            return Err(MemFsError::Noent);
        }

        if path.is_file() {
            let contents = std::fs::read(path)?;
            let file = self.create_file(path, false)?;
            file.borrow_mut().data.write_all(&contents)?;
            return Ok(Entry::File(file));
        }

        let dir = self.create_dir(path)?;
        Ok(Entry::Directory(dir))
    }

    /// Rename a file or directory in the file system.
    ///
    /// Returns:
    /// - MemFsError::Noent if the src entry does not exist
    /// - MemFsError::IsDir if the src entry is a file but the dst entry is a directory
    /// - MemFsError::NotDir if the src entry is a directory but the dst entry is a file
    /// - MemFsError::NotEmpty if the dst entry is a directory but is not empty
    pub fn rename(&self, src: impl AsRef<Path>, dst: impl AsRef<Path>) -> Result<(), MemFsError> {
        let src = src.as_ref();
        let dst = dst.as_ref();
        match self
            .entry(src)
            .ok_or(MemFsError::Noent)
            .or_else(|_err| self.load(src))?
        {
            Entry::File(file) => {
                if matches!(self.entry(dst), Some(Entry::Directory(_))) {
                    return Err(MemFsError::IsDir);
                }
                file.borrow_mut().path = dst.to_path_buf();
                self.borrow_mut()
                    .path_table
                    .insert(dst.to_path_buf(), Entry::File(file));
                self.did_remove(src, EntryType::File);
            }
            Entry::Directory(dir) => {
                if let Some(entry) = self.entry(dst) {
                    let Entry::Directory(dst_dir) = entry else {
                        return Err(MemFsError::Notdir);
                    };
                    if !dst_dir.is_empty() {
                        return Err(MemFsError::NotEmpty);
                    }
                }
                dir.borrow_mut().path = dst.to_path_buf();
                self.borrow_mut()
                    .path_table
                    .insert(dst.to_path_buf(), Entry::Directory(dir));
                self.did_remove(src, EntryType::Directory);
            }
        }
        self.borrow_mut()
            .path_table
            .remove(src)
            .expect("src should be in path table");

        Ok(())
    }

    /// Open a file or directory at the given path. There are a number of flags that can be set to
    /// change the behavior and permissions of the opening.
    ///
    /// Returns:
    /// - MemFsError::IsDir if the flags contain DIRECTORY but also CREATE, EXCLUSIVE, or TRUNCATE
    /// - MemFsError::Inval if the flags contain DSYNC, SYNC, or RSYNC
    /// - MemFsError::Acces if the flags contain CREATE or EXCLUSIVE but not opened in write or
    ///                     append mode
    /// - MemFsError::Acces if the flags contain TRUNCATE but not opened in write mode
    /// - MemFsError::AlreadyExists if the path already exists and EXCLUSIVE is set
    /// - MemFsError::IsDir if EXCLUSIVE or CREATE are set but the path points to a directory
    /// - MemFsError::Noent if the entry does not exist in memory or on disk
    /// - MemFsError::Notdir if the flags contain DIRECTORY but the path to open is file
    pub fn open(
        &self,
        path: impl AsRef<Path>,
        oflags: OFlags,
        read: bool,
        write: bool,
        fd_flags: FdFlags,
    ) -> Result<Entry, MemFsError> {
        let path = path.as_ref();
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

        // Files should not have to be created from this point onwards
        if oflags.intersects(OFlags::CREATE | OFlags::EXCLUSIVE) {
            let f = self.create_file(path, oflags.contains(OFlags::EXCLUSIVE))?;
            // All OFlags::CREATE opens have write or append already set
            f.open(fd_flags, true, read);
            if oflags.contains(OFlags::TRUNCATE) {
                f.truncate();
            }
            return Ok(Entry::File(f.clone()));
        }

        // TODO: what happens if a file is created with an absolute path, but then is retrieved
        // with a relative path?
        //   CREATE /Users/dzfrias/code/shwasi/test.txt
        //   GET ../test.txt FROM /Users/dzfrias/code/shwasi/tests/test.txt
        // Would error. In order to solve this, all .. and . components would have to be resolved
        // in the file system, which is a simple fix but still important.
        let entry = self.entry(path).ok_or(MemFsError::Noent).or_else(|err| {
            if !path.try_exists()? {
                return Err(err);
            }
            // If it doesn't exist in memory, but does exist on disk, load it into memory!
            self.load(path)
        })?;

        Ok(match entry {
            Entry::File(file) => {
                if oflags.contains(OFlags::DIRECTORY) {
                    return Err(MemFsError::Notdir);
                }
                file.open(fd_flags, write, read);
                if oflags.contains(OFlags::TRUNCATE) {
                    file.truncate();
                }
                Entry::File(file)
            }
            Entry::Directory(dir) => Entry::Directory(dir),
        })
    }

    /// Increment the garbage collector.
    pub fn gc_step(&self) {
        // Right now, the garbage collector just checks if the age of a file is at or above GC_AGE.
        // This is a pretty simplistic condition, so maybe other things can be factored in as well,
        // like size.
        const GC_AGE: u32 = 5;

        self.borrow_mut().step += 1;
        let borrow = self.borrow();
        let to_cleanup = borrow
            .path_table
            .values()
            .filter_map(|entry| match entry {
                Entry::File(file)
                    // We need to make sure to only clean up files that are not being pointed to by
                    // anything else, and haven't been written to yet.
                    if Arc::strong_count(&file.0) == 1
                        && !file.did_write()
                        && file.age() >= GC_AGE =>
                {
                    Some(file.borrow().path().to_path_buf())
                }
                _ => None,
            })
            .collect_vec();

        drop(borrow);
        for file in to_cleanup {
            self.borrow_mut().path_table.remove(&file);
        }
    }

    pub fn current_step(&self) -> u32 {
        self.borrow().step
    }

    fn try_unremove(&self, path: impl AsRef<Path>) {
        let mut borrow = self.borrow_mut();
        let Some(pos) = borrow
            .removals
            .iter()
            .position(|(removed, _)| path.as_ref() == removed)
        else {
            return;
        };
        borrow.removals.swap_remove(pos);
    }

    fn did_remove(&self, path: impl AsRef<Path>, entry_type: EntryType) {
        self.borrow_mut()
            .removals
            .push((path.as_ref().to_path_buf(), entry_type));
    }

    fn new_inode(&self) -> u64 {
        let old = self.borrow().current_inode;
        self.borrow_mut().current_inode += 1;
        old
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EntryType {
    Directory,
    File,
}

impl Entry {
    /// Returns the entry inode.
    pub fn inode(&self) -> u64 {
        match self {
            Entry::Directory(dir) => dir.inode(),
            Entry::File(file) => file.inode(),
        }
    }

    /// Returns the path of the entry.
    pub fn path(&self) -> PathBuf {
        match self {
            Entry::Directory(dir) => dir.borrow().path().to_path_buf(),
            Entry::File(file) => file.borrow().path().to_path_buf(),
        }
    }

    /// Get the type of the entry.
    pub fn ty(&self) -> EntryType {
        match self {
            Entry::Directory(_) => EntryType::Directory,
            Entry::File(_) => EntryType::File,
        }
    }

    /// Returns `true` if the entry is a directory.
    pub fn is_dir(&self) -> bool {
        matches!(self.ty(), EntryType::Directory)
    }

    /// Returns `true` if the entry is a file.
    pub fn is_file(&self) -> bool {
        matches!(self.ty(), EntryType::File)
    }
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
    fn exists(path: impl AsRef<Path>) -> bool;
    fn unlink(handle: impl AsRef<Path>) -> Result<(), MemFsError>;
    fn remove_dir(handle: impl AsRef<Path>) -> Result<(), MemFsError>;
    fn load(path: impl AsRef<Path>) -> Result<Entry, MemFsError>;
    fn rename(src: impl AsRef<Path>, dst: impl AsRef<Path>) -> Result<(), MemFsError>;
    fn open(path: impl AsRef<Path>, oflags: OFlags, read: bool, write: bool, fd_flags: FdFlags) -> Result<Entry, MemFsError>;
    fn current_step() -> u32;
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
        F: FnMut(&Path, EntryType),
    {
        self.upgrade().for_each_removal(f);
    }

    fn upgrade(&self) -> MemFs {
        MemFs(self.0.upgrade().expect("MemFs went out of scope! If you are using WeakMemFs, make sure that the originating MemFs lives longer!"))
    }
}

#[derive(Debug, Default)]
struct MemFsInner {
    /// A structure that contains everything that has been removed from the original file system.
    /// We store EntryType along with the path because we no longer have access to its type (on
    /// disk) once it has been removed.
    removals: Vec<(PathBuf, EntryType)>,
    // TODO: maybe some sort of memory management would be helpful here. If a file has only been
    // read from disk, with no modifications, it can be removed from the file system and
    // deallocated. A garbage collector, of sorts.
    path_table: HashMap<PathBuf, Entry>,
    current_inode: u64,
    step: u32,
}

/// An in-memory directory in the file system.
///
/// This uses a reference-counted pointer internally, so cloning is cheap.
#[derive(Debug, Clone)]
pub struct MemDir(Arc<RwLock<MemDirInner>>);

#[derive(Debug)]
pub struct MemDirInner {
    inode: u64,
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
    fn new(fs: WeakMemFs, path: impl AsRef<Path>, inode: u64) -> Self {
        Self(Arc::new(RwLock::new(MemDirInner {
            fs,
            inode,
            path: path.as_ref().to_path_buf(),
        })))
    }

    /// Iterate through each file in the in-memory directory with no particular order.
    ///
    /// A direct `Iterator` cannot be provided as a result of RwLock guards.
    pub fn for_each<F>(&self, mut f: F)
    where
        F: FnMut(Entry),
    {
        let borrow = self.borrow();
        // TODO: this is linearly increasing over all total entries in the file system, doing a
        // moderately expensive string comparison with each. Would be nice to have something clever
        // here.
        borrow.fs.for_each(|entry| {
            if self.is_child(&entry.path()) {
                f(entry);
            }
        });
    }

    /// Get the number of entries in the directory,
    // TODO: this actually needs to check how many files exist on disk (if the directory is
    // present), combined with the in memory files that only exist in memory and not on disk.
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
        let path = self.absolute(path);
        self.borrow().fs.unlink(path)?;
        Ok(())
    }

    /// Create a directory at `path`.
    pub fn create_dir(&self, path: &str) -> Result<MemDir, MemFsError> {
        let path = self.absolute(path);
        let dir = self.borrow().fs.create_dir(path)?;
        Ok(dir)
    }

    /// Remove a directory at `path`.
    pub fn remove_dir(&self, path: &str) -> Result<(), MemFsError> {
        let path = self.absolute(path);
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
        let path = self.absolute(path);
        self.borrow().fs.open(path, oflags, read, write, fd_flags)
    }

    /// Return the directory inode.
    pub fn inode(&self) -> u64 {
        self.borrow().inode
    }

    fn is_child(&self, path: &Path) -> bool {
        path.starts_with(self.borrow().path())
            && path.components().count() - 1 == self.borrow().path().components().count()
    }

    fn absolute(&self, path: impl AsRef<Path>) -> PathBuf {
        path.as_ref()
            .absolutize_from(self.borrow().path())
            .unwrap()
            .to_path_buf()
    }

    pub fn borrow(&self) -> RwLockReadGuard<MemDirInner> {
        self.0.read().unwrap()
    }

    pub fn borrow_mut(&self) -> RwLockWriteGuard<MemDirInner> {
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
    inode: u64,
    data: io::Cursor<Vec<u8>>,
    write: bool,
    read: bool,
    fd_flags: FdFlags,
    path: PathBuf,
    did_write: bool,
    lifetime: u32,
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
        self.borrow_mut().did_write = true;
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

    /// Returns `true` if the inner contents of the file were modified by WASI
    pub fn did_write(&self) -> bool {
        self.borrow().did_write
    }

    /// Returns the age of the file (in GC steps)
    pub fn age(&self) -> u32 {
        self.borrow().fs.current_step() - self.borrow().lifetime
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

    /// Return the file inode.
    pub fn inode(&self) -> u64 {
        self.borrow().inode
    }

    fn with_contents(fs: WeakMemFs, path: impl AsRef<Path>, inode: u64, contents: Vec<u8>) -> Self {
        let step = fs.current_step();
        Self(Arc::new(RwLock::new(MemFileInner {
            fs,
            inode,
            data: io::Cursor::new(contents),
            write: false,
            read: false,
            fd_flags: FdFlags::empty(),
            path: path.as_ref().to_path_buf(),
            did_write: false,
            lifetime: step,
        })))
    }

    fn empty(fs: WeakMemFs, path: impl AsRef<Path>, inode: u64) -> Self {
        Self::with_contents(fs, path, inode, vec![])
    }

    fn update_lifetime(&self) {
        let step = self.borrow().fs.current_step();
        self.borrow_mut().lifetime = step;
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
            return Err(WasiError::not_supported().context("SYNC family of fd flags"));
        }
        self.update_lifetime();
        self.borrow_mut().fd_flags = fdflags;
        Ok(())
    }

    async fn get_fdflags(&self) -> Result<FdFlags, WasiError> {
        self.update_lifetime();
        Ok(self.borrow().fd_flags)
    }

    async fn read_vectored<'a>(&self, bufs: &mut [io::IoSliceMut<'a>]) -> Result<u64, WasiError> {
        if !self.can_read() {
            return Err(WasiError::badf().context("read vectored"));
        }
        self.update_lifetime();

        let n = self.borrow_mut().data.read_vectored(bufs)?;
        Ok(n.try_into()?)
    }

    async fn read_vectored_at<'a>(
        &self,
        bufs: &mut [io::IoSliceMut<'a>],
        offset: u64,
    ) -> Result<u64, WasiError> {
        if !self.can_read() {
            return Err(WasiError::badf().context("read vectored at"));
        }
        self.update_lifetime();

        let mut handle = self.borrow_mut();
        let old = handle.data.position();
        handle.data.set_position(offset);
        let n = handle.data.read_vectored(bufs)?;
        handle.data.set_position(old);
        Ok(n.try_into()?)
    }

    async fn write_vectored<'a>(&self, bufs: &[io::IoSlice<'a>]) -> Result<u64, WasiError> {
        if !self.can_write() {
            return Err(WasiError::badf().context("write vectored"));
        }
        self.update_lifetime();

        self.borrow_mut().did_write = true;
        let n = self.borrow_mut().data.write_vectored(bufs)?;
        Ok(n.try_into()?)
    }

    async fn write_vectored_at<'a>(
        &self,
        bufs: &[io::IoSlice<'a>],
        offset: u64,
    ) -> Result<u64, WasiError> {
        if !self.can_write() {
            return Err(WasiError::badf().context("write vectored at"));
        }
        self.update_lifetime();

        self.borrow_mut().did_write = true;
        let mut handle = self.borrow_mut();
        let old = handle.data.position();
        handle.data.set_position(offset);
        let n = handle.data.write_vectored(bufs)?;
        handle.data.set_position(old);
        Ok(n.try_into()?)
    }

    async fn seek(&self, pos: std::io::SeekFrom) -> Result<u64, WasiError> {
        self.update_lifetime();
        Ok(self.borrow_mut().data.seek(pos)?)
    }

    async fn peek(&self, buf: &mut [u8]) -> Result<u64, WasiError> {
        if !self.can_read() {
            return Err(WasiError::badf().context("peek"));
        }
        self.update_lifetime();

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

    async fn readdir(
        &self,
        cursor: ReaddirCursor,
    ) -> Result<Box<dyn Iterator<Item = Result<ReaddirEntity, WasiError>> + Send>, WasiError> {
        let mut entries = vec![];
        self.for_each(|entry| {
            entries.push(entry);
        });
        // Load any disk entries that aren't in memory yet
        if let Ok(readdir) = std::fs::read_dir(self.borrow().path()) {
            for entry in readdir.filter_map(Result::ok) {
                let Ok(entry) = self.borrow().fs.load(entry.path()) else {
                    continue;
                };
                entries.push(entry);
            }
        }
        let iter = [
            Ok(ReaddirEntity {
                next: 1.into(),
                filetype: FileType::Directory,
                inode: self.inode(),
                name: ".".to_owned(),
            }),
            Ok(ReaddirEntity {
                next: 2.into(),
                filetype: FileType::Directory,
                inode: self.inode(),
                name: "..".to_owned(),
            }),
        ]
        .into_iter()
        .chain(
            entries
                .into_iter()
                // TODO: is it possible to find a way to not have to do this sort? This happens
                // because our internal for_each is in no particular order because of our HashMap.
                .sorted_by_key(|entry| entry.path())
                .enumerate()
                .map(|(i, entry)| {
                    Ok(ReaddirEntity {
                        // Plus 3 because we need to offset the two implicit entries ("." and "..")
                        next: ReaddirCursor::from(i as u64 + 3),
                        filetype: match entry {
                            Entry::Directory(_) => FileType::Directory,
                            Entry::File(_) => FileType::RegularFile,
                        },
                        inode: entry.inode(),
                        name: entry
                            .path()
                            .file_name()
                            .expect("path should never terminate in `..`")
                            .to_str()
                            .map(ToOwned::to_owned)
                            .ok_or(WasiError::from(Errno::Ilseq))?,
                    })
                }),
        )
        .skip(u64::from(cursor) as usize);

        Ok(Box::new(iter))
    }

    async fn rename(
        &self,
        src_path: &str,
        dst: &dyn WasiDir,
        dst_path: &str,
    ) -> Result<(), WasiError> {
        let dst = dst
            .as_any()
            .downcast_ref::<Self>()
            .ok_or(WasiError::badf().context("failed downcast to memfs dir"))?;
        let dst_path = dst.borrow().path().join(dst_path);
        let src_path = self.borrow().path().join(src_path);
        self.borrow().fs.rename(src_path, dst_path)?;
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

    #[test]
    fn rename() {
        let fs = MemFs::new();
        let dir = fs.create_dir("/hello").unwrap();
        let not_empty = fs.create_dir("/not_empty").unwrap();
        not_empty
            .open("hello.txt", OFlags::CREATE, false, true, FdFlags::empty())
            .unwrap();
        let file = fs.create_file("/hello.txt", false).unwrap();
        fs.create_file("/nice.txt", false).unwrap();
        assert!(fs.rename("/hello", "/hi").is_ok());
        assert!(fs.rename("/hello.txt", "/hi.txt").is_ok());
        assert!(matches!(
            fs.rename("/hi", "/nice.txt"),
            Err(MemFsError::Notdir)
        ));
        assert!(matches!(
            fs.rename("/hi", "/not_empty"),
            Err(MemFsError::NotEmpty)
        ));
        not_empty.unlink("hello.txt").unwrap();
        // Should be empty now
        assert!(fs.rename("/not_empty", "/hi").is_ok());
        assert!(matches!(
            fs.rename("/nice.txt", "/hi"),
            Err(MemFsError::IsDir)
        ));
        assert_eq!(Path::new("/hi"), dir.borrow().path());
        assert_eq!(Path::new("/hi.txt"), file.borrow().path());
        assert_eq!(3, fs.entry_count());
    }

    #[test]
    fn get_with_parent_path() {
        let fs = MemFs::new();
        let dir = fs.create_dir("/hello").unwrap();
        fs.create_file("/hello.txt", false).unwrap();
        assert!(dir
            .open(
                "../hello.txt",
                OFlags::empty(),
                true,
                false,
                FdFlags::empty(),
            )
            .is_ok());
    }
}
