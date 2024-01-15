use std::{
    any::Any,
    collections::HashMap,
    fs,
    hash::Hash,
    io,
    mem::ManuallyDrop,
    path::{Path, PathBuf},
    pin::Pin,
};

use filedescriptor::{AsRawFileDescriptor, FromRawFileDescriptor, RawFileDescriptor};
use shwasi_engine::{Instance, Store, WasmFuncUntyped};
use shwasi_wasi::{sync::file::File, WasiCtxBuilder, WasiDir, WasiError, WasiFile};
use smol_str::SmolStr;

use crate::{
    ast::Pipeline,
    interpreter::{
        memfs::{Entry, MemFs},
        value::Value,
    },
    Ident,
};

use super::memfs::MemFsError;

pub struct Env {
    pub mem_fs: MemFs,

    env: HashMap<Ident, Value>,
    aliases: HashMap<SmolStr, Pipeline>,
    store: Store,
    modules: Vec<Instance>,
    allowed: Vec<Allowed>,
    env_vars: Vec<String>,
    wasi_stdout: RawFileDescriptor,
    wasi_stderr: RawFileDescriptor,
}

impl Env {
    pub fn new() -> Self {
        let mut store = Store::default();
        let mut wasi_ctx = shwasi_wasi::WasiCtxBuilder::new().build();
        // Link the WASI preview 1 snapshot into the store. This is actually just a stand-in. This
        // is only here so that module instantiation doesn't fail. All host functions linked by
        // this function call are replaced every time `prepare_wasi` is called. This is because
        // WasCtx is not designed to work over multiple WASM module's lifetimes, so there are **a
        // lot** of problems when trying to maintain a global refernce counted instance of WasiCtx.
        // Perhaps if a new WASI implementation is designed from scratch, this wouldn't be
        // necessary.
        shwasi_wasi::sync::snapshots::preview_1::link(&mut store, &mut wasi_ctx);
        Self {
            env: HashMap::new(),
            aliases: HashMap::new(),
            modules: Vec::new(),
            store,
            allowed: vec![],
            env_vars: vec![],
            wasi_stderr: io::stderr().as_raw_file_descriptor(),
            wasi_stdout: io::stdout().as_raw_file_descriptor(),
            mem_fs: MemFs::new(),
        }
    }

    pub fn get(&self, sym: &Ident) -> Option<&Value> {
        self.env.get(sym)
    }

    pub fn set(&mut self, sym: Ident, value: Value) {
        self.env.insert(sym, value);
    }

    pub fn set_alias(&mut self, name: SmolStr, expr: Pipeline) {
        self.aliases.insert(name, expr);
    }

    pub fn get_alias(&self, name: &str) -> Option<&Pipeline> {
        self.aliases.get(name)
    }

    pub fn remove_alias(&mut self, name: &str) -> Option<Pipeline> {
        self.aliases.remove(name)
    }

    pub fn register_module(&mut self, instance: Instance) {
        self.modules.push(instance);
    }

    pub fn unload_modules(&mut self) -> usize {
        let len = self.modules.len();
        self.modules.clear();
        self.store.clear();
        let mut wasi_ctx = shwasi_wasi::WasiCtxBuilder::new().build();
        shwasi_wasi::sync::snapshots::preview_1::link(&mut self.store, &mut wasi_ctx);
        len
    }

    pub fn get_module_func(&self, name: &str) -> Option<WasmFuncUntyped> {
        self.modules
            .iter()
            .find_map(|m| m.get_func_untyped(&self.store, name).ok())
    }

    pub fn store(&self) -> &Store {
        &self.store
    }

    pub fn store_mut(&mut self) -> &mut Store {
        &mut self.store
    }

    /// Set the WASI stdout file descriptor.
    ///
    /// # Safety
    /// This function requires a valid file descriptor. It will **not** be taken ownership of.
    pub unsafe fn wasi_stdout(&mut self, fd: RawFileDescriptor) {
        self.wasi_stdout = fd;
    }

    /// Link the WASI API to the store. This should be called before the store is used.
    ///
    /// # Safety
    /// If passed, `stdin` must be a valid file descriptor. It **will** be taken ownership of.
    pub unsafe fn prepare_wasi<I, S>(
        &mut self,
        args: I,
        stdin: Option<RawFileDescriptor>,
        other_env: &[(String, String)],
    ) -> Result<(), WasiError>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        let stdout = unsafe { wasi_file(self.wasi_stdout) };
        let stderr = unsafe { wasi_file(self.wasi_stderr) };
        let mut builder = WasiCtxBuilder::new();
        if let Some(stdin) = stdin {
            let file = unsafe { cap_std::fs::File::from_raw_file_descriptor(stdin) };
            let stdin = File::from_cap_std(file);
            builder.stdin(Box::new(stdin));
        }
        let env = self
            .env_vars
            .iter()
            .filter_map(|var| {
                let value = std::env::var(var).ok()?;
                Some((var.clone(), value))
            })
            .chain(other_env.iter().cloned())
            .collect::<Vec<_>>();
        builder
            .stdout(Box::new(stdout))
            .stderr(Box::new(stderr))
            .envs(&env)
            .expect("should not overflow on environment vars");
        for arg in args {
            builder
                .arg(arg.as_ref())
                .expect("should not overflow on args");
        }

        // We build now because the WasiCtxBuilder API doesn't allow us to use WasiDir's when
        // setting pre-opened directories. Pre-opens are handled by interfacing with WasiCtx
        // directly.
        let mut ctx = builder.build();
        for Allowed { path, location } in &self.allowed {
            let dir: Box<dyn WasiDir> = match location {
                Location::Memory => self
                    .mem_fs
                    .entry(path)
                    .map_or_else(
                        || match self.mem_fs.create_dir(path) {
                            Ok(dir) => Ok(dir),
                            Err(MemFsError::OutOfHandles) => {
                                Err(anyhow::anyhow!("out of fs handles!"))
                            }
                            Err(err) => {
                                panic!("BUG: creating directory should not fail, but got: {err}")
                            }
                        },
                        |handle| match self.mem_fs.get(handle) {
                            Entry::Directory(dir) => Ok(dir),
                            Entry::File(_) => Err(anyhow::anyhow!("cannot pre-open virtual file!")),
                        },
                    )
                    .map(Box::new)
                    .map_err(WasiError::trap)?,
                Location::Disk => {
                    let cap_std_dir =
                        cap_std::fs::Dir::open_ambient_dir(path, cap_std::ambient_authority())?;
                    Box::new(shwasi_wasi::sync::dir::Dir::from_cap_std(cap_std_dir))
                }
            };
            ctx.push_preopened_dir(dir, path)?;
            // Both the relative and absolute paths will have to be pushed here. This WASI
            // implementation makes a distinguishment between the two.
            let Ok(current_dir) = std::env::current_dir() else {
                continue;
            };
            let Some(mut relative) = pathdiff::diff_paths(path, current_dir) else {
                continue;
            };
            if relative == Path::new("") {
                relative = PathBuf::from(".");
            }
            let dir: Box<dyn WasiDir> = match location {
                Location::Memory => match self.mem_fs.get(self.mem_fs.entry(path).unwrap()) {
                    Entry::Directory(dir) => Box::new(dir),
                    Entry::File(_) => {
                        panic!("should not be happen because we just registered it above")
                    }
                },
                Location::Disk => {
                    let cap_std_dir = cap_std::fs::Dir::open_ambient_dir(
                        &relative,
                        cap_std::ambient_authority(),
                    )?;
                    Box::new(shwasi_wasi::sync::dir::Dir::from_cap_std(cap_std_dir))
                }
            };
            ctx.push_preopened_dir(dir, relative)?;
        }

        shwasi_wasi::sync::snapshots::preview_1::link(self.store_mut(), &mut ctx);

        Ok(())
    }

    /// Set the WASI stdout file descriptor.
    ///
    /// # Safety
    /// This function requires a valid file descriptor. It will **not** be taken ownership of.
    pub unsafe fn wasi_stderr(&mut self, fd: RawFileDescriptor) {
        self.wasi_stderr = fd;
    }

    /// Allow both read and write access to the given directory for WASI modules.
    pub fn allow_dir(&mut self, path: impl AsRef<Path>, location: Location) {
        // Make sure all paths allowed are stored internally as absolute paths
        // TODO: handle errors
        let abs_path = fs::canonicalize(path).unwrap();
        self.allowed.push(Allowed {
            path: abs_path,
            location,
        });
    }

    pub fn allow_env(&mut self, env_var: impl AsRef<str>) {
        self.env_vars.push(env_var.as_ref().to_owned());
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Location {
    Memory,
    Disk,
}

#[derive(Debug)]
struct Allowed {
    path: PathBuf,
    location: Location,
}

impl Hash for Allowed {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.path.hash(state);
    }
}

unsafe fn wasi_file(fd: RawFileDescriptor) -> NoDropWasiFile<File> {
    let file = cap_std::fs::File::from_raw_file_descriptor(fd);
    let wasi_file = File::from_cap_std(file);
    NoDropWasiFile::new(wasi_file)
}

/// A wrapper implementing `WasiFile` that prevents the inner file from being dropped.
struct NoDropWasiFile<T>(ManuallyDrop<T>);

impl<T> NoDropWasiFile<T> {
    fn new(inner: T) -> Self {
        Self(ManuallyDrop::new(inner))
    }
}

impl<T> WasiFile for NoDropWasiFile<T>
where
    T: WasiFile,
{
    fn as_any(&self) -> &dyn Any {
        self.0.as_any()
    }

    fn writable<'life0, 'async_trait>(
        &'life0 self,
    ) -> Pin<
        Box<
            dyn core::future::Future<Output = Result<(), shwasi_wasi::WasiError>>
                + core::marker::Send
                + 'async_trait,
        >,
    >
    where
        'life0: 'async_trait,
        Self: 'async_trait,
    {
        self.0.writable()
    }

    fn write_vectored<'a, 'life0, 'life1, 'async_trait>(
        &'life0 self,
        bufs: &'life1 [std::io::IoSlice<'a>],
    ) -> core::pin::Pin<
        Box<
            dyn core::future::Future<Output = Result<u64, shwasi_wasi::WasiError>>
                + core::marker::Send
                + 'async_trait,
        >,
    >
    where
        'a: 'async_trait,
        'life0: 'async_trait,
        'life1: 'async_trait,
        Self: 'async_trait,
    {
        self.0.write_vectored(bufs)
    }

    fn write_vectored_at<'a, 'life0, 'life1, 'async_trait>(
        &'life0 self,
        bufs: &'life1 [std::io::IoSlice<'a>],
        offset: u64,
    ) -> core::pin::Pin<
        Box<
            dyn core::future::Future<Output = Result<u64, shwasi_wasi::WasiError>>
                + core::marker::Send
                + 'async_trait,
        >,
    >
    where
        'a: 'async_trait,
        'life0: 'async_trait,
        'life1: 'async_trait,
        Self: 'async_trait,
    {
        self.0.write_vectored_at(bufs, offset)
    }

    fn set_filestat_size<'life0, 'async_trait>(
        &'life0 self,
        size: u64,
    ) -> core::pin::Pin<
        Box<
            dyn core::future::Future<Output = Result<(), shwasi_wasi::WasiError>>
                + core::marker::Send
                + 'async_trait,
        >,
    >
    where
        'life0: 'async_trait,
        Self: 'async_trait,
    {
        self.0.set_filestat_size(size)
    }

    fn readable<'life0, 'async_trait>(
        &'life0 self,
    ) -> core::pin::Pin<
        Box<
            dyn core::future::Future<Output = Result<(), shwasi_wasi::WasiError>>
                + core::marker::Send
                + 'async_trait,
        >,
    >
    where
        'life0: 'async_trait,
        Self: 'async_trait,
    {
        self.0.readable()
    }

    fn read_vectored<'a, 'life0, 'life1, 'async_trait>(
        &'life0 self,
        bufs: &'life1 mut [std::io::IoSliceMut<'a>],
    ) -> core::pin::Pin<
        Box<
            dyn core::future::Future<Output = Result<u64, shwasi_wasi::WasiError>>
                + core::marker::Send
                + 'async_trait,
        >,
    >
    where
        'a: 'async_trait,
        'life0: 'async_trait,
        'life1: 'async_trait,
        Self: 'async_trait,
    {
        self.0.read_vectored(bufs)
    }

    fn read_vectored_at<'a, 'life0, 'life1, 'async_trait>(
        &'life0 self,
        bufs: &'life1 mut [std::io::IoSliceMut<'a>],
        offset: u64,
    ) -> core::pin::Pin<
        Box<
            dyn core::future::Future<Output = Result<u64, shwasi_wasi::WasiError>>
                + core::marker::Send
                + 'async_trait,
        >,
    >
    where
        'a: 'async_trait,
        'life0: 'async_trait,
        'life1: 'async_trait,
        Self: 'async_trait,
    {
        self.0.read_vectored_at(bufs, offset)
    }

    fn datasync<'life0, 'async_trait>(
        &'life0 self,
    ) -> core::pin::Pin<
        Box<
            dyn core::future::Future<Output = Result<(), shwasi_wasi::WasiError>>
                + core::marker::Send
                + 'async_trait,
        >,
    >
    where
        'life0: 'async_trait,
        Self: 'async_trait,
    {
        self.0.datasync()
    }

    fn sync<'life0, 'async_trait>(
        &'life0 self,
    ) -> core::pin::Pin<
        Box<
            dyn core::future::Future<Output = Result<(), shwasi_wasi::WasiError>>
                + core::marker::Send
                + 'async_trait,
        >,
    >
    where
        'life0: 'async_trait,
        Self: 'async_trait,
    {
        self.0.sync()
    }

    fn get_filestat<'life0, 'async_trait>(
        &'life0 self,
    ) -> core::pin::Pin<
        Box<
            dyn core::future::Future<Output = Result<shwasi_wasi::Filestat, shwasi_wasi::WasiError>>
                + core::marker::Send
                + 'async_trait,
        >,
    >
    where
        'life0: 'async_trait,
        Self: 'async_trait,
    {
        self.0.get_filestat()
    }

    fn set_times<'life0, 'async_trait>(
        &'life0 self,
        atime: Option<shwasi_wasi::SystemTimeSpec>,
        mtime: Option<shwasi_wasi::SystemTimeSpec>,
    ) -> core::pin::Pin<
        Box<
            dyn core::future::Future<Output = Result<(), shwasi_wasi::WasiError>>
                + core::marker::Send
                + 'async_trait,
        >,
    >
    where
        'life0: 'async_trait,
        Self: 'async_trait,
    {
        self.0.set_times(atime, mtime)
    }

    fn set_fdflags<'life0, 'async_trait>(
        &'life0 mut self,
        flags: shwasi_wasi::FdFlags,
    ) -> core::pin::Pin<
        Box<
            dyn core::future::Future<Output = Result<(), shwasi_wasi::WasiError>>
                + core::marker::Send
                + 'async_trait,
        >,
    >
    where
        'life0: 'async_trait,
        Self: 'async_trait,
    {
        self.0.set_fdflags(flags)
    }

    fn get_fdflags<'life0, 'async_trait>(
        &'life0 self,
    ) -> core::pin::Pin<
        Box<
            dyn core::future::Future<Output = Result<shwasi_wasi::FdFlags, shwasi_wasi::WasiError>>
                + core::marker::Send
                + 'async_trait,
        >,
    >
    where
        'life0: 'async_trait,
        Self: 'async_trait,
    {
        self.0.get_fdflags()
    }

    fn advise<'life0, 'async_trait>(
        &'life0 self,
        offset: u64,
        len: u64,
        advice: shwasi_wasi::Advice,
    ) -> core::pin::Pin<
        Box<
            dyn core::future::Future<Output = Result<(), shwasi_wasi::WasiError>>
                + core::marker::Send
                + 'async_trait,
        >,
    >
    where
        'life0: 'async_trait,
        Self: 'async_trait,
    {
        self.0.advise(offset, len, advice)
    }

    fn seek<'life0, 'async_trait>(
        &'life0 self,
        pos: std::io::SeekFrom,
    ) -> core::pin::Pin<
        Box<
            dyn core::future::Future<Output = Result<u64, shwasi_wasi::WasiError>>
                + core::marker::Send
                + 'async_trait,
        >,
    >
    where
        'life0: 'async_trait,
        Self: 'async_trait,
    {
        self.0.seek(pos)
    }

    fn peek<'life0, 'life1, 'async_trait>(
        &'life0 self,
        buf: &'life1 mut [u8],
    ) -> core::pin::Pin<
        Box<
            dyn core::future::Future<Output = Result<u64, shwasi_wasi::WasiError>>
                + core::marker::Send
                + 'async_trait,
        >,
    >
    where
        'life0: 'async_trait,
        'life1: 'async_trait,
        Self: 'async_trait,
    {
        self.0.peek(buf)
    }

    fn num_ready_bytes(&self) -> Result<u64, shwasi_wasi::WasiError> {
        self.0.num_ready_bytes()
    }

    fn isatty(&self) -> bool {
        self.0.isatty()
    }

    fn get_filetype<'life0, 'async_trait>(
        &'life0 self,
    ) -> Pin<
        Box<
            dyn ::core::future::Future<
                    Output = Result<shwasi_wasi::FileType, shwasi_wasi::WasiError>,
                > + ::core::marker::Send
                + 'async_trait,
        >,
    >
    where
        'life0: 'async_trait,
        Self: 'async_trait,
    {
        self.0.get_filetype()
    }

    #[cfg(unix)]
    fn pollable(&self) -> Option<rustix::fd::BorrowedFd> {
        None
    }

    #[cfg(windows)]
    fn pollable(&self) -> Option<io_extras::os::windows::RawHandleOrSocket> {
        None
    }

    fn sock_accept<'life0, 'async_trait>(
        &'life0 self,
        fdflags: shwasi_wasi::FdFlags,
    ) -> core::pin::Pin<
        Box<
            dyn core::future::Future<Output = Result<Box<dyn WasiFile>, shwasi_wasi::WasiError>>
                + core::marker::Send
                + 'async_trait,
        >,
    >
    where
        'life0: 'async_trait,
        Self: 'async_trait,
    {
        self.0.sock_accept(fdflags)
    }

    fn sock_recv<'a, 'life0, 'life1, 'async_trait>(
        &'life0 self,
        ri_data: &'life1 mut [std::io::IoSliceMut<'a>],
        ri_flags: shwasi_wasi::RiFlags,
    ) -> core::pin::Pin<
        Box<
            dyn core::future::Future<
                    Output = Result<(u64, shwasi_wasi::RoFlags), shwasi_wasi::WasiError>,
                > + core::marker::Send
                + 'async_trait,
        >,
    >
    where
        'a: 'async_trait,
        'life0: 'async_trait,
        'life1: 'async_trait,
        Self: 'async_trait,
    {
        self.0.sock_recv(ri_data, ri_flags)
    }

    fn sock_send<'a, 'life0, 'life1, 'async_trait>(
        &'life0 self,
        si_data: &'life1 [std::io::IoSlice<'a>],
        si_flags: shwasi_wasi::SiFlags,
    ) -> core::pin::Pin<
        Box<
            dyn core::future::Future<Output = Result<u64, shwasi_wasi::WasiError>>
                + core::marker::Send
                + 'async_trait,
        >,
    >
    where
        'a: 'async_trait,
        'life0: 'async_trait,
        'life1: 'async_trait,
        Self: 'async_trait,
    {
        self.0.sock_send(si_data, si_flags)
    }

    fn sock_shutdown<'life0, 'async_trait>(
        &'life0 self,
        how: shwasi_wasi::SdFlags,
    ) -> core::pin::Pin<
        Box<
            dyn core::future::Future<Output = Result<(), shwasi_wasi::WasiError>>
                + core::marker::Send
                + 'async_trait,
        >,
    >
    where
        'life0: 'async_trait,
        Self: 'async_trait,
    {
        self.0.sock_shutdown(how)
    }
}
