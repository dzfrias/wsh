use std::{
    any::Any,
    collections::HashMap,
    io,
    mem::ManuallyDrop,
    path::{Path, PathBuf},
    pin::Pin,
};

use filedescriptor::{AsRawFileDescriptor, FromRawFileDescriptor};
use wsh_engine::Store;
use wsh_wasi::{cap_std, WasiCtxBuilder, WasiFile};

use crate::{
    shell_v2::value::Value,
    v2::ast::{Ast, Ident},
};

pub struct Env {
    vars: HashMap<Ident, Value>,
    aliases: HashMap<String, Ast>,

    store: Store,
    allowed_dirs: Vec<PathBuf>,
    allowed_envs: Vec<String>,
}

impl Env {
    pub fn new() -> Self {
        let mut store = Store::default();
        let mut wasi_ctx = WasiCtxBuilder::new().build();
        // Link the WASI preview 1 snapshot into the store. This is actually just a stand-in. This
        // is only here so that module instantiation doesn't fail. All host functions linked by
        // this function call are replaced every time `prepare_wasi` is called. This is because
        // WasCtx is not designed to work over multiple WASM module's lifetimes, so there are **a
        // lot** of problems when trying to maintain a global refernce counted instance of WasiCtx.
        // Perhaps if a new WASI implementation is designed from scratch, this wouldn't be
        // necessary.
        wsh_wasi::sync::snapshots::preview_1::link(&mut store, &mut wasi_ctx);
        Self {
            vars: HashMap::new(),
            aliases: HashMap::new(),
            store: Store::default(),
            allowed_dirs: vec![],
            allowed_envs: vec![],
        }
    }

    pub fn get_alias(&mut self, ident: &str) -> Option<Ast> {
        self.aliases.remove(ident)
    }

    pub fn set_alias(&mut self, ident: String, ast: Ast) {
        self.aliases.insert(ident, ast);
    }

    pub fn get_var(&self, ident: &Ident) -> Option<&Value> {
        self.vars.get(ident)
    }

    pub fn set_var(&mut self, ident: Ident, value: Value) {
        self.vars.insert(ident, value);
    }

    pub fn prepare_wasi(&mut self, ctx: WasiContext) -> Result<(), wsh_wasi::WasiError> {
        let mut builder = WasiCtxBuilder::new();
        if let Some(stdin) = ctx.stdin {
            builder.stdin(Box::new(to_wasi_file(stdin)));
        }
        let mut ctx = builder
            .stdout(Box::new(to_wasi_file(ctx.stdout)))
            .stderr(Box::new(to_wasi_file(ctx.stderr)))
            .args(ctx.args)
            .expect("should not overflow on args")
            .envs(ctx.env)
            .expect("should not overflow on environment vars")
            .build();

        for env_var in &self.allowed_envs {
            let Ok(value) = std::env::var(env_var) else {
                continue;
            };
            ctx.push_env(env_var, &value)
                .expect("should not overflow on env vars");
        }

        for path in &self.allowed_dirs {
            let dir = wasi_dir(path)?;
            ctx.push_preopened_dir(dir, path)?;
            let Ok(current_dir) = std::env::current_dir() else {
                continue;
            };
            let Some(mut relative) = pathdiff::diff_paths(path, current_dir) else {
                continue;
            };
            if relative == Path::new("") {
                relative = PathBuf::from(".");
            }
            let dir = wasi_dir(&relative)?;
            ctx.push_preopened_dir(dir, relative)?;
        }

        wsh_wasi::sync::snapshots::preview_1::link(self.store_mut(), &mut ctx);
        Ok(())
    }

    pub fn allow_dir(&mut self, p: impl AsRef<Path>) -> io::Result<()> {
        let abs_path = std::fs::canonicalize(p.as_ref())?;
        self.allowed_dirs.push(abs_path);
        Ok(())
    }

    pub fn allow_env(&mut self, env: String) {
        self.allowed_envs.push(env);
    }

    pub fn store(&self) -> &Store {
        &self.store
    }

    pub fn store_mut(&mut self) -> &mut Store {
        &mut self.store
    }
}

#[derive(Debug)]
pub struct WasiContext<'a> {
    stdout: &'a std::fs::File,
    stderr: &'a std::fs::File,
    stdin: Option<&'a std::fs::File>,
    args: &'a [String],
    env: &'a [(String, String)],
}

impl<'a> WasiContext<'a> {
    pub fn new(
        stdout: &'a std::fs::File,
        stderr: &'a std::fs::File,
        stdin: Option<&'a std::fs::File>,
    ) -> Self {
        Self {
            stdout,
            stderr,
            stdin,
            args: &[],
            env: &[],
        }
    }

    pub fn args(mut self, args: &'a [String]) -> Self {
        self.args = args;
        self
    }

    pub fn env(mut self, env: &'a [(String, String)]) -> Self {
        self.env = env;
        self
    }
}

fn to_wasi_file(file: &std::fs::File) -> NoDropWasiFile<wsh_wasi::sync::file::File> {
    // SAFETY: this is safe because we have a borrow of std::fs::File. We cap_std's File **will**
    // close the inner file descriptor on drop, but NoDropWasiFile prevents it from being closed.
    // This allows us to recreate the semantics of a borrwed file descriptor, while using an owned
    // API.
    let file =
        unsafe { cap_std::fs::File::from_raw_file_descriptor(file.as_raw_file_descriptor()) };
    NoDropWasiFile::new(wsh_wasi::sync::file::File::from_cap_std(file))
}

fn wasi_dir(path: impl AsRef<Path>) -> Result<Box<wsh_wasi::sync::dir::Dir>, wsh_wasi::WasiError> {
    let cap_std_dir = cap_std::fs::Dir::open_ambient_dir(path, cap_std::ambient_authority())?;
    let dir = Box::new(wsh_wasi::sync::dir::Dir::from_cap_std(cap_std_dir));
    Ok(dir)
}

/// A wrapper implementing `WasiFile` that prevents the inner file from being dropped.
struct NoDropWasiFile<T>(ManuallyDrop<T>)
where
    T: WasiFile;

impl<T> NoDropWasiFile<T>
where
    T: WasiFile,
{
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
            dyn core::future::Future<Output = Result<(), wsh_wasi::WasiError>>
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
            dyn core::future::Future<Output = Result<u64, wsh_wasi::WasiError>>
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
            dyn core::future::Future<Output = Result<u64, wsh_wasi::WasiError>>
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
            dyn core::future::Future<Output = Result<(), wsh_wasi::WasiError>>
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
            dyn core::future::Future<Output = Result<(), wsh_wasi::WasiError>>
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
            dyn core::future::Future<Output = Result<u64, wsh_wasi::WasiError>>
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
            dyn core::future::Future<Output = Result<u64, wsh_wasi::WasiError>>
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
            dyn core::future::Future<Output = Result<(), wsh_wasi::WasiError>>
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
            dyn core::future::Future<Output = Result<(), wsh_wasi::WasiError>>
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
            dyn core::future::Future<Output = Result<wsh_wasi::Filestat, wsh_wasi::WasiError>>
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
        atime: Option<wsh_wasi::SystemTimeSpec>,
        mtime: Option<wsh_wasi::SystemTimeSpec>,
    ) -> core::pin::Pin<
        Box<
            dyn core::future::Future<Output = Result<(), wsh_wasi::WasiError>>
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
        flags: wsh_wasi::FdFlags,
    ) -> core::pin::Pin<
        Box<
            dyn core::future::Future<Output = Result<(), wsh_wasi::WasiError>>
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
            dyn core::future::Future<Output = Result<wsh_wasi::FdFlags, wsh_wasi::WasiError>>
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
        advice: wsh_wasi::Advice,
    ) -> core::pin::Pin<
        Box<
            dyn core::future::Future<Output = Result<(), wsh_wasi::WasiError>>
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
            dyn core::future::Future<Output = Result<u64, wsh_wasi::WasiError>>
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
            dyn core::future::Future<Output = Result<u64, wsh_wasi::WasiError>>
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

    fn num_ready_bytes(&self) -> Result<u64, wsh_wasi::WasiError> {
        self.0.num_ready_bytes()
    }

    fn isatty(&self) -> bool {
        self.0.isatty()
    }

    fn get_filetype<'life0, 'async_trait>(
        &'life0 self,
    ) -> Pin<
        Box<
            dyn ::core::future::Future<Output = Result<wsh_wasi::FileType, wsh_wasi::WasiError>>
                + ::core::marker::Send
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
        fdflags: wsh_wasi::FdFlags,
    ) -> core::pin::Pin<
        Box<
            dyn core::future::Future<Output = Result<Box<dyn WasiFile>, wsh_wasi::WasiError>>
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
        ri_flags: wsh_wasi::RiFlags,
    ) -> core::pin::Pin<
        Box<
            dyn core::future::Future<Output = Result<(u64, wsh_wasi::RoFlags), wsh_wasi::WasiError>>
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
        self.0.sock_recv(ri_data, ri_flags)
    }

    fn sock_send<'a, 'life0, 'life1, 'async_trait>(
        &'life0 self,
        si_data: &'life1 [std::io::IoSlice<'a>],
        si_flags: wsh_wasi::SiFlags,
    ) -> core::pin::Pin<
        Box<
            dyn core::future::Future<Output = Result<u64, wsh_wasi::WasiError>>
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
        how: wsh_wasi::SdFlags,
    ) -> core::pin::Pin<
        Box<
            dyn core::future::Future<Output = Result<(), wsh_wasi::WasiError>>
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
