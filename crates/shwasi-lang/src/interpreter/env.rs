use std::{any::Any, collections::HashMap, mem::ManuallyDrop, path::Path, pin::Pin};

use filedescriptor::{FromRawFileDescriptor, RawFileDescriptor};
use shwasi_engine::{Instance, Store, WasmFuncUntyped};
use shwasi_wasi::{
    sync::{dir::Dir, file::File},
    WasiCtx, WasiError, WasiFile,
};
use smol_str::SmolStr;

use crate::{ast::Pipeline, interpreter::value::Value, Ident};

pub struct Env {
    env: HashMap<Ident, Value>,
    aliases: HashMap<SmolStr, Pipeline>,
    wasi_ctx: WasiCtx,
    store: Store,
    modules: Vec<Instance>,
}

impl Env {
    pub fn new() -> Self {
        let mut store = Store::default();
        let mut wasi_ctx = shwasi_wasi::WasiCtxBuilder::new().build();
        // Link the WASI preview 1 snapshot into the store, so we can use it in our modules.
        shwasi_wasi::sync::snapshots::preview_1::link(&mut store, &mut wasi_ctx);
        Self {
            env: HashMap::new(),
            aliases: HashMap::new(),
            modules: Vec::new(),
            wasi_ctx,
            store,
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
        shwasi_wasi::sync::snapshots::preview_1::link(&mut self.store, &mut self.wasi_ctx);
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
        let file = cap_std::fs::File::from_raw_file_descriptor(fd);
        let wasi_file = File::from_cap_std(file);
        let no_drop = NoDropWasiFile::new(wasi_file);
        self.wasi_ctx.set_stdout(Box::new(no_drop));
    }

    /// Set the WASI stdout file descriptor.
    ///
    /// # Safety
    /// This function requires a valid file descriptor. It will **not** be taken ownership of.
    pub unsafe fn wasi_stderr(&mut self, fd: RawFileDescriptor) {
        let file = cap_std::fs::File::from_raw_file_descriptor(fd);
        let wasi_file = File::from_cap_std(file);
        let no_drop = NoDropWasiFile::new(wasi_file);
        self.wasi_ctx.set_stderr(Box::new(no_drop));
    }

    /// Allow both read and write access to the given directory for WASI modules.
    pub fn allow_dir(&mut self, path: impl AsRef<Path>) -> Result<(), WasiError> {
        let owned_path = path.as_ref().to_owned();
        let dir = cap_std::fs::Dir::open_ambient_dir(&owned_path, cap_std::ambient_authority())?;
        let wasi_dir = Dir::from_cap_std(dir);
        self.wasi_ctx.push_dir(Box::new(wasi_dir), owned_path)?;
        Ok(())
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
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
