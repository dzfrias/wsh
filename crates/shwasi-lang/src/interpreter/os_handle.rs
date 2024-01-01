#[cfg(any(unix, target_os = "wasi"))]
use std::os::fd::{FromRawFd, IntoRawFd, RawFd};
#[cfg(any(unix, target_os = "wasi"))]
pub trait OsHandle: IntoRawFd + FromRawFd + Sized {
    #[inline]
    fn into_os_handle(self) -> RawFd {
        self.into_raw_fd()
    }
    #[inline]
    unsafe fn from_os_handle(fd: RawFd) -> Self {
        Self::from_raw_fd(fd)
    }
}
#[cfg(any(unix, target_os = "wasi"))]
impl<T> OsHandle for T where T: IntoRawFd + FromRawFd {}

#[cfg(windows)]
use std::os::windows::io::{FromRawHandle, IntoRawHandle, RawHandle};
#[cfg(windows)]
pub trait OsHandle: IntoRawHandle + FromRawHandle + Sized {
    #[inline]
    fn into_os_handle(self) -> RawHandle {
        self.into_raw_handle()
    }
    #[inline]
    unsafe fn from_os_handle(handle: RawHandle) -> Self {
        Self::from_raw_handle(handle)
    }
}
#[cfg(windows)]
impl<T> OsHandle for T where T: IntoRawHandle + FromRawHandle {}
