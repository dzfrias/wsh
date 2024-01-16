use std::io;

use shwasi_wasi::{Errno, WasiError};
use thiserror::Error;

/// An error type representing failures in the in-memory file system.
#[derive(Debug, Clone, Copy, Error, PartialEq, Eq)]
pub enum MemFsError {
    #[error("is directory")]
    IsDir,
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
    #[error("directory not empty")]
    NotEmpty,
}

impl From<MemFsError> for WasiError {
    fn from(value: MemFsError) -> Self {
        let errno = match value {
            MemFsError::IsDir => Errno::Isdir,
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
            MemFsError::NotEmpty => Errno::Notempty,
        };
        WasiError::from(errno)
    }
}

impl From<io::Error> for MemFsError {
    fn from(err: io::Error) -> Self {
        match err.kind() {
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
