use std::io;

use thiserror::Error;

use crate::{
    shell_v2::value::ValueType,
    v2::{
        ast::{BinopKind, Ident, UnopKind},
        Source, SourceError,
    },
};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Error)]
#[error("{kind}")]
pub struct Error {
    kind: ErrorKind,
    offset: usize,
}

impl Error {
    pub fn new(kind: ErrorKind, offset: usize) -> Self {
        Self { kind, offset }
    }

    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    pub fn offset(&self) -> usize {
        self.offset
    }
}

impl SourceError for Error {
    fn fmt_on(&self, source: &Source, mut writer: impl io::Write) -> io::Result<()> {
        if let ErrorKind::ParseError(parse_error) = self.kind() {
            return parse_error.fmt_on(source, writer);
        }
        writeln!(writer, "wsh: {}", self.kind())
    }
}

#[derive(Debug, Error)]
pub enum ErrorKind {
    #[error("type error: invalid `{op}` between `{lhs}` and `{rhs}`")]
    BinopTypeError {
        op: BinopKind,
        lhs: ValueType,
        rhs: ValueType,
    },
    #[error("type error: invalid `{op}` on `{type_}`")]
    UnopTypeError { op: UnopKind, type_: ValueType },
    #[error("parse error: {0}")]
    ParseError(crate::v2::error::Error),
    #[error("home directory not found")]
    HomeDirNotFound,
    #[error("home dir contains invalid utf-8")]
    HomeDirInvalidUTF8,
    #[error("command failed to start: {0}")]
    CommandFailedToStart(io::Error),
    #[error("command failed: {0}")]
    CommandFailed(io::Error),
    #[error("bad redirect: {0}")]
    BadRedirect(io::Error),
    #[error("unbound variable: `{0}`")]
    UnboundVariable(Ident),
    #[error("capture error: {0}")]
    CaptureError(io::Error),
    #[error("Wasm function arg mismatch: expected {want} args, got {got} args")]
    WasmArgLenMismatch { want: usize, got: usize },
    #[error("bad Wasm argument {idx}: {reason}")]
    BadWasmArg { idx: usize, reason: &'static str },
}

pub trait WithPosition<T> {
    fn with_position(self, pos: usize) -> Result<T>;
}

impl<T> WithPosition<T> for std::result::Result<T, ErrorKind> {
    fn with_position(self, pos: usize) -> Result<T> {
        self.map_err(|err| Error {
            kind: err,
            offset: pos,
        })
    }
}
