use std::io;

use thiserror::Error;

use crate::{
    shell_v2::value::ValueType,
    v2::ast::{BinopKind, UnopKind},
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
