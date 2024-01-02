use std::io;

use thiserror::Error;

use crate::{
    ast::{InfixOp, PrefixOp},
    Ident, ParseError, Type,
};

#[derive(Debug, Error)]
pub enum ShellError {
    #[error("command failed: {0}")]
    CommandFailed(io::Error),
    #[error("command not found")]
    CommandNotFound,
    #[error("pipe error: {0}")]
    PipeError(io::Error),
    #[error("type error: `{lhs}` `{op}` `{rhs}`")]
    TypeErrorInfix { lhs: Type, rhs: Type, op: InfixOp },
    #[error("type error: `{op}` `{expr}`")]
    TypeErrorPrefix { expr: Type, op: PrefixOp },
    #[error("unbound: `{0}`")]
    Unbound(Ident),
    #[error("parse error: {0}")]
    ParseError(ParseError),
    #[error("error duplicating fd {0}: {1}")]
    FdDupError(i64, io::Error),
}

impl ShellError {
    pub fn from_command_error(err: io::Error) -> Self {
        if err.kind() == io::ErrorKind::NotFound {
            Self::CommandNotFound
        } else {
            Self::CommandFailed(err)
        }
    }
}

pub type ShellResult<T> = std::result::Result<T, ShellError>;
