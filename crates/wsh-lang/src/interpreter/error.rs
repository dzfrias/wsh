use std::{io, path::PathBuf};

use smol_str::SmolStr;
use thiserror::Error;

use crate::{
    ast::{InfixOp, PrefixOp},
    Ident, Type,
};

#[derive(Debug, Error)]
pub enum ShellError {
    #[error("command failed: {0}")]
    CommandFailed(io::Error),
    #[error("command not found: {0}")]
    CommandNotFound(SmolStr),
    #[error("pipe error: {0}")]
    PipeError(io::Error),
    #[error("type error: `{lhs}` `{op}` `{rhs}`")]
    TypeErrorInfix { lhs: Type, rhs: Type, op: InfixOp },
    #[error("type error: `{op}` `{expr}`")]
    TypeErrorPrefix { expr: Type, op: PrefixOp },
    #[error("unbound: `{0}`")]
    Unbound(Ident),
    #[error("the shell encountered a parse error")]
    ParseError,
    #[error("error duplicating fd: {0}")]
    DupError(io::Error),
    #[error("error writing to fd in builtin: {0}")]
    BuiltinWriteError(io::Error),
    #[error("error opening fd for redirection: {0}")]
    RedirectError(io::Error),
    #[error("at files are currently not supported on non-unix platforms!")]
    AtFilesNotSupported,
    #[error("file not found in memfs: {0}")]
    MemfsNotFound(PathBuf),
}

pub type ShellResult<T> = std::result::Result<T, ()>;
