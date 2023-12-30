use std::io;

use thiserror::Error;

use crate::{
    ast::{InfixOp, PrefixOp},
    Ident, Type,
};

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error("command failed: {0}")]
    CommandFailed(io::Error),
    #[error("type error: `{lhs}` `{op}` `{rhs}`")]
    TypeErrorInfix { lhs: Type, rhs: Type, op: InfixOp },
    #[error("type error: `{op}` `{expr}`")]
    TypeErrorPrefix { expr: Type, op: PrefixOp },
    #[error("unbound: `{0}`")]
    Unbound(Ident),
}

pub type RuntimeResult<T> = std::result::Result<T, RuntimeError>;
