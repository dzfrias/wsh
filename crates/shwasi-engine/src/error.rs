use shwasi_parser::ValType;
use thiserror::Error;

use crate::{
    store::{Extern, ExternVal},
    vm::Trap,
};

/// A error that can occur during instantiation or at runtime.
#[derive(Debug, Error)]
pub enum Error {
    // Instantiation errors
    #[error("invalid number of externs: want {want}, got {got}")]
    InvalidExternLength { want: usize, got: usize },
    #[error("extern not found: {module}.{field}")]
    ExternNotFound { module: String, field: String },
    #[error("bad extern type: want {want}, got {got}")]
    BadExternType { want: Extern, got: Extern },
    #[error("validation error: {0}")]
    Validation(anyhow::Error),

    // Runtime errors
    #[error("trap: {0}")]
    Trap(Trap),
    #[error("function not found {0}")]
    FunctionNotFound(String),
    #[error("attempting to call non-function ({0:?})")]
    AttemptingToCallNonFunction(ExternVal),
    #[error("attempting to call function with wrong args: want {want:?}, got {got:?}")]
    FunctionArgsMismatch {
        want: Vec<ValType>,
        got: Vec<ValType>,
    },
}

/// A convenience type alias for `Result<T, Error>`.
pub type Result<T> = std::result::Result<T, Error>;
