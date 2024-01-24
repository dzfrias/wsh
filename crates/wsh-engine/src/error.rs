use thiserror::Error;
use wsh_parser::ValType;

use crate::store::{Extern, ExternType};
pub use crate::vm::Trap;

#[derive(Debug, Error)]
#[error(transparent)]
pub struct Error {
    inner: Box<ErrorKind>,
}

impl Error {
    pub fn kind(&self) -> &ErrorKind {
        &self.inner
    }
}

impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Self {
        Self {
            inner: Box::new(kind),
        }
    }
}

impl From<Trap> for Error {
    fn from(trap: Trap) -> Self {
        Self {
            inner: Box::new(ErrorKind::Trap(trap)),
        }
    }
}

/// A error that can occur during instantiation or at runtime.
#[derive(Debug, Error)]
pub enum ErrorKind {
    // Instantiation errors
    #[error("extern not found: \"{module}\" \"{field}\"")]
    ExternNotFound { module: String, field: String },
    #[error("bad extern type: want {want}, got {got}")]
    BadExtern { want: Extern, got: Extern },
    #[error("validation error: {0}")]
    ValidationError(anyhow::Error),

    // Runtime errors
    #[error("trap: {0}")]
    Trap(#[from] Trap),
    #[error("function not found \"{0}\"")]
    FunctionNotFound(String),
    #[error("attempting to call non-function: \"{0}\"")]
    AttemptingToCallNonFunction(ExternType),
    #[error("attempting to call function with wrong args: want {want:?}, got {got:?}")]
    FunctionArgsMismatch {
        want: Vec<ValType>,
        got: Vec<ValType>,
    },

    #[error("error: {0}")]
    Custom(anyhow::Error),
}

/// A convenience type alias for `Result<T, Error>`.
pub type Result<T> = std::result::Result<T, Error>;
