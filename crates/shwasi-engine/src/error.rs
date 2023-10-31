use thiserror::Error;

use crate::store::{Extern, ExternVal};

/// A error that can occur during instantiation or at runtime.
#[derive(Debug, Error)]
pub enum Error {
    // Instantiation errors
    #[error("invalid number of externs: want {want}, got {got}")]
    InvalidExternLength { want: usize, got: usize },
    #[error("extern not found: {0}")]
    ExternNotFound(ExternVal),
    #[error("bad extern type: want {want}, got {got}")]
    BadExternType { want: Extern, got: Extern },
    #[error("validation error: {0}")]
    Validation(anyhow::Error),
}

/// A convenience type alias for `Result<T, Error>`.
pub type Result<T> = std::result::Result<T, Error>;
