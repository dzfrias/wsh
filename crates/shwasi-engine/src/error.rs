use thiserror::Error;

use crate::store::{Extern, ExternVal};

/// A error that can occur during instantiation or at runtime.
#[derive(Debug, Error)]
pub enum Error {
    /// The provided externs do not match the imports (instantiation).
    #[error("invalid number of externs: want {want}, got {got}")]
    InvalidExternLength { want: usize, got: usize },
    /// A provided extern was not found in the store (instantiation).
    #[error("extern not found: {0}")]
    ExternNotFound(ExternVal),
    /// The provided extern's type does not match the import's type (instantiation).
    #[error("bad extern type: want {want}, got {got}")]
    BadExternType { want: Extern, got: Extern },
}

/// A convenience type alias for `Result<T, Error>`.
pub type Result<T> = std::result::Result<T, Error>;
