use std::io;

use thiserror::Error;

#[derive(Debug, Error)]
#[error("shwasi: ")]
pub enum RuntimeError {
    #[error("command failed: {0}")]
    CommandFailed(io::Error),
}

pub type RuntimeResult<T> = std::result::Result<T, RuntimeError>;
