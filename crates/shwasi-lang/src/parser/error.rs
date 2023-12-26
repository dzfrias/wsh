use thiserror::Error;

use crate::parser::lexer::Token;

#[derive(Debug, Error, PartialEq)]
pub enum ErrorKind {
    #[error("unexpected token: {token}, want {expected}")]
    UnexpectedToken {
        token: Token,
        expected: &'static str,
    },
}

#[derive(Debug, Error, PartialEq)]
#[error("{offset}: {kind}")]
pub struct ParseError {
    pub offset: usize,
    pub kind: ErrorKind,
}

impl ParseError {
    pub fn new(offset: usize, kind: ErrorKind) -> Self {
        Self { offset, kind }
    }
}

pub type ParseResult<T> = std::result::Result<T, ParseError>;
