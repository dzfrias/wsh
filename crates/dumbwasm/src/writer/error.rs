use std::{borrow::Cow, ops::Range};

use thiserror::Error;

use crate::lexer::Token;

#[derive(Debug, Error)]
pub enum WriteErrorKind {
    #[error("unexpected token: `{0:?}`")]
    UnexpectedToken(Token),
    #[error("expected token: `{0:?}`")]
    ExpectedToken(Token),
    #[error("unknown keyword: `{0}`")]
    UnknownKeyword(String),
    #[error("invalid token")]
    InvalidToken,
    #[error("unclosed array")]
    UnclosedArray,
    #[error("unclosed section")]
    UnclosedSection,
    #[error("unclosed instrs")]
    UnclosedInstrs,
}

#[derive(Debug, Error)]
#[error("writer error")]
pub struct WriteError {
    #[source]
    pub kind: WriteErrorKind,
    pub span: Range<usize>,
    pub help: Option<Cow<'static, str>>,
}
