use std::{borrow::Cow, ops::Range};

use thiserror::Error;

use crate::parser::lexer::Token;

pub type ParseResult<T> = std::result::Result<T, ParseError>;

#[derive(Debug, Error, PartialEq)]
pub enum ParseErrorKind {
    #[error("unexpected token: {token}, {expected}")]
    UnexpectedToken {
        token: Token,
        expected: &'static str,
    },
    #[error("unfinished pipeline")]
    UnfinishedPipeline,
}

#[derive(Debug, Error, PartialEq)]
#[error("{offset}: {kind}")]
pub struct ParseError {
    pub offset: usize,
    pub kind: ParseErrorKind,
    pub labels: Vec<Label>,
}

#[derive(Debug, PartialEq)]
pub struct Label {
    pub range: Range<usize>,
    pub message: Cow<'static, str>,
}

pub trait LabelAttach {
    fn attach(self, label: Label) -> Self;
}

impl<T> LabelAttach for Result<T, ParseError> {
    fn attach(mut self, label: Label) -> Self {
        let Err(ref mut err) = self else {
            return self;
        };
        err.labels.push(label);
        self
    }
}

impl ParseError {
    pub fn new(offset: usize, kind: ParseErrorKind) -> Self {
        Self {
            offset,
            kind,
            labels: vec![],
        }
    }

    pub fn new_with_labels(offset: usize, kind: ParseErrorKind, labels: Vec<Label>) -> Self {
        Self {
            offset,
            kind,
            labels,
        }
    }
}

impl Label {
    pub fn new(range: Range<usize>, message: impl Into<Cow<'static, str>>) -> Self {
        Self {
            range,
            message: message.into(),
        }
    }
}
