use std::{borrow::Cow, io::Write, ops::Range};

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
#[error("{range:?}: {kind}")]
pub struct ParseError {
    pub range: Range<usize>,
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
    pub fn new(range: Range<usize>, kind: ParseErrorKind) -> Self {
        Self {
            range,
            kind,
            labels: vec![],
        }
    }

    pub fn new_with_labels(range: Range<usize>, kind: ParseErrorKind, labels: Vec<Label>) -> Self {
        Self {
            range,
            kind,
            labels,
        }
    }

    pub fn write_to(self, src: &str, name: &str, writer: impl Write) {
        use ariadne::{Color, Label, Report, ReportKind, Source};

        let a = Color::Blue;
        let b = Color::Green;
        let range = Range {
            start: self.range.start.min(src.len() - 1),
            end: self.range.end.min(src.len()),
        };

        Report::build(ReportKind::Error, name, range.start)
            .with_message(self.kind.to_string())
            .with_label(
                Label::new((name, range))
                    .with_message("error happened here")
                    .with_color(a),
            )
            .with_labels(self.labels.into_iter().map(|label| {
                Label::new((name, label.range))
                    .with_message(label.message)
                    .with_color(b)
            }))
            .finish()
            .write((name, Source::from(src)), writer)
            .unwrap();
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
