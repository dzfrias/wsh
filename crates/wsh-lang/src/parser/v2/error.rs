use std::{
    borrow::Cow,
    ops::{Bound, Range, RangeBounds},
};

use thiserror::Error;

/// A conveinience type for `Result<T, Error>`
pub type Result<T> = std::result::Result<T, Error>;

/// The error type, encompassing all parsing errors.
#[derive(Debug, Error)]
#[error("{msg}")]
pub struct Error {
    offset: usize,
    msg: Cow<'static, str>,
    labels: Vec<Label>,
}

impl Error {
    pub fn new(offset: usize, msg: impl Into<Cow<'static, str>>) -> Self {
        Self {
            offset,
            msg: msg.into(),
            labels: vec![],
        }
    }

    pub fn msg(&self) -> &str {
        &self.msg
    }

    pub fn labels(&self) -> &[Label] {
        &self.labels
    }

    pub fn offset(&self) -> usize {
        self.offset
    }
}

#[derive(Debug, PartialEq)]
pub struct Label {
    pub range: Range<usize>,
    pub msg: Cow<'static, str>,
}

impl Label {
    pub fn new(range: impl RangeBounds<usize>, msg: impl Into<Cow<'static, str>>) -> Self {
        let start = match range.start_bound() {
            Bound::Included(i) => *i,
            Bound::Excluded(i) => *i + 1,
            Bound::Unbounded => panic!("label should not be unbounded"),
        };
        let end = match range.end_bound() {
            Bound::Included(i) => *i + 1,
            Bound::Excluded(i) => *i,
            Bound::Unbounded => panic!("label should not be unbounded"),
        };
        Self {
            range: start..end,
            msg: msg.into(),
        }
    }
}

/// Extension trait to attach labels to Result types.
pub trait LabelAttach {
    fn attach(self, label: Label) -> Self;
}

impl<T> LabelAttach for Result<T> {
    fn attach(mut self, label: Label) -> Self {
        let Err(ref mut err) = self else {
            return self;
        };
        err.labels.push(label);
        self
    }
}
