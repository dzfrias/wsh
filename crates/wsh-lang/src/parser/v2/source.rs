use std::{borrow::Cow, io};

use crate::v2::error::Error;

/// The original text of wsh's input, along with a source name.
#[derive(Debug)]
pub struct Source {
    contents: String,
    name: Cow<'static, str>,
}

impl Source {
    /// Create a new source with the given name and text.
    pub fn new(name: impl Into<Cow<'static, str>>, contents: String) -> Self {
        Self {
            name: name.into(),
            contents,
        }
    }

    /// Get the underlying contents of the source.
    pub fn contents(&self) -> &str {
        &self.contents
    }

    /// Get the name of the source.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Format an error on this source. This will write the error message to the `writer`.
    pub fn fmt_error(&self, error: Error, writer: impl io::Write) -> io::Result<()> {
        use ariadne::{Color, Label, Report, ReportKind, Source};

        Report::build(ReportKind::Error, self.name(), error.offset())
            .with_message(error.msg())
            .with_labels(error.labels().iter().map(|label| {
                Label::new((self.name(), label.range.clone()))
                    .with_message(&label.msg)
                    .with_color(Color::Blue)
            }))
            .finish()
            .write((self.name(), Source::from(self.contents())), writer)
    }
}
