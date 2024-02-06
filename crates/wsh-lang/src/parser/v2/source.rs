use std::io;

/// The original text of wsh's input, along with a source name.
#[derive(Debug)]
pub struct Source<'a> {
    contents: String,
    name: &'a str,
}

impl<'a> Source<'a> {
    /// Create a new source with the given name and text.
    pub fn new(name: &'a str, contents: String) -> Self {
        Self { name, contents }
    }

    /// Get the underlying contents of the source.
    pub fn contents(&self) -> &str {
        &self.contents
    }

    /// Get the name of the source.
    pub fn name(&self) -> &'a str {
        self.name
    }
}

pub trait SourceError {
    fn fmt_on(&self, source: &Source, writer: impl io::Write) -> io::Result<()>;
}
