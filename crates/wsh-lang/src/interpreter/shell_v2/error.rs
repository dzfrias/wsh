use std::io;

use ariadne::Color;
use itertools::Itertools;
use thiserror::Error;

use crate::{
    shell_v2::value::ValueType,
    v2::{
        ast::{BinopKind, Ident, UnopKind},
        Source, SourceError,
    },
};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Error)]
#[error("{kind}")]
pub struct Error {
    kind: ErrorKind,
    offset: usize,
}

impl Error {
    pub fn new(kind: ErrorKind, offset: usize) -> Self {
        Self { kind, offset }
    }

    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    pub fn offset(&self) -> usize {
        self.offset
    }
}

impl SourceError for Error {
    fn fmt_on(&self, source: &Source, mut writer: impl io::Write, color: bool) -> io::Result<()> {
        if let ErrorKind::ParseError(parse_error) = self.kind() {
            return parse_error.fmt_on(source, writer, color);
        }
        let lines = source.contents().lines().collect_vec();
        let linenr = {
            let mut current = 0;
            lines
                .iter()
                .enumerate()
                .find_map(|(i, line)| {
                    current += line.len() + 1;
                    if current > self.offset() {
                        Some(i)
                    } else {
                        None
                    }
                })
                .expect("offset should be in source")
        };
        if color {
            writeln!(
                writer,
                "{}: {}",
                Color::Red.paint("wsh error").bold(),
                Color::White.paint(self.kind()).bold(),
            )?;
        } else {
            writeln!(writer, "wsh error: {}", self.kind())?;
        }
        if lines.len() > 1 {
            if color {
                writeln!(
                    writer,
                    "{} {}",
                    Color::Blue.paint(format_args!("{}|", linenr + 1)).bold(),
                    lines[linenr],
                )?;
            } else {
                writeln!(writer, "{}| {}", linenr + 1, lines[linenr])?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Error)]
pub enum ErrorKind {
    #[error("type error: invalid `{op}` between `{lhs}` and `{rhs}`")]
    BinopTypeError {
        op: BinopKind,
        lhs: ValueType,
        rhs: ValueType,
    },
    #[error("type error: invalid `{op}` on `{type_}`")]
    UnopTypeError { op: UnopKind, type_: ValueType },
    #[error("parse error: {0}")]
    ParseError(crate::v2::error::Error),
    #[error("home directory not found")]
    HomeDirNotFound,
    #[error("home dir contains invalid utf-8")]
    HomeDirInvalidUTF8,
    #[error("command failed to start: {0}")]
    CommandFailedToStart(io::Error),
    #[error("command not found")]
    CommandNotFound,
    #[error("command failed: {0}")]
    CommandFailed(io::Error),
    #[error("bad redirect: {0}")]
    BadRedirect(io::Error),
    #[error("unbound variable: `{0}`")]
    UnboundVariable(Ident),
    #[error("capture error: {0}")]
    CaptureError(io::Error),
    #[error("Wasm function arg mismatch: expected {want} args, got {got} args")]
    WasmArgLenMismatch { want: usize, got: usize },
    #[error("bad Wasm argument {idx}: {reason}")]
    BadWasmArg { idx: usize, reason: &'static str },
    #[error("error opening memfile: {0}")]
    MemFileError(io::Error),
}

pub trait WithPosition<T> {
    fn with_position(self, pos: usize) -> Result<T>;
}

impl<T> WithPosition<T> for std::result::Result<T, ErrorKind> {
    fn with_position(self, pos: usize) -> Result<T> {
        self.map_err(|err| Error {
            kind: err,
            offset: pos,
        })
    }
}
