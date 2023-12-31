use smol_str::SmolStr;
use std::{fmt, ops::Deref};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Ident(Ident),
    Number(f64),
    String(SmolStr),
    QuotedString(SmolStr),
    UnquotedString(SmolStr),
    BoolTrue,
    BoolFalse,
    Backtick,
    Assign,
    Pipe,
    Write,
    Append,
    Newline,
    QuestionMark,
    Dollar,

    LParen,
    RParen,

    Plus,
    Minus,
    Star,
    Slash,
    Eq,
    Ne,
    Gt,
    Lt,
    Le,
    Ge,
    Bang,

    If,
    Then,
    Else,
    End,
    Alias,
    Export,

    Invalid(char),
    Eof,
}

#[derive(Debug, Default)]
pub struct TokenBuffer {
    infos: Vec<TokenInfo>,
    offset: usize,
    str_table: Vec<SmolStr>,
    number_table: Vec<f64>,
}

#[derive(Debug, PartialEq)]
struct TokenInfo {
    pub kind: TokenKind,
    pub offset: usize,
    pub payload: u32,
}

impl TokenBuffer {
    pub fn new() -> Self {
        Self {
            offset: 0,
            infos: Vec::new(),
            str_table: Vec::new(),
            number_table: Vec::new(),
        }
    }

    pub fn push(&mut self, tok: Token) {
        let kind = tok.kind();
        let size = tok.size();
        let payload = match tok {
            Token::Ident(ident) => {
                let offset = self.str_table.len();
                self.str_table.push(ident.0);
                offset as u32
            }
            Token::Number(n) => {
                self.number_table.push(n);
                (self.number_table.len() - 1) as u32
            }
            Token::String(s) | Token::UnquotedString(s) | Token::QuotedString(s) => {
                let offset = self.str_table.len();
                self.str_table.push(s);
                offset as u32
            }
            Token::Invalid(c) => c as u32,
            _ => 0,
        };
        self.infos.push(TokenInfo {
            kind,
            offset: self.offset,
            payload,
        });
        self.offset += size;
    }

    pub fn get(&self, i: usize) -> Option<Token> {
        let TokenInfo {
            kind,
            offset: _,
            payload,
        } = self.get_info(i)?;

        let t = match kind {
            TokenKind::Ident => {
                let s = &self.str_table[*payload as usize];
                Token::Ident(Ident(s.clone()))
            }
            TokenKind::Backtick => Token::Backtick,
            TokenKind::String => {
                let s = &self.str_table[*payload as usize];
                Token::String(s.clone())
            }
            TokenKind::QuotedString => {
                let s = &self.str_table[*payload as usize];
                Token::QuotedString(s.clone())
            }
            TokenKind::UnquotedString => {
                let s = &self.str_table[*payload as usize];
                Token::UnquotedString(s.clone())
            }
            TokenKind::Number => {
                let n = self.number_table[*payload as usize];
                Token::Number(n)
            }
            TokenKind::Assign => Token::Assign,
            TokenKind::Newline => Token::Newline,
            TokenKind::LParen => Token::LParen,
            TokenKind::RParen => Token::RParen,
            TokenKind::Plus => Token::Plus,
            TokenKind::Minus => Token::Minus,
            TokenKind::Star => Token::Star,
            TokenKind::Slash => Token::Slash,
            TokenKind::Eq => Token::Eq,
            TokenKind::Ne => Token::Ne,
            TokenKind::Bang => Token::Bang,
            TokenKind::If => Token::If,
            TokenKind::Alias => Token::Alias,
            TokenKind::Then => Token::Then,
            TokenKind::QuestionMark => Token::QuestionMark,
            TokenKind::Else => Token::Else,
            TokenKind::End => Token::End,
            TokenKind::Pipe => Token::Pipe,
            TokenKind::Gt => Token::Gt,
            TokenKind::Lt => Token::Lt,
            TokenKind::Le => Token::Le,
            TokenKind::Ge => Token::Ge,
            TokenKind::BoolTrue => Token::BoolTrue,
            TokenKind::BoolFalse => Token::BoolFalse,
            TokenKind::Write => Token::Write,
            TokenKind::Append => Token::Append,
            TokenKind::Dollar => Token::Dollar,
            TokenKind::Export => Token::Export,
            TokenKind::Invalid => Token::Invalid(*payload as u8 as char),
            TokenKind::Eof => Token::Eof,
        };

        Some(t)
    }

    pub fn get_ident(&self, main: usize) -> Option<Ident> {
        self.get(main).and_then(|t| match t {
            Token::Ident(ident) => Some(ident),
            _ => None,
        })
    }

    pub fn get_string(&self, main: usize) -> Option<SmolStr> {
        self.get(main).and_then(|t| match t {
            Token::String(s) | Token::QuotedString(s) => Some(s),
            _ => None,
        })
    }

    pub fn iter(&self) -> TokensIter<'_> {
        TokensIter { buf: self, i: 0 }
    }

    pub fn offset(&self, i: usize) -> Option<usize> {
        Some(self.get_info(i)?.offset)
    }

    pub fn len(&self) -> usize {
        self.infos.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub(super) fn skip(&mut self, n: usize) {
        self.offset += n;
    }

    fn get_info(&self, i: usize) -> Option<&TokenInfo> {
        self.infos.get(i)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub(super) enum TokenKind {
    Ident,
    Number,
    String,
    UnquotedString,
    QuotedString,
    BoolTrue,
    BoolFalse,
    Assign,
    Pipe,
    Write,
    Append,
    Newline,
    Backtick,
    QuestionMark,
    Dollar,

    LParen,
    RParen,

    Plus,
    Minus,
    Star,
    Slash,
    Eq,
    Ne,
    Gt,
    Lt,
    Le,
    Ge,
    Bang,

    If,
    Then,
    Else,
    End,
    Alias,
    Export,

    Invalid,
    Eof,
}

pub struct TokensIter<'a> {
    buf: &'a TokenBuffer,
    i: usize,
}

impl<'a> Iterator for TokensIter<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.buf.get(self.i)?;
        self.i += 1;
        Some(tok)
    }
}

impl PartialEq for TokenBuffer {
    /// Compare two token buffers based on the tokens they contain.
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len()
            && self
                .infos
                .iter()
                .zip(other.infos.iter())
                .all(|(a, b)| a.kind == b.kind)
            && self.str_table == other.str_table
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord)]
pub struct Ident(SmolStr);

impl Ident {
    pub fn new(s: &str) -> Self {
        Self(s.into())
    }
}

impl Deref for Ident {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Token {
    pub(super) fn kind(&self) -> TokenKind {
        match self {
            Token::Ident(_) => TokenKind::Ident,
            Token::Number(_) => TokenKind::Number,
            Token::String(_) => TokenKind::String,
            Token::UnquotedString(_) => TokenKind::UnquotedString,
            Token::QuotedString(_) => TokenKind::QuotedString,
            Token::Assign => TokenKind::Assign,
            Token::Backtick => TokenKind::Backtick,

            Token::LParen => TokenKind::LParen,
            Token::RParen => TokenKind::RParen,

            Token::Plus => TokenKind::Plus,
            Token::Minus => TokenKind::Minus,
            Token::Star => TokenKind::Star,
            Token::Slash => TokenKind::Slash,
            Token::Eq => TokenKind::Eq,
            Token::Ne => TokenKind::Ne,
            Token::Bang => TokenKind::Bang,
            Token::Pipe => TokenKind::Pipe,

            Token::If => TokenKind::If,
            Token::Then => TokenKind::Then,
            Token::Else => TokenKind::Else,
            Token::End => TokenKind::End,
            Token::Export => TokenKind::Export,
            Token::Newline => TokenKind::Newline,
            Token::Invalid(_) => TokenKind::Invalid,
            Token::QuestionMark => TokenKind::QuestionMark,
            Token::Gt => TokenKind::Gt,
            Token::Lt => TokenKind::Lt,
            Token::Le => TokenKind::Le,
            Token::BoolTrue => TokenKind::BoolTrue,
            Token::BoolFalse => TokenKind::BoolFalse,
            Token::Ge => TokenKind::Ge,
            Token::Eof => TokenKind::Eof,
            Token::Write => TokenKind::Write,
            Token::Append => TokenKind::Append,
            Token::Dollar => TokenKind::Dollar,
            Token::Alias => TokenKind::Alias,
        }
    }

    pub fn size(&self) -> usize {
        match self {
            Token::Assign
            | Token::LParen
            | Token::RParen
            | Token::Plus
            | Token::Minus
            | Token::Star
            | Token::Slash
            | Token::Newline
            | Token::Invalid(_)
            | Token::Bang
            | Token::Pipe
            | Token::Backtick
            | Token::QuestionMark
            | Token::Gt
            | Token::Lt
            | Token::Write
            | Token::Dollar => 1,
            Token::Eq | Token::Ne | Token::If | Token::Le | Token::Ge | Token::Append => 2,
            Token::Then | Token::Else | Token::BoolTrue => 4,
            Token::Alias | Token::BoolFalse => 5,
            Token::Export => 6,
            Token::End => 3,
            Token::Ident(Ident(s)) | Token::String(s) => s.len(),
            Token::QuotedString(s) => s.len() + 2,
            Token::UnquotedString(s) => s.len() + 1,
            // TODO: perhaps the ryu crate can be used here?
            Token::Number(n) => n.to_string().len(),
            Token::Eof => 0,
        }
    }

    pub(super) fn from_kw(kw: &str) -> Option<Self> {
        Some(match kw {
            "if" => Token::If,
            "then" => Token::Then,
            "else" => Token::Else,
            "end" => Token::End,
            "alias" => Token::Alias,
            "export" => Token::Export,
            _ => return None,
        })
    }

    pub(super) fn is_mode_switch_kw(&self) -> bool {
        matches!(self, Token::If)
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Ident(ident) => write!(f, "ident `{ident}`"),
            Token::Number(i) => write!(f, "int `{i}`"),
            Token::String(s) => write!(f, "string `{s}`"),
            Token::UnquotedString(s) => write!(f, "unquoted string `{s}`"),
            Token::QuotedString(s) => write!(f, "quoted string `{s}`"),
            Token::BoolTrue => write!(f, "true"),
            Token::BoolFalse => write!(f, "false"),
            Token::Assign => write!(f, "assign"),
            Token::Newline => write!(f, "newline"),
            Token::LParen => write!(f, "lparen"),
            Token::RParen => write!(f, "rparen"),
            Token::Plus => write!(f, "plus"),
            Token::Minus => write!(f, "minus"),
            Token::Star => write!(f, "star"),
            Token::Slash => write!(f, "slash"),
            Token::Eq => write!(f, "eq"),
            Token::Ne => write!(f, "ne"),
            Token::Gt => write!(f, "gt"),
            Token::Lt => write!(f, "lt"),
            Token::Le => write!(f, "le"),
            Token::Ge => write!(f, "ge"),
            Token::QuestionMark => write!(f, "question mark"),
            Token::Bang => write!(f, "bang"),
            Token::If => write!(f, "if"),
            Token::Alias => write!(f, "alias"),
            Token::Then => write!(f, "then"),
            Token::Else => write!(f, "else"),
            Token::End => write!(f, "end"),
            Token::Export => write!(f, "export"),
            Token::Pipe => write!(f, "pipe"),
            Token::Write => write!(f, "write"),
            Token::Dollar => write!(f, "dollar"),
            Token::Append => write!(f, "append"),
            Token::Backtick => write!(f, "backtick"),
            Token::Invalid(c) => write!(f, "invalid `{c}`"),
            Token::Eof => write!(f, "eof"),
        }
    }
}
