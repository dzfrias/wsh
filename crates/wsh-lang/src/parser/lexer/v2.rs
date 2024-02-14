use std::{collections::VecDeque, ops::Range, str::Chars};
use tracing::trace;

#[derive(Debug, Clone)]
pub struct Token<'src> {
    start: usize,
    kind: TokenKind<'src>,
}

impl<'src> Token<'src> {
    /// The offset pointing to the start of the token.
    pub fn start(&self) -> usize {
        self.start
    }

    /// The [`TokenKind`] of the token.
    pub fn kind(&self) -> TokenKind<'src> {
        self.kind
    }

    /// The size of the token.
    pub fn size(&self) -> usize {
        self.kind().size()
    }

    pub fn range(&self) -> Range<usize> {
        self.start()..self.end()
    }

    /// The offset pointing to the end of the token, not inclusive.
    pub fn end(&self) -> usize {
        self.start() + self.size()
    }
}

impl Default for Token<'_> {
    fn default() -> Self {
        Self {
            start: 0,
            kind: TokenKind::Eof,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind<'src> {
    Ident(&'src str),
    Number(&'src str),
    String(&'src str),
    EnvVar(&'src str),
    BadString(&'src str),
    BadNumber(&'src str),

    BoolTrue,
    BoolFalse,
    Backtick,
    Assign,
    Pipe,
    PercentPipe,
    Write,
    PercentWrite,
    Append,
    PercentAppend,
    Semicolon,
    Space,
    QuestionMark,
    Tilde,
    Colon,
    ColonAssign,
    At,

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
    Def,
    While,
    Break,
    Return,
    Continue,
    Do,
    Then,
    Else,
    End,

    Invalid,
    Eof,
}

impl TokenKind<'_> {
    pub fn from_kw(s: &str) -> Option<Self> {
        let kw = match s {
            "if" => Self::If,
            "else" => Self::Else,
            "then" => Self::Then,
            "while" => Self::While,
            "end" => Self::End,
            "do" => Self::Do,
            "def" => Self::Def,
            "break" => Self::Break,
            "continue" => Self::Continue,
            "return" => Self::Return,
            _ => return None,
        };
        Some(kw)
    }

    /// Get the expected size of the token in as a string.
    pub fn size(&self) -> usize {
        match self {
            TokenKind::Eof | TokenKind::Space => 0,
            TokenKind::Assign
            | TokenKind::LParen
            | TokenKind::RParen
            | TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::Semicolon
            | TokenKind::Invalid
            | TokenKind::Bang
            | TokenKind::Pipe
            | TokenKind::Backtick
            | TokenKind::QuestionMark
            | TokenKind::Gt
            | TokenKind::Lt
            | TokenKind::Write
            | TokenKind::Tilde
            | TokenKind::Colon | TokenKind::At => 1,
            TokenKind::Eq
            | TokenKind::Ne
            | TokenKind::If
            | TokenKind::Le
            | TokenKind::Ge
            | TokenKind::Append
            | TokenKind::PercentPipe
            | TokenKind::PercentWrite
            | TokenKind::PercentAppend
            | TokenKind::Do
            | TokenKind::ColonAssign => 2,
            TokenKind::End | TokenKind::Def => 3,
            TokenKind::Then | TokenKind::Else | TokenKind::BoolTrue => 4,
            TokenKind::BoolFalse | TokenKind::While | TokenKind::Break => 5,
            TokenKind::Return => 6,
            TokenKind::Continue => 8,
            TokenKind::Ident(s)
            | TokenKind::String(s)  // TODO: this doesn't work with strings with quotes
            | TokenKind::BadNumber(s)
            | TokenKind::Number(s) => s.len(),
            TokenKind::BadString(s) | TokenKind::EnvVar(s) => s.len() + 1,
        }
    }
}

/// The current mode of the lexer. This will decide how certain characters are treated.
#[derive(Debug, Clone, Copy)]
pub enum LexMode {
    Normal,
    Strict,
}

impl LexMode {
    #[must_use]
    pub fn is_strict(&self) -> bool {
        matches!(self, Self::Strict)
    }

    #[must_use]
    pub fn is_normal(&self) -> bool {
        matches!(self, Self::Normal)
    }
}

#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    src: &'src str,
    chars: Chars<'src>,
    peek_buf: VecDeque<char>,

    current: char,
    pos: usize,
    nested_parens: usize,
    insert_semi: bool,
}

impl<'src> Lexer<'src> {
    /// Create a new lexer for the given source code.
    pub fn new(src: &'src str) -> Self {
        let mut lexer = Self {
            src,
            chars: src.chars(),
            current: '\0',
            pos: 0,
            nested_parens: 0,
            peek_buf: VecDeque::new(),
            insert_semi: false,
        };
        lexer.consume();
        lexer.pos = 0;
        lexer
    }

    /// Return the next token in the input stream.
    ///
    /// This takes in a `LexMode` because lexical analysis in wsh is context-sensitive. The
    /// `LexMode` decides how some characters should be treated. For example, in regular mode, the
    /// tokens in
    ///    echo + 10
    /// should all be strings. In strict mode, they should be special. The context for whether to
    /// get the next token in strict or normal mode depends on the parsing state.
    pub fn next_token(&mut self, mut mode: LexMode) -> Token<'src> {
        if self.pos > self.src.len() {
            return Token {
                start: self.pos - 1,
                kind: TokenKind::Eof,
            };
        }

        /// Helper macro to consume a token and return a new one
        macro_rules! consume_and {
            ($t:ident) => {{
                self.consume();
                TokenKind::$t
            }};
        }

        // Skip vertical whitespace, if possible
        if !self.insert_semi {
            self.consume_while(|c| c == '\n');
        }
        // Dots will force us into strict mode
        if self.current == '.' && !matches!(self.peek(), '.' | ' ' | '\0' | '\t') {
            self.consume();
            mode = LexMode::Strict;
        }
        // Skip comments
        if self.current == '#' {
            self.consume_while(|c| c != '\n');
            self.consume();
        }

        let mut consume_next = true;
        let ch = self.current;
        let pos = self.pos;
        let kind = match ch {
            // --Strict--
            ':' if self.peek() == '=' && mode.is_strict() => consume_and!(ColonAssign),
            '=' if self.peek() == '=' && mode.is_strict() => consume_and!(Eq),
            '!' if self.peek() == '=' && mode.is_strict() => consume_and!(Ne),
            '<' if self.peek() == '=' && mode.is_strict() => consume_and!(Le),
            '>' if self.peek() == '=' && mode.is_strict() => consume_and!(Ge),
            '<' if mode.is_strict() => TokenKind::Lt,
            '>' if mode.is_strict() => TokenKind::Gt,
            '!' if mode.is_strict() => TokenKind::Bang,
            '?' if mode.is_strict() => TokenKind::QuestionMark,
            '+' if mode.is_strict() => TokenKind::Plus,
            '-' if mode.is_strict() => TokenKind::Minus,
            '*' if mode.is_strict() => TokenKind::Star,
            '/' if mode.is_strict() => TokenKind::Slash,
            ':' if mode.is_strict() => TokenKind::Colon,
            '=' if mode.is_strict() => TokenKind::Assign,
            '(' if mode.is_strict() => {
                self.nested_parens += 1;
                // Too many nested parens will cause a stack overflow in the parser, and the lexer can prevent that
                if self.nested_parens > 32 {
                    TokenKind::Invalid
                } else {
                    TokenKind::LParen
                }
            }
            ')' if mode.is_strict() => {
                self.nested_parens = self.nested_parens.saturating_sub(1);
                TokenKind::RParen
            }
            i if i.is_ascii_digit() && mode.is_strict() => {
                let n = self.consume_while(|c| c.is_ascii_alphanumeric() || c == '.');
                consume_next = false;
                if n.parse::<f64>().is_err() {
                    TokenKind::BadNumber(n)
                } else {
                    TokenKind::Number(n)
                }
            }
            c if is_ident_char(c) && mode.is_strict() => {
                let s = self.consume_ident();
                consume_next = false;
                match TokenKind::from_kw(s) {
                    Some(t) => t,
                    None if s == "true" => TokenKind::BoolTrue,
                    None if s == "false" => TokenKind::BoolFalse,
                    None => TokenKind::Ident(s),
                }
            }
            '^' if mode.is_strict() => {
                self.consume();
                consume_next = false;
                TokenKind::Ident(self.consume_ident())
            }

            // --Normal--
            '|' => TokenKind::Pipe,
            '%' if self.peek() == '|' => consume_and!(PercentPipe),
            '%' if self.peek() == '>' => {
                self.consume();
                if self.peek() == '>' {
                    self.consume();
                    TokenKind::PercentAppend
                } else {
                    TokenKind::PercentWrite
                }
            }
            '>' if self.peek() == '>' => consume_and!(Append),
            '>' => TokenKind::Write,
            '`' => TokenKind::Backtick,
            ';' => TokenKind::Semicolon,
            // This is a special edge case. If this was not here, we would go to the default case
            // (`consume_unquoted_str`), which would see the '%' and not move any further, causing
            // the lexer to get stuck. This makes sure that a single (unaccompanied) '%' will make
            // progress in the lexer.
            '%' => TokenKind::Invalid,
            '~' if mode.is_normal() => TokenKind::Tilde,
            '@' if mode.is_normal() && !matches!(self.peek(), ' ' | '\t' | '\n' | '\0') => {
                TokenKind::At
            }
            '^' => {
                self.consume();
                consume_next = false;
                TokenKind::String(self.consume_unquoted_str())
            }
            '"' | '\'' => {
                let (s, ok) = self.consume_quoted_str();
                consume_next = false;
                if ok {
                    TokenKind::String(s)
                } else {
                    TokenKind::BadString(s)
                }
            }
            '$' => {
                self.consume();
                consume_next = false;
                TokenKind::EnvVar(self.consume_ident())
            }

            // --Whitespace--
            ' ' | '\t' => {
                self.consume_while(is_horizontal_ws);
                consume_next = false;
                TokenKind::Space
            }
            '\0' => TokenKind::Eof,
            '\n' => {
                self.insert_semi = false;
                TokenKind::Semicolon
            }

            // --Default--
            _ if mode.is_normal() => {
                let s = self.consume_unquoted_str();
                consume_next = false;
                TokenKind::from_kw(s).unwrap_or(TokenKind::String(s))
            }
            _ => TokenKind::Invalid,
        };
        if consume_next {
            self.consume();
        }

        // This powers the automatic semicolon insertion. There are a few prerequisites:
        // - The token is a special keyword, literal, or closing parenthesis
        // - The next token (self.current) is a newline (or has horizontal whitespace leading up to
        //   a newline)
        // This is similar to Go's semicolon rules. However, there is one minor adjustment: if the
        // prerequisites hold, and the character after the newline is a pipe or redirect (or a
        // binary operator in strict mode), the semicolon will not be inserted. This allows for
        // multi-line piping:
        // echo hi
        //   | wc -w
        //   | xargs
        if matches!(
            kind,
            TokenKind::Continue
                | TokenKind::Return
                | TokenKind::Break
                | TokenKind::BoolFalse
                | TokenKind::BoolTrue
                | TokenKind::String(_)
                | TokenKind::Ident(_)
                | TokenKind::EnvVar(_)
                | TokenKind::Tilde
                | TokenKind::QuestionMark
                | TokenKind::Number(_)
                | TokenKind::RParen
                | TokenKind::End
                | TokenKind::Backtick
        ) && self.should_semicolon(mode)
        {
            self.insert_semi = true;
        }

        trace!("read token: {kind:?} at {pos}");
        Token { start: pos, kind }
    }

    /// Peek the next token in the text, skipping whitepsace.
    pub fn peek_token(&mut self, mode: LexMode) -> Token<'src> {
        let clone = self.clone();
        let mut t = self.next_token(mode);
        while t.kind() == TokenKind::Space {
            t = self.next_token(mode);
        }
        *self = clone;
        t
    }

    /// Peek the next token in the text.
    pub fn peek_token_raw(&mut self, mode: LexMode) -> Token<'src> {
        let clone = self.clone();
        let t = self.next_token(mode);
        *self = clone;
        t
    }

    /// Advance the lexer by one character.
    fn consume(&mut self) {
        self.pos += self.current.len_utf8();
        self.current = self
            .peek_buf
            .pop_front()
            .unwrap_or_else(|| self.chars.next().unwrap_or('\0'));
    }

    /// Peek at the next character after `self.current`
    fn peek(&mut self) -> char {
        self.peek_buf.front().copied().unwrap_or_else(|| {
            let next = self.chars.next().unwrap_or('\0');
            self.peek_buf.push_back(next);
            next
        })
    }

    /// Consume characters while the given predicate holds. It will also halt once the end of the
    /// input has been received.
    fn consume_while<F>(&mut self, mut f: F) -> &'src str
    where
        F: FnMut(char) -> bool,
    {
        let start = self.pos;
        while self.current != '\0' && f(self.current) {
            self.consume();
        }
        let end = self.pos;
        &self.src[start..end]
    }

    fn consume_unquoted_str(&mut self) -> &'src str {
        self.consume_while(|c| !matches!(c, ';' | '|' | '%' | '>' | '`' | '$' | ' ' | '\t' | '\n'))
    }

    fn consume_quoted_str(&mut self) -> (&'src str, bool) {
        let end = self.current;
        self.consume();
        let s = self.consume_while(|c| c != end);
        if self.current == end {
            self.consume();
        } else {
            return (s, false);
        }
        (s, true)
    }

    fn consume_ident(&mut self) -> &'src str {
        self.consume_while(is_ident_char)
    }

    /// Returns `true` if the current position is viable for semicolon insertion. See the comment
    /// in the `next_token` method for the precise rules.
    fn should_semicolon(&mut self, mode: LexMode) -> bool {
        // TODO: memchr optimizations here?
        let mut current = self.current;
        while is_horizontal_ws(current) {
            current = self.chars.next().unwrap_or('\0');
            self.peek_buf.push_back(current);
        }

        if current != '\n' {
            return false;
        }
        current = self.chars.next().unwrap_or('\0');
        self.peek_buf.push_back(current);
        while is_horizontal_ws(current) {
            current = self.chars.next().unwrap_or('\0');
            self.peek_buf.push_back(current);
        }

        match mode {
            LexMode::Normal => !matches!(current, '|' | '%' | '>'),
            LexMode::Strict => !matches!(current, '+' | '-' | '/' | '*' | '>' | '<' | '!' | '='),
        }
    }
}

fn is_horizontal_ws(c: char) -> bool {
    matches!(c, ' ' | '\t')
}

fn is_ident_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;

    macro_rules! lexer_test {
        ($name:ident, $input:expr, [$($mode:ident $kind:expr),* $(,)?]) => {
            #[test]
            fn $name() {
                use self::TokenKind::*;
                #[allow(unused_mut, unused_variables)]
                let mut lexer = Lexer::new($input);
                $(
                    assert_eq!($kind, lexer.next_token(LexMode::$mode).kind());
                )*
                assert_eq!(TokenKind::Eof, lexer.next_token(LexMode::Normal).kind());
            }
        };
    }

    lexer_test!(
        basic_commands,
        "echo hello",
        [Normal String("echo"), Normal Space, Normal String("hello")]
    );

    lexer_test!(
        semicolon_insertion_after_strings,
        "hello\nhello     \n\n\nhi",
        [
            Normal String("hello"),
            Normal Semicolon,
            Normal String("hello"),
            Normal Space,
            Normal Semicolon,
            Normal String("hi"),
        ]
    );

    lexer_test!(
        semicolon_insertion_after_rparen,
        ".)\n",
        [
            Normal RParen,
            Normal Semicolon,
        ]
    );

    lexer_test!(
        numbers,
        "1.1 100 0.01 000.01",
        [
            Strict Number("1.1"),
            Strict Space,
            Strict Number("100"),
            Strict Space,
            Strict Number("0.01"),
            Strict Space,
            Strict Number("000.01"),
        ]
    );

    lexer_test!(
        keywords,
        "while if break continue return end do then else",
        [
            Normal While,
            Normal Space,
            Normal If,
            Normal Space,
            Normal Break,
            Normal Space,
            Normal Continue,
            Normal Space,
            Normal Return,
            Normal Space,
            Normal End,
            Normal Space,
            Normal Do,
            Normal Space,
            Normal Then,
            Normal Space,
            Normal Else,
        ]
    );

    lexer_test!(
        semicolon_after_some_keywords,
        "break\ncontinue\nreturn\nthen\nend\n",
        [
            Normal Break,
            Normal Semicolon,
            Normal Continue,
            Normal Semicolon,
            Normal Return,
            Normal Semicolon,
            Normal Then,
            Normal End,
            Normal Semicolon,
        ]
    );

    lexer_test!(
        string_break,
        "hello|hi>nice",
        [
            Normal String("hello"),
            Normal Pipe,
            Normal String("hi"),
            Normal Write,
            Normal String("nice"),
        ]
    );

    lexer_test!(
        strict_idents,
        "hello",
        [
            Strict Ident("hello"),
        ]
    );

    lexer_test!(
        dot_into_strict,
        ".hello.break.(",
        [
            Normal Ident("hello"),
            Normal Break,
            Normal LParen,
        ]
    );

    lexer_test!(
        escape_keywords,
        "^end",
        [
            Normal String("end"),
        ]
    );

    lexer_test!(
        invalid_numbers,
        ".101ls",
        [
            Normal BadNumber("101ls"),
        ]
    );

    lexer_test!(
        skip_comment,
        "# skip\nhello",
        [
            Normal String("hello"),
        ]
    );

    lexer_test!(
        multi_line_pipeline,
        "echo hello\n | echo hi\n| echo hi",
        [
            Normal String("echo"),
            Normal Space,
            Normal String("hello"),
            Normal Space,
            Normal Pipe,
            Normal Space,
            Normal String("echo"),
            Normal Space,
            Normal String("hi"),
            Normal Pipe,
            Normal Space,
            Normal String("echo"),
            Normal Space,
            Normal String("hi"),
        ]
    );

    lexer_test!(
        quoted_strings,
        "echo \"hello\" 'hi' \"bad",
        [
            Normal String("echo"),
            Normal Space,
            Normal String("hello"),
            Normal Space,
            Normal String("hi"),
            Normal Space,
            Normal BadString("bad"),
        ]
    );

    lexer_test!(
        tilde,
        "~/code co~de",
        [Normal Tilde, Normal String("/code"), Normal Space, Normal String("co~de")]
    );

    lexer_test!(
        at,
        "@hello @ hel@lo",
        [
            Normal At,
            Normal String("hello"),
            Normal Space,
            Normal String("@"),
            Normal Space,
            Normal String("hel@lo")
        ]
    );
}
