mod token;

use std::str::CharIndices;

pub use token::*;

/// A lexer that creates a `TokenBuffer` from a source string in the wsh shell language.
#[derive(Debug)]
pub struct Lexer<'src> {
    src: &'src str,
    chars: CharIndices<'src>,

    mode: Mode,
    peek: Option<(usize, char)>,
    current: char,
    pos: usize,
}

impl<'src> Lexer<'src> {
    /// Create a new lexer from the given source string.
    pub fn new(src: &'src str) -> Self {
        Self {
            src,
            chars: src.char_indices(),

            mode: Mode::Normal,
            current: '\0',
            peek: None,
            pos: 0,
        }
    }

    /// Lex the source string into a new `TokenBuffer`.
    pub fn lex(mut self) -> TokenBuffer {
        let mut buf = TokenBuffer::new();

        macro_rules! push {
            ($tok:expr) => {{
                #[allow(unused_imports)]
                use Token::*;

                buf.push($tok);
            }};
        }

        while let Some(c) = self.next() {
            match c {
                '\n' => {
                    let len = self.consume_while(|c| c == '\n').len();
                    buf.skip(len - 1);
                    if let Mode::Strict {
                        ending: Token::Newline,
                        ..
                    } = self.mode
                    {
                        self.mode = Mode::Normal;
                    }
                    push!(Newline);
                }
                ' ' | '\t' => {
                    let len = self.consume_while(|c| c.is_whitespace() && c != '\n').len();
                    buf.skip(len);
                    if !self.strict() {
                        push!(Space);
                    }
                }

                ':' if self.peek() == Some('=') => {
                    self.next();
                    push!(ColonAssign);
                }
                '=' if self.peek() == Some('=') && self.strict() => {
                    self.next();
                    push!(Eq);
                }
                '!' if self.peek() == Some('=') && self.strict() => {
                    self.next();
                    push!(Ne);
                }
                '<' if self.peek() == Some('=') && self.strict() => {
                    self.next();
                    push!(Le);
                }
                '>' if self.peek() == Some('=') && self.strict() => {
                    self.next();
                    push!(Ge);
                }
                '<' if self.strict() => push!(Lt),
                '>' if self.strict() => push!(Gt),
                '!' if self.strict() => push!(Bang),
                '?' if self.strict() => push!(QuestionMark),
                '+' if self.strict() => push!(Plus),
                '-' if self.strict() => push!(Minus),
                '*' if self.strict() => push!(Star),
                '/' if self.strict() => push!(Slash),
                ':' if self.strict() => push!(Colon),
                '#' => {
                    let s = self.consume_while(|c| c != '\n');
                    buf.skip(s.len() + 1);
                    self.next();
                }
                i if i.is_ascii_digit() && self.strict() => {
                    let n_str = self.consume_num();
                    let n = n_str.parse::<f64>().unwrap();
                    let len = n.to_string().len();
                    if n_str.len() > len {
                        buf.skip(n_str.len() - len);
                    }
                    // We need to skip the proper number of characters parsed. Since 1.00000 parses
                    // as 1, we need to skip the difference between the actual token length and the
                    // parsed number length.
                    push!(Number(n));
                }
                '=' => push!(Assign),
                '|' => push!(Pipe),
                '%' if self.peek() == Some('|') => {
                    self.next();
                    push!(PercentPipe);
                }
                '%' if self.peek() == Some('>') => {
                    self.next();
                    if self.peek() == Some('>') {
                        self.next();
                        push!(PercentAppend);
                    } else {
                        push!(PercentWrite);
                    }
                }
                '^' if self
                    .peek()
                    .is_some_and(|c| is_ident_char(c) && !c.is_ascii_digit()) =>
                {
                    self.next();
                    push!(Ident(token::Ident::new(self.consume_ident())));
                }
                '$' => {
                    push!(Dollar);
                    if self
                        .peek()
                        .is_some_and(|c| is_ident_char(c) && !c.is_ascii_digit())
                    {
                        self.next();
                        push!(Ident(token::Ident::new(self.consume_ident())));
                    }
                }
                '>' if self.peek() == Some('>') && !self.strict() => {
                    self.next();
                    push!(Append);
                }
                '~' if !self.strict() => push!(Tilde),
                '>' => push!(Write),
                '`' => {
                    if let Mode::NormalUntilBacktick(old) = self.mode {
                        self.mode = *old;
                    } else {
                        self.mode = Mode::NormalUntilBacktick(self.mode.into());
                    }
                    push!(Backtick);
                }
                '"' | '\'' => {
                    let (s, ok) = self.consume_quoted_str(c);
                    let tok = if ok {
                        Token::QuotedString(s.into())
                    } else {
                        Token::UnquotedString(s.into())
                    };
                    push!(tok);
                }

                '(' if self.strict() => {
                    if let Mode::Strict {
                        ending: Token::RParen,
                        ref mut count,
                    } = self.mode
                    {
                        *count += 1;
                    }
                    push!(LParen);
                }
                ')' if self.strict() => {
                    if let Mode::Strict {
                        ending: Token::RParen,
                        ref mut count,
                    } = self.mode
                    {
                        *count -= 1;
                        if *count == 0 {
                            self.mode = Mode::Normal;
                        }
                    }
                    push!(RParen);
                }

                '.' if self.strict() => match self.peek() {
                    Some(c) if is_ident_char(c) && !c.is_ascii_digit() => {
                        buf.skip(1);
                        self.next();
                        push!(Ident(token::Ident::new(self.consume_str())));
                    }
                    _ => push!(Token::Invalid('.')),
                },
                '.' if !self.strict() => match self.peek() {
                    Some('(') => {
                        buf.skip(1);
                        self.mode = Mode::Strict {
                            ending: Token::RParen,
                            count: 0,
                        };
                    }
                    Some('?') => {
                        buf.skip(1);
                        self.next();
                        push!(QuestionMark);
                    }
                    Some(c) if is_ident_char(c) && !c.is_ascii_digit() => {
                        buf.skip(1);
                        self.next();
                        push!(Ident(token::Ident::new(self.consume_str())));
                        // Edge case here for EOF
                        if self.peek().is_some() {
                            let ws = self.consume_while(|c| matches!(c, ' ' | '\t'));
                            if !ws.is_empty() {
                                push!(Space);
                            }
                        }
                        if matches!(self.peek(), Some('=' | ':')) {
                            self.mode = Mode::Strict {
                                ending: Token::Newline,
                                count: 0,
                            };
                        }
                    }
                    _ => push!(String(self.consume_str().into())),
                },

                // Identifiers in strict mode. Note that they cannot start with a digit
                c if is_ident_char(c) && !c.is_ascii_digit() && self.strict() => {
                    let s = self.consume_ident();
                    let last_is_newline = last_implicit_newline(&buf);
                    match Token::from_kw(s, last_is_newline) {
                        Some(t) if t.is_mode_switch_kw() => {
                            self.mode = Mode::StrictNoEnd;
                            push!(t);
                        }
                        Some(t) => {
                            self.mode = Mode::Normal;
                            push!(t);
                        }
                        None if s == "true" => push!(BoolTrue),
                        None if s == "false" => push!(BoolFalse),
                        _ => push!(Ident(token::Ident::new(s))),
                    }
                }
                // Everything else in normal mode is a string
                _ if !self.strict() => {
                    let s = self.consume_str();
                    let last_is_newline = last_implicit_newline(&buf);
                    match Token::from_kw(s, last_is_newline) {
                        Some(t) if t.is_mode_switch_kw() => {
                            self.mode = Mode::StrictNoEnd;
                            push!(t);
                        }
                        Some(t) => {
                            self.mode = Mode::Normal;
                            push!(t);
                        }
                        _ => push!(String(s.into())),
                    }
                }

                _ => push!(Invalid(c)),
            }
        }
        buf.push(Token::Eof);

        buf
    }

    fn next(&mut self) -> Option<char> {
        if let Some((pos, c)) = self.peek.take() {
            self.pos = pos;
            self.current = c;
            return Some(c);
        }

        let (pos, c) = self.chars.next()?;
        self.pos = pos;
        self.current = c;
        Some(c)
    }

    fn peek(&mut self) -> Option<char> {
        if let Some((_, c)) = self.peek {
            return Some(c);
        }

        self.peek = self.chars.next();
        self.peek.map(|p| p.1)
    }

    fn strict(&self) -> bool {
        matches!(self.mode, Mode::Strict { .. } | Mode::StrictNoEnd)
    }

    fn consume_str(&mut self) -> &'src str {
        self.consume_while(|c| {
            !c.is_whitespace() && !matches!(c, '`' | '|' | '=' | '(' | ')' | '>' | '$' | '%')
        })
    }

    fn consume_quoted_str(&mut self, end: char) -> (&'src str, bool) {
        self.next();
        let s = self.consume_while(|c| c != end);
        if self.peek() == Some(end) {
            self.next();
        } else {
            return (s, false);
        }
        (s, true)
    }

    fn consume_ident(&mut self) -> &'src str {
        self.consume_while(is_ident_char)
    }

    fn consume_num(&mut self) -> &'src str {
        let mut seen_dot = false;
        self.consume_while(|c| {
            if !c.is_ascii_digit() && (c != '.' || seen_dot) {
                return false;
            }
            if c == '.' {
                seen_dot = true;
            }

            true
        })
    }

    fn consume_while<F>(&mut self, mut f: F) -> &'src str
    where
        F: FnMut(char) -> bool,
    {
        let start_pos = self.pos;
        while let Some(c) = self.peek() {
            if !f(c) {
                break;
            }
            self.next();
        }
        let end = self.pos + self.current.len_utf8();
        &self.src[start_pos..end]
    }
}

fn is_ident_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

/// Returns true if the last non-space item in the buffer is an implicit newline token
///
/// An implicit newline token is one where keywords like "break" and "if" should be recognized if
/// they succeed the token. For example `while true do break end` should not lex "break" as a
/// string. `echo i'm finally on break`, **should** lex "break" as a string. Because of this
/// descrepancy there's a need for the notion of an "implicit newline" token.
fn last_implicit_newline(buf: &TokenBuffer) -> bool {
    if buf.is_empty() {
        return true;
    }
    if buf.last().unwrap() == Token::Space {
        let Some(second_last) = buf.len().checked_sub(2) else {
            return true;
        };
        return matches!(
            buf.get(second_last).unwrap(),
            Token::Newline | Token::Do | Token::Else | Token::Then
        );
    }

    matches!(
        buf.last().unwrap(),
        Token::Newline | Token::Do | Token::Else | Token::Then
    )
}

#[derive(Debug, PartialEq)]
enum Mode {
    Normal,
    NormalUntilBacktick(Box<Mode>),
    Strict { ending: Token, count: u32 },
    StrictNoEnd,
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! token_buf {
        ($($tok:expr),* $(,)?) => {{
            let mut buf = TokenBuffer::new();
            $(buf.push($tok);)*
            buf
        }};
    }

    #[test]
    fn basic_commands() {
        let input = "echo hello\necho hi";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(
            Token::String("echo".into()),
            Token::Space,
            Token::String("hello".into()),
            Token::Newline,
            Token::String("echo".into()),
            Token::Space,
            Token::String("hi".into()),
            Token::Eof,
        );
        assert_eq!(expect, buf);
    }

    #[test]
    fn idents() {
        let input = "echo .i";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(
            Token::String("echo".into()),
            Token::Space,
            Token::Ident(super::token::Ident::new("i")),
            Token::Eof,
        );
        assert_eq!(expect, buf);
    }

    #[test]
    fn escape_to_strict() {
        let input = "echo .(x + y)";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(
            Token::String("echo".into()),
            Token::Space,
            Token::LParen,
            Token::Ident(super::token::Ident::new("x")),
            Token::Plus,
            Token::Ident(super::token::Ident::new("y")),
            Token::RParen,
            Token::Eof,
        );
        assert_eq!(expect, buf);
    }

    #[test]
    fn if_() {
        let input = "if x == y then echo hi end";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(
            Token::If,
            Token::Ident(super::token::Ident::new("x")),
            Token::Eq,
            Token::Ident(super::token::Ident::new("y")),
            Token::Then,
            Token::Space,
            Token::String("echo".into()),
            Token::Space,
            Token::String("hi".into()),
            Token::Space,
            Token::End,
            Token::Eof,
        );
        assert_eq!(expect, buf);
    }

    #[test]
    fn dot_in_strict_err() {
        let input = "echo .(.)";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(
            Token::String("echo".into()),
            Token::Space,
            Token::LParen,
            Token::Invalid('.'),
            Token::RParen,
            Token::Eof,
        );
        assert_eq!(expect, buf);
    }

    #[test]
    fn regular_dot() {
        let input = "./nice";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(Token::String("./nice".into()), Token::Eof);
        assert_eq!(expect, buf);
    }

    #[test]
    fn integers() {
        let input = ".(1 + 1) 2";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(
            Token::LParen,
            Token::Number(1.0),
            Token::Plus,
            Token::Number(1.0),
            Token::RParen,
            Token::Space,
            Token::String("2".into()),
            Token::Eof,
        );
        assert_eq!(expect, buf);
    }

    #[test]
    fn floats() {
        let input = ".(19.333334 - 1.1)";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(
            Token::LParen,
            Token::Number(19.333334),
            Token::Minus,
            Token::Number(1.1),
            Token::RParen,
            Token::Eof,
        );
        assert_eq!(expect, buf);
    }

    #[test]
    fn strings_with_quotes() {
        let input = "echo \'hello world\' .(\"nice\" + \"ice\")";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(
            Token::String("echo".into()),
            Token::Space,
            Token::QuotedString("hello world".into()),
            Token::Space,
            Token::LParen,
            Token::QuotedString("nice".into()),
            Token::Plus,
            Token::QuotedString("ice".into()),
            Token::RParen,
            Token::Eof,
        );
        assert_eq!(expect, buf);
    }

    #[test]
    fn unquoted_strings() {
        let input = "echo \"hello";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(
            Token::String("echo".into()),
            Token::Space,
            Token::UnquotedString("hello".into()),
            Token::Eof,
        );
        assert_eq!(expect, buf);
    }

    #[test]
    fn backticks() {
        let input = "echo `nice`";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(
            Token::String("echo".into()),
            Token::Space,
            Token::Backtick,
            Token::String("nice".into()),
            Token::Backtick,
            Token::Eof,
        );
        assert_eq!(expect, buf);
    }

    #[test]
    fn pipe_after_string() {
        let input = "hello|";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(Token::String("hello".into()), Token::Pipe, Token::Eof);
        assert_eq!(expect, buf);
    }

    #[test]
    fn question_mark() {
        let input = "echo .? .(?)";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(
            Token::String("echo".into()),
            Token::Space,
            Token::QuestionMark,
            Token::Space,
            Token::LParen,
            Token::QuestionMark,
            Token::RParen,
            Token::Eof
        );
        assert_eq!(expect, buf);
    }

    #[test]
    fn alias() {
        let input = "alias echo = echo -n";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(
            Token::Alias,
            Token::Space,
            Token::String("echo".into()),
            Token::Space,
            Token::Assign,
            Token::Space,
            Token::String("echo".into()),
            Token::Space,
            Token::String("-n".into()),
            Token::Eof
        );
        assert_eq!(expect, buf);
    }

    #[test]
    fn assign() {
        let input = ".x = 10";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(
            Token::Ident(Ident::new("x")),
            Token::Space,
            Token::Assign,
            Token::Number(10.0),
            Token::Eof
        );
        assert_eq!(expect, buf);
    }

    #[test]
    fn comparison_tokens() {
        let input = ".(>= > < <=)";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(
            Token::LParen,
            Token::Ge,
            Token::Gt,
            Token::Lt,
            Token::Le,
            Token::RParen,
            Token::Eof
        );
        assert_eq!(expect, buf);
    }

    #[test]
    fn boolean_literals() {
        let input = "true false .(true false)";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(
            Token::String("true".into()),
            Token::Space,
            Token::String("false".into()),
            Token::Space,
            Token::LParen,
            Token::BoolTrue,
            Token::BoolFalse,
            Token::RParen,
            Token::Eof
        );
        assert_eq!(expect, buf);
    }

    #[test]
    fn write_redirect() {
        let input = "echo hi > t";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(
            Token::String("echo".into()),
            Token::Space,
            Token::String("hi".into()),
            Token::Space,
            Token::Write,
            Token::Space,
            Token::String("t".into()),
            Token::Eof
        );
        assert_eq!(expect, buf);
    }

    #[test]
    fn append_redirect() {
        let input = "echo hi >> t";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(
            Token::String("echo".into()),
            Token::Space,
            Token::String("hi".into()),
            Token::Space,
            Token::Append,
            Token::Space,
            Token::String("t".into()),
            Token::Eof
        );
        assert_eq!(expect, buf);
    }

    #[test]
    fn dollar() {
        let input = "$";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(Token::Dollar, Token::Eof);
        assert_eq!(expect, buf);
    }

    #[test]
    fn export() {
        let input = "export";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(Token::Export, Token::Eof);
        assert_eq!(expect, buf);
    }

    #[test]
    fn ignore_comments() {
        let input = "# I'm a comment!\necho hi";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(
            Token::String("echo".into()),
            Token::Space,
            Token::String("hi".into()),
            Token::Eof
        );
        assert_eq!(expect, buf);
    }

    #[test]
    fn newlines_collapse() {
        let input = "\n\n\n\nhi";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(Token::Newline, Token::String("hi".into()), Token::Eof);
        assert_eq!(expect, buf);
    }

    #[test]
    fn floats_without_leading_zero_no_crash() {
        let input = ".(.999999999999999999999999999999";
        let lexer = Lexer::new(input);
        lexer.lex();
    }

    #[test]
    fn some_keywords_need_newlines_before() {
        let input = "echo export alias if";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(
            Token::String("echo".into()),
            Token::Space,
            Token::String("export".into()),
            Token::Space,
            Token::String("alias".into()),
            Token::Space,
            Token::String("if".into()),
            Token::Eof
        );
        assert_eq!(expect, buf);
    }

    #[test]
    fn percent_redirects() {
        let input = "echo hi %| cat %> file.txt %>> hi.txt";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(
            Token::String("echo".into()),
            Token::Space,
            Token::String("hi".into()),
            Token::Space,
            Token::PercentPipe,
            Token::Space,
            Token::String("cat".into()),
            Token::Space,
            Token::PercentWrite,
            Token::Space,
            Token::String("file.txt".into()),
            Token::Space,
            Token::PercentAppend,
            Token::Space,
            Token::String("hi.txt".into()),
            Token::Eof
        );
        assert_eq!(expect, buf);
    }

    #[test]
    fn tilde() {
        let input = "echo ~/nice hell~o";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(
            Token::String("echo".into()),
            Token::Space,
            Token::Tilde,
            Token::String("/nice".into()),
            Token::Space,
            Token::String("hell~o".into()),
            Token::Eof
        );
        assert_eq!(expect, buf);
    }

    #[test]
    fn while_() {
        let input = "while true do echo hi end";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(
            Token::While,
            Token::BoolTrue,
            Token::Do,
            Token::Space,
            Token::String("echo".into()),
            Token::Space,
            Token::String("hi".into()),
            Token::Space,
            Token::End,
            Token::Eof
        );
        assert_eq!(expect, buf);
    }

    #[test]
    fn dot_idents_in_strict_mode() {
        let input = ".(.x + 1)";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(
            Token::LParen,
            Token::Ident(Ident::new("x")),
            Token::Plus,
            Token::Number(1.0),
            Token::RParen,
            Token::Eof
        );
        assert_eq!(expect, buf);
    }

    #[test]
    fn def_and_colon() {
        let input = "def f : x y do echo hello end";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(
            Token::Def,
            Token::Ident(Ident::new("f")),
            Token::Colon,
            Token::Ident(Ident::new("x")),
            Token::Ident(Ident::new("y")),
            Token::Do,
            Token::Space,
            Token::String("echo".into()),
            Token::Space,
            Token::String("hello".into()),
            Token::Space,
            Token::End,
            Token::Eof
        );
        assert_eq!(expect, buf);
    }

    #[test]
    fn escape_idents() {
        let input = "echo ^end";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(
            Token::String("echo".into()),
            Token::Space,
            Token::Ident(Ident::new("end")),
            Token::Eof
        );
        assert_eq!(expect, buf);
    }
}
