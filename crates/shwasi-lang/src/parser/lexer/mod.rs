mod token;

use std::str::CharIndices;

pub use token::*;

#[derive(Debug)]
pub struct Lexer<'src> {
    src: &'src str,
    chars: CharIndices<'src>,

    mode: Mode,
    peek: Option<(usize, char)>,
    pos: usize,
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            src,
            chars: src.char_indices(),

            mode: Mode::Normal,
            peek: None,
            pos: 0,
        }
    }

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
                '\n' => push!(Newline),
                ' ' | '\t' => buf.skip(1),

                '=' if self.peek() == Some('=') && self.strict() => {
                    self.next();
                    push!(Eq);
                }
                '!' if self.peek() == Some('=') && self.strict() => {
                    self.next();
                    push!(Ne);
                }
                '!' if self.strict() => push!(Bang),
                '+' if self.strict() => push!(Plus),
                '-' if self.strict() => push!(Minus),
                '*' if self.strict() => push!(Star),
                '/' if self.strict() => push!(Slash),
                i if i.is_ascii_digit() && self.strict() => {
                    let n = self.consume_num();
                    push!(Number(n.parse().unwrap()));
                }
                '=' => push!(Assign),
                '|' => push!(Pipe),

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

                '.' if !self.strict() => match self.peek() {
                    Some('(') => {
                        buf.skip(1);
                        self.mode = Mode::Strict {
                            ending: Token::RParen,
                            count: 0,
                        };
                    }
                    Some(c) if is_ident_char(c) && !c.is_ascii_digit() => {
                        buf.skip(1);
                        self.next();
                        push!(Ident(token::Ident::new(self.consume_str())));
                    }
                    _ => push!(String(self.consume_str().into())),
                },

                // Identifiers in strict mode. Note that they cannot start with a digit
                c if is_ident_char(c) && !c.is_ascii_digit() && self.strict() => {
                    let s = self.consume_ident();
                    match Token::from_kw(s) {
                        Some(t) if t.is_mode_switch_kw() => {
                            self.mode = Mode::StrictNoEnd;
                            push!(t);
                        }
                        Some(t) => {
                            self.mode = Mode::Normal;
                            push!(t);
                        }
                        None => push!(Ident(token::Ident::new(s))),
                    }
                }
                // Everything else in normal mode is a string
                _ if !self.strict() => {
                    let s = self.consume_str();
                    match Token::from_kw(s) {
                        Some(t) if t.is_mode_switch_kw() => {
                            self.mode = Mode::StrictNoEnd;
                            push!(t);
                        }
                        Some(t) => {
                            self.mode = Mode::Normal;
                            push!(t);
                        }
                        None => push!(String(s.into())),
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
            return Some(c);
        }

        let (pos, c) = self.chars.next()?;
        self.pos = pos;
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
        self.consume_while(|c| !c.is_whitespace())
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
        let end_pos = self.pos;
        &self.src[start_pos..end_pos + 1]
    }
}

fn is_ident_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

#[derive(Debug)]
enum Mode {
    Normal,
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
            Token::String("hello".into()),
            Token::Newline,
            Token::String("echo".into()),
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
            Token::String("echo".into()),
            Token::String("hi".into()),
            Token::End,
            Token::Eof,
        );
        assert_eq!(expect, buf);
    }

    #[test]
    fn dot_in_strict_err() {
        let input = "if .x == y then echo hi end";
        let lexer = Lexer::new(input);
        let buf = lexer.lex();
        let expect = token_buf!(
            Token::If,
            Token::Invalid('.'),
            Token::Ident(super::token::Ident::new("x")),
            Token::Eq,
            Token::Ident(super::token::Ident::new("y")),
            Token::Then,
            Token::String("echo".into()),
            Token::String("hi".into()),
            Token::End,
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
}
