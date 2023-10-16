use std::rc::Rc;

use logos::{self, Lexer, Logos};

fn hex(lex: &mut Lexer<Token>) -> Rc<str> {
    let hex_str = lex
        .slice()
        .strip_prefix("0x")
        .expect("should have leading 0x");
    let hex = u128::from_str_radix(hex_str, 16).expect("should be valid hex number");
    hex.to_string().into()
}

fn binary_to_decimal(lex: &mut Lexer<Token>) -> Rc<str> {
    let binary_str = lex
        .slice()
        .strip_prefix("0b")
        .expect("should have leading 0b");
    let binary = u128::from_str_radix(binary_str, 2).expect("should be valid hex number");
    binary.to_string().into()
}

fn parse_string(lex: &mut Lexer<Token>) -> Option<Rc<str>> {
    snailquote::unescape(lex.slice()).ok().map(Into::into)
}

#[derive(Logos, Debug, PartialEq, Clone)]
// whitespace
#[logos(skip r"[ \t\n\f]+")]
// annotations
#[logos(skip r"\([^)]+\)")]
// comments
#[logos(skip r";;.*")]
pub enum Token {
    #[regex(r"[_a-z][_a-z0-9\.]*", |lex| Rc::from(lex.slice()))]
    Instruction(Rc<str>),
    #[regex("\"(?s:[^\"\\\\]|\\\\.)*\"", parse_string)]
    String(Rc<str>),

    // numbers are left as strings, in order to make their size ambiguos
    #[regex(r"-?[0-9]+", |lex| Rc::from(lex.slice()), priority = 10)]
    #[regex(r"0x[0-9a-fA-F]+", hex)]
    #[regex(r"0b[0-1]+", binary_to_decimal)]
    Integer(Rc<str>),
    #[regex(r"-?[0-9]+\.[0-9]+", |lex| Rc::from(lex.slice()))]
    Float(Rc<str>),

    #[token("instrs")]
    Instrs,

    #[token("custom")]
    Custom,
    #[token("type")]
    Type,
    #[token("import")]
    Import,
    #[token("function")]
    Function,
    #[token("table")]
    Table,
    #[token("memory")]
    Memory,
    #[token("global")]
    Global,
    #[token("export")]
    Export,
    #[token("start")]
    Start,
    #[token("element")]
    Element,
    #[token("code")]
    Code,
    #[token("data")]
    Data,
    #[token("datacount")]
    DataCount,
    #[token("i32")]
    I32,
    #[token("i64")]
    I64,
    #[token("f32")]
    F32,
    #[token("f64")]
    F64,
    #[token("funcref")]
    Funcref,
    #[token("externref")]
    Externref,
    #[token("form.func")]
    FuncForm,
    #[token("extern.func")]
    ExternFunc,
    #[token("extern.table")]
    ExternTable,
    #[token("extern.mem")]
    ExternMem,
    #[token("extern.global")]
    ExternGlobal,
    #[token("magic")]
    Magic,
    #[token("version")]
    Version,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token(",")]
    Comma,

    #[token("{")]
    Lbrace,
    #[token("}")]
    Rbrace,
    #[token("[")]
    Lbracket,
    #[token("]")]
    Rbracket,

    #[token("<")]
    Langle,
    #[token(">")]
    Rangle,
    #[token("u8")]
    U8,
    #[token("u32")]
    U32,
    #[token("u64")]
    U64,
    #[token("s32")]
    S32,
    #[token("s33")]
    S33,
    #[token("s64")]
    S64,

    Eof,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexes_binary_literals() {
        let mut lexer = Token::lexer("0b1010");
        assert_eq!(
            Token::Integer("10".into()),
            lexer.next().expect("should lex something").unwrap()
        );
    }

    #[test]
    fn lexes_negative_number_literals() {
        let mut lexer = Token::lexer("-10");
        assert_eq!(
            Token::Integer("-10".into()),
            lexer.next().expect("should lex something").unwrap()
        );
    }

    #[test]
    fn lexes_negative_float_literals() {
        let mut lexer = Token::lexer("-10.1 1.3");
        assert_eq!(
            Token::Float("-10.1".into()),
            lexer.next().expect("should lex something").unwrap()
        );
        assert_eq!(
            Token::Float("1.3".into()),
            lexer.next().expect("should lex something").unwrap()
        );
    }

    #[test]
    fn skips_annotations() {
        let mut lexer = Token::lexer("123 (hello) 456");
        assert_eq!(
            Token::Integer("123".into()),
            lexer.next().expect("should lex something").unwrap()
        );
        assert_eq!(
            Ok(Token::Integer("456".into())),
            lexer.next().expect("should lex something")
        );
    }

    #[test]
    fn lexes_instructions_before_type_names() {
        let mut lexer = Token::lexer("f32.const f32");
        assert_eq!(
            Token::Instruction("f32.const".into()),
            lexer.next().expect("should lex something").unwrap()
        );
        assert_eq!(
            Token::F32,
            lexer.next().expect("should lex something").unwrap()
        );
    }

    #[test]
    fn lexes_hexadecimal_literals() {
        let mut lexer = Token::lexer("0xde3");
        assert_eq!(
            Token::Integer("3555".into()),
            lexer.next().expect("should lex something").unwrap()
        );
    }

    #[test]
    fn annotations_with_arbitrary_characters() {
        let mut lexer = Token::lexer("(1239----...&&&)");
        assert!(lexer.next().is_none());
    }
}
