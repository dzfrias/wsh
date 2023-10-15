use logos::{self, Lexer, Logos};

fn byte(lex: &mut Lexer<Token>) -> u8 {
    let hex_str = lex
        .slice()
        .strip_prefix("b0x")
        .expect("should have leading 0x");
    u8::from_str_radix(hex_str, 16).expect("should be valid hex number")
}

fn hex(lex: &mut Lexer<Token>) -> u32 {
    let hex_str = lex
        .slice()
        .strip_prefix("0x")
        .expect("should have leading 0x");
    u32::from_str_radix(hex_str, 16).expect("should be valid hex number")
}

fn binary_to_decimal(lex: &mut Lexer<Token>) -> u32 {
    let binary_str = lex
        .slice()
        .strip_prefix("0b")
        .expect("should have leading 0b");
    u32::from_str_radix(binary_str, 2).expect("should be valid binary number")
}

fn parse_string(lex: &mut Lexer<Token>) -> String {
    snailquote::unescape(lex.slice()).unwrap()
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+")]
#[logos(skip r"\([\w\s]+\)")]
#[logos(skip r";;.*")]
pub enum Token {
    #[regex(r"[_a-z][_a-z0-9\.]*", |lex| lex.slice().to_owned())]
    Instruction(String),
    #[regex(r"[0-9]+", |lex| lex.slice().parse::<u32>().unwrap())]
    #[regex(r"0x[0-9a-fA-F]+", hex)]
    #[regex(r"0b[0-1]+", binary_to_decimal)]
    Integer(u32),
    #[regex(r"b0x[0-9a-fA-F]{2}", byte)]
    Byte(u8),
    #[regex("\"(?s:[^\"\\\\]|\\\\.)*\"", parse_string)]
    String(String),

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

    Eof,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexes_binary_literals() {
        let mut lexer = Token::lexer("0b1010");
        assert_eq!(
            Token::Integer(10),
            lexer.next().expect("should lex something").unwrap()
        );
    }

    #[test]
    fn skips_annotations() {
        let mut lexer = Token::lexer("123 (hello) 456");
        assert_eq!(
            Token::Integer(123),
            lexer.next().expect("should lex something").unwrap()
        );
        assert_eq!(
            Ok(Token::Integer(456)),
            lexer.next().expect("should lex something")
        );
    }

    #[test]
    fn lexes_hexadecimal_literals() {
        let mut lexer = Token::lexer("0xde3");
        assert_eq!(
            Token::Integer(3555),
            lexer.next().expect("should lex something").unwrap()
        );
    }
}
