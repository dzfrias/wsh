mod error;
mod instructions;

use std::{borrow::Cow, io::Write};

use byteorder::{LittleEndian, WriteBytesExt};
use logos::Lexer;

pub use self::error::*;
use self::instructions::{ARITH_PREFIX_INSTRUCTIONS, INSTRUCTIONS};
use crate::lexer::Token;

const MAGIC: u32 = 0x6d73_6100;

#[derive(Debug)]
enum State {
    Section(Vec<u8>),
    Array { contents: Vec<u8>, items: u32 },
    Instructions(Vec<u8>),
}

#[derive(Debug)]
pub struct Writer<'src> {
    lexer: Lexer<'src, Token>,
    out: Vec<u8>,
    states: Vec<State>,
    token: Token,
}

impl<'src> Writer<'src> {
    pub fn new(lexer: Lexer<'src, Token>) -> Self {
        Self {
            lexer,
            out: vec![],
            states: vec![],
            token: Token::Eof,
        }
    }

    pub fn write(mut self) -> Result<Vec<u8>, WriteError> {
        // moves past initial Token::Eof
        self.advance()?;
        while self.token != Token::Eof {
            match &self.token {
                Token::Instruction(instr) => self.write_instr(&instr.clone())?,
                Token::Integer(i) => self.write_u32(*i),
                // TODO: no clone here, perhaps change tokenization to use Rc<str>
                Token::String(s) => self.write_string(&s.clone()),
                Token::Instrs => self.write_instrs()?,
                Token::Custom => self.write_section(0)?,
                Token::Type => self.write_section(1)?,
                Token::Import => self.write_section(2)?,
                Token::Function => self.write_section(3)?,
                Token::Table => self.write_section(4)?,
                Token::Memory => self.write_section(5)?,
                Token::Global => self.write_section(6)?,
                Token::Export => self.write_section(7)?,
                Token::Start => self.write_section(8)?,
                Token::Element => self.write_section(9)?,
                Token::Code => self.write_section(10)?,
                Token::Data => self.write_section(11)?,
                Token::DataCount => self.write_section(12)?,
                Token::I32 => self.write_byte(0x7f),
                Token::I64 => self.write_byte(0x7e),
                Token::F32 => self.write_byte(0x7d),
                Token::F64 => self.write_byte(0x7c),
                Token::Funcref => self.write_byte(0x70),
                Token::Externref => self.write_byte(0x6f),
                Token::ExternFunc => self.write_byte(0x00),
                Token::ExternTable => self.write_byte(0x01),
                Token::ExternMem => self.write_byte(0x02),
                Token::ExternGlobal => self.write_byte(0x03),
                Token::FuncForm => self.write_byte(0x60),
                Token::Comma => {
                    self.advance()?;
                    let Some(State::Array { ref mut items, .. }) = self.states.last_mut() else {
                        return Err(self.error(WriteErrorKind::UnexpectedToken(Token::Comma)));
                    };
                    // This is necessary because the actual number of items is always items + 1
                    // Since trailing commas are optional, there needs to be some way to count
                    // the correct number in both scenarios. The `+ 1` accounts for the comma
                    // skipped on this line. With no trailing comma, the `+ 1` accounts for the
                    // last item they did not use a comma for.
                    if self.token != Token::Rbracket {
                        *items += 1;
                    }
                    continue;
                }
                Token::Lbracket => {
                    self.states.push(State::Array {
                        contents: vec![],
                        items: 0,
                    });
                }
                Token::Rbracket => match self.states.pop() {
                    Some(State::Array { contents, items }) => {
                        if contents.is_empty() {
                            self.write_u32(0);
                        } else {
                            self.write_u32(items + 1);
                        }
                        self.write_bytes(&contents);
                    }
                    _ => {
                        return Err(self.error_with_help(
                            WriteErrorKind::UnexpectedToken(Token::Rbracket),
                            "perhaps you meant to start a vector earlier?",
                        ))
                    }
                },
                Token::Lbrace => {
                    return Err(self.error(WriteErrorKind::UnexpectedToken(Token::Lbrace)))
                }
                Token::Rbrace => {
                    match self.states.pop() {
                        Some(State::Section(buf) | State::Instructions(buf)) => {
                            let len = buf.len() as u32;
                            self.write_u32(len);
                            self.write_bytes(&buf);
                        }
                        _ => return Err(self.error_with_help(
                            WriteErrorKind::UnexpectedToken(Token::Rbrace),
                            "perhaps you meant to start a section or instruction sequence earlier?",
                        )),
                    }
                }
                Token::Magic => self.write_u32_raw(MAGIC),
                Token::Version => self.write_u32_raw(1),
                Token::True => self.write_byte(1),
                Token::False => self.write_byte(0),
                Token::Byte(byte) => self.write_byte(*byte),
                Token::Eof => unreachable!(),
            }

            self.advance()?;
        }

        Ok(self.out)
    }

    fn write_section(&mut self, id: u8) -> Result<(), WriteError> {
        self.write_byte(id);
        self.advance()?;
        if self.token != Token::Lbrace {
            return Err(self.error(WriteErrorKind::ExpectedToken(Token::Lbrace)));
        }
        self.states.push(State::Section(vec![]));
        Ok(())
    }

    fn write_instrs(&mut self) -> Result<(), WriteError> {
        self.advance()?;
        if self.token != Token::Lbrace {
            return Err(self.error(WriteErrorKind::ExpectedToken(Token::Lbrace)));
        }
        self.states.push(State::Instructions(vec![]));
        Ok(())
    }

    fn write_byte(&mut self, b: u8) {
        match self.states.last_mut() {
            Some(State::Section(buf)) | Some(State::Instructions(buf)) => {
                buf.push(b);
            }
            Some(State::Array { contents, .. }) => contents.push(b),
            None => self.out.push(b),
        }
    }

    fn write_u32(&mut self, b: u32) {
        match self.states.last_mut() {
            Some(
                State::Section(buf) | State::Instructions(buf) | State::Array { contents: buf, .. },
            ) => wasm_leb128::write_u32_leb128(buf, b),
            None => wasm_leb128::write_u32_leb128(&mut self.out, b),
        }
        .expect("write should not fail");
    }

    fn write_bytes(&mut self, bytes: &[u8]) {
        match self.states.last_mut() {
            Some(
                State::Section(buf) | State::Instructions(buf) | State::Array { contents: buf, .. },
            ) => buf.write_all(bytes),
            None => self.out.write_all(bytes),
        }
        .expect("write should not fail")
    }

    fn write_u32_raw(&mut self, b: u32) {
        match self.states.last_mut() {
            Some(State::Section(buf)) | Some(State::Instructions(buf)) => {
                buf.write_u32::<LittleEndian>(b)
            }
            Some(State::Array { contents, .. }) => contents.write_u32::<LittleEndian>(b),
            None => self.out.write_u32::<LittleEndian>(b),
        }
        .expect("write should not fail")
    }

    fn error(&self, kind: WriteErrorKind) -> WriteError {
        WriteError {
            kind,
            span: self.lexer.span(),
            help: None,
        }
    }

    fn error_with_help(
        &self,
        kind: WriteErrorKind,
        help: impl Into<Cow<'static, str>>,
    ) -> WriteError {
        WriteError {
            help: Some(help.into()),
            ..self.error(kind)
        }
    }

    fn advance(&mut self) -> Result<(), WriteError> {
        self.token = self
            .lexer
            .next()
            .unwrap_or(Ok(Token::Eof))
            .map_err(|_| self.error(WriteErrorKind::InvalidToken))?;
        Ok(())
    }

    fn write_string(&mut self, s: &str) {
        self.write_u32(s.len() as u32);
        self.write_bytes(s.as_bytes());
    }

    fn write_instr(&mut self, s: &str) -> Result<(), WriteError> {
        if let Some(idx) = ARITH_PREFIX_INSTRUCTIONS.get(s) {
            // prefix byte
            self.write_byte(0xfc);
            self.write_u32(*idx);
            return Ok(());
        }

        let Some(byte) = INSTRUCTIONS.get(s) else {
            let most_similar = INSTRUCTIONS
                .keys()
                .map(|instr| (strsim::damerau_levenshtein(s, instr), instr))
                .min_by_key(|instr| instr.0)
                .expect("should have more than zero instructions")
                .1;
            return Err(self.error_with_help(
                WriteErrorKind::UnknownInstruction(s.to_owned()),
                format!("perhaps you meant: `{most_similar}`"),
            ));
        };
        self.write_byte(*byte);

        Ok(())
    }
}
