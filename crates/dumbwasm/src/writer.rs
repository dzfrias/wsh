use std::io::Write;

use byteorder::{LittleEndian, WriteBytesExt};
use logos::Lexer;

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
        let mut w = Self {
            lexer,
            out: vec![],
            states: vec![],
            token: Token::Rbrace,
        };
        w.advance();
        w
    }

    pub fn write(mut self) -> Vec<u8> {
        while self.token != Token::Eof {
            match &self.token {
                Token::Instruction(instr) => self.write_instr(&instr.clone()),
                Token::Integer(i) => self.write_u32(*i),
                // TODO: no clone here, perhaps change tokenization to use Rc<str>
                Token::String(s) => self.write_string(&s.clone()),
                Token::Instrs => self.write_instrs(),
                Token::Custom => self.write_section(0),
                Token::Type => self.write_section(1),
                Token::Import => self.write_section(2),
                Token::Function => self.write_section(3),
                Token::Table => self.write_section(4),
                Token::Memory => self.write_section(5),
                Token::Global => self.write_section(6),
                Token::Export => self.write_section(7),
                Token::Start => self.write_section(8),
                Token::Element => self.write_section(9),
                Token::Code => self.write_section(10),
                Token::Data => self.write_section(11),
                Token::DataCount => self.write_section(12),
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
                    self.advance();
                    let Some(State::Array { ref mut items, .. }) = self.states.last_mut() else {
                        todo!("error");
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
                    _ => todo!("error"),
                },
                Token::Lbrace => todo!("error"),
                Token::Rbrace => match self.states.pop() {
                    Some(State::Section(buf) | State::Instructions(buf)) => {
                        let len = buf.len() as u32;
                        self.write_u32(len);
                        self.write_bytes(&buf);
                    }
                    _ => todo!("error"),
                },
                Token::Magic => self.write_u32_raw(MAGIC),
                Token::Version => self.write_u32_raw(1),
                Token::True => self.write_byte(1),
                Token::False => self.write_byte(0),
                Token::Byte(byte) => self.write_byte(*byte),
                Token::Eof => unreachable!(),
            }

            self.advance();
        }

        self.out
    }

    fn write_section(&mut self, id: u8) {
        self.write_byte(id);
        self.advance();
        // TODO: error handling
        assert_eq!(Token::Lbrace, self.token);
        self.states.push(State::Section(vec![]));
    }

    fn write_instrs(&mut self) {
        self.advance();
        // TODO: error handling
        assert_eq!(Token::Lbrace, self.token);
        self.states.push(State::Instructions(vec![]));
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

    fn write_string(&mut self, s: &str) {
        self.write_u32(s.len() as u32);
        self.write_bytes(s.as_bytes());
    }

    fn advance(&mut self) {
        // TODO: error handling
        self.token = self.lexer.next().unwrap_or(Ok(Token::Eof)).unwrap();
    }

    fn write_instr(&mut self, s: &str) {
        // for instructions that have a prefix byte (i.e. 0xfc)
        'prefix: {
            let idx = match s {
                "i32.trunc_sat_f32_s" => 0,
                "i32.trunc_sat_f32_u" => 1,
                "i32.trunc_sat_f64_s" => 2,
                "i32.trunc_sat_f64_u" => 3,
                "i64.trunc_sat_f32_s" => 4,
                "i64.trunc_sat_f32_u" => 5,
                "i64.trunc_sat_f64_s" => 6,
                "i64.trunc_sat_f64_u" => 7,
                "memory.init" => 8,
                "data.drop" => 9,
                "memory.copy" => 10,
                "memory.fill" => 11,
                "table.init" => 12,
                "elem.drop" => 13,
                "table.copy" => 14,
                "table.grow" => 15,
                "table.size" => 16,
                "table.fill" => 17,
                _ => break 'prefix,
            };

            self.write_byte(0xfc);
            self.write_u32(idx);
            return;
        }

        let byte = match s {
            "unreachable" => 0x00,
            "nop" => 0x01,
            "block" => 0x02,
            "loop" => 0x03,
            "if" => 0x04,
            "else" => 0x05,
            "end" => 0x0b,
            "br" => 0x0c,
            "br_if" => 0x0d,
            "br_table" => 0x0e,
            "return" => 0x0f,
            "call" => 0x10,
            "drop" => 0x1a,
            "select" => 0x1b,
            "local.get" => 0x20,
            "local.set" => 0x21,
            "local.tee" => 0x22,
            "global.get" => 0x23,
            "global.set" => 0x24,
            "table.get" => 0x25,
            "table.set" => 0x26,
            "i32.load" => 0x28,
            "i64.load" => 0x29,
            "f32.load" => 0x2a,
            "f64.load" => 0x2b,
            "i32.load8_s" => 0x2c,
            "i32.load8_u" => 0x2d,
            "i32.load16_s" => 0x2e,
            "i32.load16_u" => 0x2f,
            "i64.load8_s" => 0x30,
            "i64.load8_u" => 0x31,
            "i64.load16_s" => 0x32,
            "i64.load16_u" => 0x33,
            "i64.load32_s" => 0x34,
            "i64.load32_u" => 0x35,
            "i32.store" => 0x36,
            "i64.store" => 0x37,
            "f32.store" => 0x38,
            "f64.store" => 0x39,
            "i32.store8" => 0x3a,
            "i32.store16" => 0x3b,
            "i64.store8" => 0x3c,
            "i64.store16" => 0x3d,
            "i64.store32" => 0x3e,
            "i32.const" => 0x41,
            "i64.const" => 0x42,
            "f32.const" => 0x43,
            "f64.const" => 0x44,
            "i32.eqz" => 0x45,
            "i32.eq" => 0x46,
            "i32.ne" => 0x47,
            "i32.lt_s" => 0x48,
            "i32.lt_u" => 0x49,
            "i32.gt_s" => 0x4a,
            "i32.gt_u" => 0x4b,
            "i32.le_s" => 0x4c,
            "i32.le_u" => 0x4d,
            "i32.ge_s" => 0x4e,
            "i32.ge_u" => 0x4f,
            "i64.eqz" => 0x50,
            "i64.eq" => 0x51,
            "i64.ne" => 0x52,
            "i64.lt_s" => 0x53,
            "i64.lt_u" => 0x54,
            "i64.gt_s" => 0x55,
            "i64.gt_u" => 0x56,
            "i64.le_s" => 0x57,
            "i64.le_u" => 0x58,
            "i64.ge_s" => 0x59,
            "i64.ge_u" => 0x5a,
            "f32.eq" => 0x5b,
            "f32.ne" => 0x5c,
            "f32.lt" => 0x5d,
            "f32.gt" => 0x5e,
            "f32.le" => 0x5f,
            "f32.ge" => 0x60,
            "f64.eq" => 0x61,
            "f64.ne" => 0x62,
            "f64.lt" => 0x63,
            "f64.gt" => 0x64,
            "f64.le" => 0x65,
            "f64.ge" => 0x66,
            "i32.clz" => 0x67,
            "i32.ctz" => 0x68,
            "i32.popcnt" => 0x69,
            "i32.add" => 0x6a,
            "i32.sub" => 0x6b,
            "i32.mul" => 0x6c,
            "i32.div_s" => 0x6d,
            "i32.div_u" => 0x6e,
            "i32.rem_s" => 0x6f,
            "i32.rem_u" => 0x70,
            "i32.and" => 0x71,
            "i32.or" => 0x72,
            "i32.xor" => 0x73,
            "i32.shl" => 0x74,
            "i32.shr_s" => 0x75,
            "i32.shr_u" => 0x76,
            "i32.rotl" => 0x77,
            "i32.rotr" => 0x78,
            "i64.clz" => 0x79,
            "i64.ctz" => 0x7a,
            "i64.popcnt" => 0x7b,
            "i64.add" => 0x7c,
            "i64.sub" => 0x7d,
            "i64.mul" => 0x7e,
            "i64.div_s" => 0x7f,
            "i64.div_u" => 0x80,
            "i64.rem_s" => 0x81,
            "i64.rem_u" => 0x82,
            "i64.and" => 0x83,
            "i64.or" => 0x84,
            "i64.xor" => 0x85,
            "i64.shl" => 0x86,
            "i64.shr_s" => 0x87,
            "i64.shr_u" => 0x88,
            "i64.rotl" => 0x89,
            "i64.rotr" => 0x8a,
            "f32.abs" => 0x8b,
            "f32.neg" => 0x8c,
            "f32.ceil" => 0x8d,
            "f32.floor" => 0x8e,
            "f32.trunc" => 0x8f,
            "f32.nearest" => 0x90,
            "f32.sqrt" => 0x91,
            "f32.add" => 0x92,
            "f32.sub" => 0x93,
            "f32.mul" => 0x94,
            "f32.div" => 0x95,
            "f32.min" => 0x96,
            "f32.max" => 0x97,
            "f32.copysign" => 0x98,
            "f64.abs" => 0x99,
            "f64.neg" => 0x9a,
            "f64.ceil" => 0x9b,
            "f64.floor" => 0x9c,
            "f64.trunc" => 0x9d,
            "f64.nearest" => 0x9e,
            "f64.sqrt" => 0x9f,
            "f64.add" => 0xa0,
            "f64.sub" => 0xa1,
            "f64.mul" => 0xa2,
            "f64.div" => 0xa3,
            "f64.min" => 0xa4,
            "f64.max" => 0xa5,
            "f64.copysign" => 0xa6,
            "i32.wrap_i64" => 0xa7,
            "i32.trunc_f32_s" => 0xa8,
            "i32.trunc_f32_u" => 0xa9,
            "i32.trunc_f64_s" => 0xaa,
            "i32.trunc_f64_u" => 0xab,
            "i64.extend_i32_s" => 0xac,
            "i64.extend_i32_u" => 0xad,
            "i64.trunc_f32_s" => 0xae,
            "i64.trunc_f32_u" => 0xaf,
            "i64.trunc_f64_s" => 0xb0,
            "i64.trunc_f64_u" => 0xb1,
            "f32.convert_i32_s" => 0xb2,
            "f32.convert_i32_u" => 0xb3,
            "f32.convert_i64_s" => 0xb4,
            "f32.convert_i64_u" => 0xb5,
            "f32.demote_f64" => 0xb6,
            "f64.convert_i32_s" => 0xb7,
            "f64.convert_i32_u" => 0xb8,
            "f64.convert_i64_s" => 0xb9,
            "f64.convert_i64_u" => 0xba,
            "f64.promote_f32" => 0xbb,
            "i32.reinterpret_f32" => 0xbc,
            "i64.reinterpret_f64" => 0xbd,
            "f32.reinterpret_i32" => 0xbe,
            "f64.reinterpret_i64" => 0xbf,
            "ref.null" => 0xd0,
            "ref.is_null" => 0xd1,
            "ref.func" => 0xd2,
            "memory.size" => 0x3f,
            "memory.grow" => 0x40,
            instr => todo!("error, got instr {instr}"),
        };
        self.write_byte(byte);
    }
}