#![allow(dead_code)]

mod opcode;

use std::fmt;

use num_enum::TryFromPrimitive;

use self::opcode::OperandsType;
pub use self::opcode::{is_prefix_byte, Opcode};
use crate::{RefType, ValType};

#[derive(Debug, Clone, Copy, Default, Hash)]
pub struct Instr(u32);

#[derive(Debug, Clone, Default)]
pub struct InstrBuffer {
    infos: Vec<InstrInfo>,
    mem_args: Vec<MemArg>,
    block_types: Vec<BlockType>,
    br_tables: Vec<BrTable>,
    eight_bytes: Vec<(u32, u32)>,
    u64s: Vec<u64>,
}

#[derive(Debug, Clone, Default)]
pub struct MemArg {
    pub offset: u32,
    pub align: u32,
}

#[derive(Debug, Clone)]
pub struct BrTable {
    pub depths: Vec<u32>,
    pub default_depth: u32,
}

#[derive(Debug)]
pub struct Instrs {
    cap: usize,
    current: u32,
}

#[derive(Debug, Clone)]
struct InstrInfo {
    opcode: Opcode,
    payload: u32,
}

#[derive(Debug)]
pub enum Operands {
    MemArg(MemArg),
    FourBytes(u32),
    EightBytes(u32, u32),
    BlockType(BlockType),
    RefType(RefType),
    BrTable(BrTable),
    U64(u64),
    Empty,
}

#[derive(Debug, Clone)]
pub enum BlockType {
    Empty,
    Type(ValType),
    FuncType(u32),
}

impl InstrBuffer {
    pub fn new() -> Self {
        Self {
            infos: vec![],
            mem_args: vec![],
            block_types: vec![],
            eight_bytes: vec![],
            br_tables: vec![],
            u64s: vec![],
        }
    }

    pub fn opcode(&self, instr: Instr) -> Opcode {
        self.get_info(instr).opcode
    }

    pub fn add_instr(&mut self, opcode: Opcode, payload: Operands) {
        if payload.ty() != opcode.operand_type() {
            panic!("opcode operands do not match payload");
        }

        let encoding = match payload {
            Operands::MemArg(mem_arg) => {
                self.mem_args.push(mem_arg);
                self.mem_args.len() as u32 - 1
            }
            Operands::FourBytes(bytes) => bytes,
            Operands::EightBytes(b1, b2) => {
                self.eight_bytes.push((b1, b2));
                self.eight_bytes.len() as u32 - 1
            }
            Operands::BlockType(block_type) => {
                self.block_types.push(block_type);
                self.block_types.len() as u32 - 1
            }
            Operands::RefType(ref_type) => {
                let int_repr = ref_type as u8;
                int_repr as u32
            }
            Operands::BrTable(table) => {
                self.br_tables.push(table);
                self.br_tables.len() as u32 - 1
            }
            Operands::U64(u64) => {
                self.u64s.push(u64);
                self.u64s.len() as u32 - 1
            }
            Operands::Empty => 0,
        };

        self.infos.push(InstrInfo {
            opcode,
            payload: encoding,
        });
    }

    pub fn four_byte_operand(&self, instr: Instr) -> u32 {
        match self.operands(instr) {
            Operands::FourBytes(bytes) => bytes,
            _ => panic!("should not be called when instr does not have a four byte operand"),
        }
    }

    pub fn u64_operand(&self, instr: Instr) -> u64 {
        match self.operands(instr) {
            Operands::U64(u64) => u64,
            _ => panic!("should not be called when instr does not have a u64 operand"),
        }
    }

    pub fn eight_byte_operand(&self, instr: Instr) -> (u32, u32) {
        match self.operands(instr) {
            Operands::EightBytes(b1, b2) => (b1, b2),
            _ => panic!("should not be called when instr does not have a two four byte operands"),
        }
    }

    pub fn mem_arg(&self, instr: Instr) -> MemArg {
        match self.operands(instr) {
            Operands::MemArg(mem_arg) => mem_arg,
            _ => panic!("should not be called when instr does not have a memarg operand"),
        }
    }

    pub fn block_type(&self, instr: Instr) -> BlockType {
        match self.operands(instr) {
            Operands::BlockType(block_type) => block_type,
            _ => panic!("should not be called when instr does not have a blocktype operand"),
        }
    }

    pub fn br_table(&self, instr: Instr) -> BrTable {
        match self.operands(instr) {
            Operands::BrTable(br_table) => br_table,
            _ => panic!("should not be called when instr does not have a brtable operand"),
        }
    }

    pub fn operands(&self, instr: Instr) -> Operands {
        let InstrInfo { opcode, payload } = self.get_info(instr);
        let payload = *payload;
        let payload_type = opcode.operand_type();
        match payload_type {
            OperandsType::MemArg => Operands::MemArg(self.mem_args[payload as usize].clone()),
            OperandsType::FourBytes => Operands::FourBytes(payload),
            OperandsType::EightBytes => {
                let (b1, b2) = self.eight_bytes[payload as usize];
                Operands::EightBytes(b1, b2)
            }
            OperandsType::BlockType => {
                Operands::BlockType(self.block_types[payload as usize].clone())
            }
            OperandsType::Empty => Operands::Empty,
            OperandsType::RefType => {
                let int_repr = payload as u8;
                Operands::RefType(RefType::try_from_primitive(int_repr).unwrap())
            }
            OperandsType::BrTable => Operands::BrTable(self.br_tables[payload as usize].clone()),
            OperandsType::U64 => Operands::U64(self.u64s[payload as usize]),
        }
    }

    pub fn len(&self) -> usize {
        self.infos.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn instrs(&self) -> Instrs {
        Instrs {
            cap: self.len(),
            current: 0,
        }
    }

    pub fn get(&self, n: usize) -> Option<Instr> {
        if n >= self.len() {
            return None;
        }

        Some(Instr(n as u32))
    }

    fn get_info(&self, instr: Instr) -> &InstrInfo {
        &self.infos[instr.0 as usize]
    }
}

impl Operands {
    fn ty(&self) -> OperandsType {
        match self {
            Operands::MemArg(_) => OperandsType::MemArg,
            Operands::FourBytes(_) => OperandsType::FourBytes,
            Operands::EightBytes(_, _) => OperandsType::EightBytes,
            Operands::BlockType(_) => OperandsType::BlockType,
            Operands::Empty => OperandsType::Empty,
            Operands::RefType(_) => OperandsType::RefType,
            Operands::U64(_) => OperandsType::U64,
            Operands::BrTable(_) => OperandsType::BrTable,
        }
    }
}

impl Iterator for Instrs {
    type Item = Instr;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cap == self.current as usize {
            return None;
        }

        let instr = Instr(self.current);
        self.current += 1;
        Some(instr)
    }
}

impl fmt::Display for InstrBuffer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut indent = String::new();
        for instr in self.instrs() {
            let opcode = self.opcode(instr);
            let operands = self.operands(instr);

            if opcode == Opcode::End {
                indent.truncate(indent.len().saturating_sub(2))
            }

            writeln!(f, "{indent}{opcode:?} {operands}")?;

            if matches!(opcode, Opcode::If | Opcode::Block | Opcode::Loop) {
                indent.push_str("  ");
            }
        }

        Ok(())
    }
}

impl fmt::Display for Operands {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operands::MemArg(mem_arg) => {
                write!(f, "{} (align) {} (offset)", mem_arg.align, mem_arg.offset)
            }
            Operands::FourBytes(bytes) => write!(f, "{bytes}"),
            Operands::EightBytes(b1, b2) => write!(f, "{b1} {b2}"),
            Operands::BlockType(blockty) => match blockty {
                BlockType::Empty => Ok(()),
                BlockType::Type(val_type) => {
                    let write = match val_type {
                        ValType::I32 => "i32",
                        ValType::I64 => "i64",
                        ValType::F32 => "f32",
                        ValType::F64 => "f64",
                        ValType::Func => "funcref",
                        ValType::Extern => "externref",
                    };
                    write!(f, "{write}")
                }
                BlockType::FuncType(functype) => write!(f, "{functype} (functype)"),
            },
            Operands::RefType(reftype) => {
                let write = match reftype {
                    RefType::Func => "funcref",
                    RefType::Extern => "externref",
                };
                write!(f, "{write}")
            }
            Operands::BrTable(br_table) => {
                write!(f, "{} (depths)", br_table.depths.len())?;
                for depth in &br_table.depths {
                    write!(f, " {depth}")?;
                }
                write!(f, "{}", br_table.default_depth)?;
                Ok(())
            }
            Operands::U64(u64) => write!(f, "{u64}"),
            Operands::Empty => Ok(()),
        }
    }
}
