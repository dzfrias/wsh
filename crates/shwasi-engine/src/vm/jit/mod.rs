mod assembler;
#[cfg(test)]
mod debug;
mod executable;

use std::{cmp::Reverse, collections::BinaryHeap};

use bitflags::bitflags;
use shwasi_parser::{Code, Instruction};
use thiserror::Error;

use self::assembler::*;
pub use self::executable::*;

#[derive(Debug)]
pub struct Compiler {
    asm: Assembler,
}

#[derive(Debug, Error)]
#[error("compilation error")]
pub enum CompilationError {
    #[error("executable mapping failed: {0}")]
    ExecMapError(ExecMapError),
    #[error("unsupported instruction: `{0}`")]
    UnsupportedInstruction(Instruction),
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            asm: Assembler::new(),
        }
    }

    pub fn compile(mut self, code: &Code) -> Result<Executable, CompilationError> {
        use Instruction as I;

        // `used` holds Memory and registers that are NOT free to use
        let mut used = Vec::new();
        let mut free = FreeMem::default();
        let mut total_stack_size = 0;

        macro_rules! binop {
            ($method:ident or $fold:ident) => {{
                let rhs = used.pop().unwrap();
                let lhs = used.pop().unwrap();
                if let (Operand::Imm64(rhs), Operand::Imm64(lhs)) = (rhs, lhs) {
                    used.push(Operand::Imm64(lhs.$fold(rhs)));
                } else {
                    free.release(rhs);
                    free.release(lhs);
                    self.asm.$method(free.current, lhs, rhs);
                    used.push(free.current);
                    free.next_free();
                }
            }};
        }

        // This nop will be patched to subtract the stack size from the stack pointer later.
        self.asm.nop();
        for instr in &code.body {
            match instr {
                I::Nop => {}
                I::LocalGet { idx } => {
                    self.asm.load_local(free.current, idx);
                    used.push(free.current);
                    free.next_free();
                }
                I::I32Const(val) => {
                    used.push(Operand::Imm64(val as u64));
                }
                I::I32Add => binop!(add or saturating_add),
                I::I32Sub => binop!(sub or saturating_sub),
                I::LocalSet { idx } => {
                    let reg = used.pop().unwrap();
                    free.release(reg);
                    self.asm.store_local(idx, reg);
                }
                I::End => {
                    for (offset, op) in used.iter().enumerate() {
                        self.asm.store(offset as u32, Reg::OutBase, *op);
                    }
                    break;
                }
                _ => return Err(CompilationError::UnsupportedInstruction(instr)),
            }
            if let Operand::Mem64(reg, offset) = free.current {
                debug_assert!(reg == Reg::Sp);
                total_stack_size = total_stack_size.max(offset);
            }
        }
        // times 8 because each mem64 item is 8 bytes
        total_stack_size *= 8;
        if total_stack_size > 0 {
            // Align the stack size to 16 bytes
            if total_stack_size % 16 != 0 {
                total_stack_size += 8;
            }
            // Patch the nop
            self.asm.patch(0, |asm| {
                asm.sub(Reg::Sp, Reg::Sp, total_stack_size);
            });
            self.asm.add(Reg::Sp, Reg::Sp, total_stack_size);
        }
        self.asm.ret();

        // SAFETY: The code is valid, as long as the assembler and compiler are correct. If they
        // aren't, then that's a bug...
        unsafe { Executable::map(&self.asm.consume()).map_err(CompilationError::ExecMapError) }
    }
}

/// A data structure that keeps track of free stack memory and free registers.
///
/// This is used to get the most optimal area to put the next operand.
#[derive(Debug)]
struct FreeMem {
    /// The current, most optimal operand that is free to use.
    current: Operand,
    /// The registers that are free to use. The bits are set for the registers that are free.
    reg: FreeReg,
    /// A min-heap of free memory offsets. The offsets are in bytes. This is used to get the
    /// smallest offset into the stack.
    free_heap: BinaryHeap<Reverse<u64>>,
}

impl Default for FreeMem {
    fn default() -> Self {
        let mut s = Self {
            current: Operand::Reg(Reg::GPR0),
            reg: FreeReg::all(),
            free_heap: BinaryHeap::new(),
        };
        s.next_free();
        s
    }
}

bitflags! {
    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    struct FreeReg: u8 {
        const GPR0 = 0b0000_0001;
        const GPR1 = 0b0000_0010;
        const GPR2 = 0b0000_0100;
    }
}

impl FreeMem {
    fn next_free(&mut self) {
        let next = if let Some(reg) = self.reg.iter().next() {
            self.reg.remove(reg);
            match reg {
                FreeReg::GPR0 => Operand::Reg(Reg::GPR0),
                FreeReg::GPR1 => Operand::Reg(Reg::GPR1),
                FreeReg::GPR2 => Operand::Reg(Reg::GPR2),
                _ => unreachable!(),
            }
        } else if let Some(offset) = self.free_heap.pop() {
            self.free_heap.push(Reverse(offset.0 + 8));
            Operand::Mem64(Reg::Sp, offset.0)
        } else {
            self.free_heap.push(Reverse(8));
            Operand::Mem64(Reg::Sp, 0)
        };
        self.current = next;
    }

    fn release(&mut self, op: Operand) {
        match op {
            Operand::Reg(reg) => {
                self.reg.insert(match reg {
                    Reg::GPR0 => FreeReg::GPR0,
                    Reg::GPR1 => FreeReg::GPR1,
                    Reg::GPR2 => FreeReg::GPR2,
                    reg => panic!("cannot free up special register: {reg:?}"),
                });
            }
            Operand::Mem64(_, offset) => {
                self.free_heap.push(Reverse(offset));
            }
            Operand::Imm64(_) => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use shwasi_parser::InstrBuffer;

    use super::*;
    use crate::{value::ValueUntyped, Value};
    use debug::asm_assert_eq;

    #[test]
    fn simple() {
        let code = Code {
            body: InstrBuffer::from_iter([
                Instruction::I32Const(42),
                Instruction::LocalSet { idx: 0 },
                Instruction::LocalGet { idx: 0 },
                Instruction::End,
            ]),
            locals: vec![],
        };
        let compiler = Compiler::new();
        let executable = compiler.compile(&code).unwrap();
        let mut locals = [ValueUntyped::default()];
        let mut out = [ValueUntyped::default()];
        executable.run_with(&mut locals, &mut out);
        assert_eq!(out[0].as_i32(), 42);
    }

    #[test]
    fn multi_params() {
        let code = Code {
            body: InstrBuffer::from_iter([
                Instruction::LocalGet { idx: 0 },
                Instruction::LocalGet { idx: 1 },
                Instruction::I32Add,
                Instruction::End,
            ]),
            locals: vec![],
        };
        let compiler = Compiler::new();
        let executable = compiler.compile(&code).unwrap();
        let mut locals = [Value::I32(10).untyped(), Value::I32(32).untyped()];
        let mut out = [ValueUntyped::default()];
        executable.run_with(&mut locals, &mut out);
        assert_eq!(out[0].as_i32(), 42);
    }

    #[test]
    fn multi_results() {
        let code = Code {
            body: InstrBuffer::from_iter([
                Instruction::I32Const(32),
                Instruction::I32Const(10),
                Instruction::I32Add,
                Instruction::I32Const(32),
                Instruction::End,
            ]),
            locals: vec![],
        };
        let compiler = Compiler::new();
        let executable = compiler.compile(&code).unwrap();
        let mut locals = [];
        let mut out = [ValueUntyped::default(); 2];
        executable.run_with(&mut locals, &mut out);
        assert_eq!([Value::I32(42).untyped(), Value::I32(32).untyped()], out);
    }

    #[test]
    fn constant_folding() {
        let code = Code {
            body: InstrBuffer::from_iter([
                Instruction::I32Const(32),
                Instruction::I32Const(10),
                Instruction::I32Add,
                Instruction::I32Const(32),
                Instruction::I32Sub,
                Instruction::End,
            ]),
            locals: vec![],
        };
        let compiler = Compiler::new();
        let executable = compiler.compile(&code).unwrap();
        asm_assert_eq!(
            &[0xd503201f, 0xd2800158, 0xf9000038, 0xd65f03c0],
            executable.as_bytes()
        );
    }
}
