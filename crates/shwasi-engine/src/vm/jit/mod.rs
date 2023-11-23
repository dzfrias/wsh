mod assembler;
#[cfg(test)]
mod debug;
mod executable;

use std::{cmp::Reverse, collections::BinaryHeap};

use bitflags::bitflags;
use shwasi_parser::{BlockType, Instruction, Opcode};
use thiserror::Error;

use crate::{Instance, ModuleFunc};

use self::assembler::*;
pub use self::executable::*;

#[derive(Debug)]
pub struct Compiler {
    asm: Assembler,
    module: Instance,
    used: Vec<Operand>,
    free: FreeMem,
    labels: Vec<Label>,
}

#[derive(Debug, Error)]
#[error("compilation error")]
pub enum CompilationError {
    #[error("executable mapping failed: {0}")]
    ExecMapError(ExecMapError),
    #[error("unsupported instruction: `{0}`")]
    UnsupportedInstruction(Instruction),
}

#[derive(Debug)]
struct Label {
    arity: usize,
    stack_height: usize,
    opcode: Opcode,
    to_patch: Vec<(usize, Opcode)>,
    unreachable: bool,
    end_loc: usize,
}

impl Compiler {
    pub fn new(module: Instance) -> Self {
        Self {
            asm: Assembler::new(),
            module,
            used: vec![],
            labels: vec![],
            free: FreeMem::default(),
        }
    }

    pub fn compile(
        mut self,
        ModuleFunc { code, ty, .. }: &ModuleFunc,
    ) -> Result<Executable, CompilationError> {
        use Instruction as I;

        self.labels.push(Label {
            stack_height: 0,
            to_patch: vec![],
            arity: ty.1.len(),
            opcode: Opcode::Call,
            unreachable: false,
            end_loc: code.body.len() - 1,
        });
        let mut total_stack_size = 0;

        /// A macro to generate binary operations.
        ///
        /// The first argument is the method to call on the assembler, and the second argument is a
        /// function that takes two u64s and returns a u64. This function is used to fold constant
        /// immediate operands.
        macro_rules! binop {
            ($method:ident or $fold:expr) => {{
                let rhs = self.used.pop().unwrap();
                let lhs = self.used.pop().unwrap();
                if let (Operand::Imm64(rhs), Operand::Imm64(lhs)) = (rhs, lhs) {
                    #[allow(clippy::redundant_closure_call)]
                    self.push(Operand::Imm64($fold(lhs, rhs)));
                } else {
                    self.free.release(rhs);
                    self.free.release(lhs);
                    self.asm.$method(self.free.current, lhs, rhs);
                    self.push(self.free.current);
                    self.free.next_free();
                }
            }};
        }

        // This nop will be patched to subtract the stack size from the stack pointer later.
        self.asm.nop();
        for (i, instr) in code.body.iter().enumerate() {
            // Unreachable instructions should never be compiled.
            if i != self.labels.last().unwrap().end_loc && self.labels.last().unwrap().unreachable {
                continue;
            }

            match instr {
                I::Nop => {}
                I::LocalGet { idx } => {
                    self.asm.load_local(self.free.current, idx);
                    self.push(self.free.current);
                    self.free.next_free();
                }
                I::I32Const(val) => {
                    self.push(Operand::Imm64(val as u64));
                }
                I::I32Add => binop!(add or |lhs: u64, rhs| lhs.saturating_add(rhs)),
                I::I32Sub => binop!(sub or |lhs: u64, rhs| lhs.saturating_sub(rhs)),
                I::Block(block) => {
                    self.labels.push(Label {
                        to_patch: vec![],
                        arity: self.return_arity(block.ty),
                        stack_height: self.used.len() - self.param_arity(block.ty),
                        opcode: Opcode::Block,
                        unreachable: false,
                        end_loc: block.end,
                    });
                }
                I::LocalSet { idx } => {
                    let reg = self.used.pop().unwrap();
                    self.free.release(reg);
                    self.asm.store_local(idx, reg);
                }
                I::Br { depth } => {
                    for _ in 0..depth {
                        self.labels.pop();
                    }
                    let labelty = self.labels.last().unwrap().opcode;
                    if labelty == Opcode::Call {
                        self.set_early_return();
                    } else {
                        let label = self.labels.last_mut().unwrap();
                        label.unreachable = true;
                        self.used
                            .drain(label.stack_height..self.used.len() - label.arity);
                        label.to_patch.push((self.asm.addr() as usize, Opcode::Br));
                        // This will be patched later, at the `end` instruction.
                        self.asm.branch(0xdeadbeef);
                    }
                }
                I::I32Eq => binop!(eq or |lhs: u64, rhs| (lhs == rhs) as u64),
                I::I32Ne => binop!(ne or |lhs: u64, rhs| (lhs != rhs) as u64),
                I::I32Eqz => {
                    let op = self.used.pop().unwrap();
                    if let Operand::Imm64(val) = op {
                        self.push(Operand::Imm64((val == 0) as u64));
                    } else {
                        self.free.release(op);
                        self.asm.eq(self.free.current, op, 0);
                        self.push(self.free.current);
                        self.free.next_free();
                    }
                }
                I::Return => {
                    self.labels.drain(1..);
                    self.set_early_return();
                }
                I::End => {
                    let label = self.labels.pop().unwrap();
                    let current_addr = self.asm.addr();
                    match label.opcode {
                        Opcode::Block => {
                            for (pos, opcode) in label.to_patch {
                                self.asm.patch(pos, |asm| match opcode {
                                    Opcode::Br => {
                                        asm.branch(current_addr);
                                    }
                                    Opcode::BrIf => todo!(),
                                    Opcode::BrTable => todo!(),
                                    _ => unreachable!(),
                                });
                            }
                        }
                        // Implicit return
                        Opcode::Call => {
                            // TODO: maybe this can be removed?
                            self.used
                                .drain(label.stack_height..self.used.len() - label.arity);
                            // Set return vals
                            for (offset, op) in self.used.iter().enumerate() {
                                self.asm.store(offset as u32, Reg::OutBase, *op);
                            }
                            let addr = self.asm.addr();
                            // This is to patch all the early returns
                            for to_patch in label.to_patch {
                                self.asm.patch(to_patch.0, |asm| {
                                    asm.branch(addr);
                                });
                            }
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
                        }
                        _ => unreachable!(),
                    }
                }
                _ => return Err(CompilationError::UnsupportedInstruction(instr)),
            }
            if let Operand::Mem64(reg, offset) = self.free.current {
                debug_assert!(reg == Reg::Sp);
                // times 8 because each mem64 item is 8 bytes
                total_stack_size = total_stack_size.max(offset * 8);
            }
        }

        let code = self.asm.consume();
        // SAFETY: The code is valid, as long as the assembler and compiler are correct. If they
        // aren't, then that's a bug...
        unsafe { Executable::map(&code).map_err(CompilationError::ExecMapError) }
    }

    fn set_early_return(&mut self) {
        let label = self.labels.last_mut().unwrap();
        label.unreachable = true;
        self.used
            .drain(label.stack_height..self.used.len() - label.arity);
        for (offset, op) in self.used.iter().enumerate() {
            self.asm.store(offset as u32, Reg::OutBase, *op);
        }
        label.to_patch.push((self.asm.addr() as usize, Opcode::Br));
        // This will be patched to go to the end of the function.
        self.asm.branch(0xdeadbeef);
    }

    fn push(&mut self, op: Operand) {
        self.used.push(op);
    }

    #[inline(always)]
    fn return_arity(&self, blockty: BlockType) -> usize {
        match blockty {
            BlockType::Empty => 0,
            BlockType::Type(_) => 1,
            BlockType::FuncType(idx) => self.module.types()[idx as usize].1.len(),
        }
    }

    #[inline(always)]
    fn param_arity(&self, blockty: BlockType) -> usize {
        match blockty {
            BlockType::Empty | BlockType::Type(_) => 0,
            BlockType::FuncType(idx) => self.module.types()[idx as usize].0.len(),
        }
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
    use shwasi_parser::{Block, Code, FuncType, InstrBuffer, ValType};

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
        let compiler = Compiler::new(Instance::default());
        let executable = compiler
            .compile(&ModuleFunc {
                ty: FuncType(vec![ValType::I32], vec![ValType::I32]),
                code,
                inst: Instance::default(),
            })
            .unwrap();
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
        let compiler = Compiler::new(Instance::default());
        let executable = compiler
            .compile(&ModuleFunc {
                ty: FuncType(vec![ValType::I32, ValType::I32], vec![ValType::I32]),
                code,
                inst: Instance::default(),
            })
            .unwrap();
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
        let compiler = Compiler::new(Instance::default());
        let executable = compiler
            .compile(&ModuleFunc {
                ty: FuncType(vec![], vec![ValType::I32, ValType::I32]),
                code,
                inst: Instance::default(),
            })
            .unwrap();
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
        let compiler = Compiler::new(Instance::default());
        let executable = compiler
            .compile(&ModuleFunc {
                ty: FuncType(vec![], vec![ValType::I32]),
                code,
                inst: Instance::default(),
            })
            .unwrap();
        asm_assert_eq!(
            &[0xd503201f, 0xd2800158, 0xf9000038, 0xd65f03c0],
            executable.as_bytes()
        );
    }

    #[test]
    fn block_branching() {
        let code = Code {
            body: InstrBuffer::from_iter([
                Instruction::Block(Block {
                    ty: BlockType::Type(ValType::I32),
                    end: 4,
                }),
                Instruction::I32Const(10),
                Instruction::Br { depth: 0 },
                Instruction::I32Const(20),
                Instruction::End,
                Instruction::LocalGet { idx: 0 },
                Instruction::I32Add,
                Instruction::End,
            ]),
            locals: vec![],
        };
        let compiler = Compiler::new(Instance::default());
        let executable = compiler
            .compile(&ModuleFunc {
                ty: FuncType(vec![ValType::I32], vec![ValType::I32]),
                code,
                inst: Instance::default(),
            })
            .unwrap();
        let mut locals = [Value::I32(32).untyped()];
        let mut out = [ValueUntyped::default()];
        executable.run_with(&mut locals, &mut out);
        assert_eq!([Value::I32(42).untyped()], out);
    }

    #[test]
    fn early_return() {
        let code = Code {
            body: InstrBuffer::from_iter([
                Instruction::I32Const(10),
                Instruction::I32Const(32),
                Instruction::Br { depth: 0 },
                Instruction::I32Const(20),
                Instruction::End,
            ]),
            locals: vec![],
        };
        let compiler = Compiler::new(Instance::default());
        let executable = compiler
            .compile(&ModuleFunc {
                ty: FuncType(vec![], vec![ValType::I32]),
                code,
                inst: Instance::default(),
            })
            .unwrap();
        let mut locals = [];
        let mut out = [ValueUntyped::default()];
        executable.run_with(&mut locals, &mut out);
        assert_eq!([Value::I32(32).untyped()], out);
    }

    #[test]
    fn early_return_with_used_stack_space() {
        let code = Code {
            body: InstrBuffer::from_iter([
                Instruction::LocalGet { idx: 0 },
                Instruction::LocalGet { idx: 1 },
                Instruction::LocalGet { idx: 2 },
                Instruction::LocalGet { idx: 3 },
                Instruction::LocalGet { idx: 4 },
                Instruction::Br { depth: 0 },
                Instruction::I32Const(1),
                Instruction::I32Const(1),
                Instruction::I32Const(1),
                Instruction::I32Const(1),
                Instruction::I32Const(1),
                Instruction::End,
            ]),
            locals: vec![],
        };
        let compiler = Compiler::new(Instance::default());
        let executable = compiler
            .compile(&ModuleFunc {
                ty: FuncType(vec![ValType::I32; 5], vec![ValType::I32; 5]),
                code,
                inst: Instance::default(),
            })
            .unwrap();
        let mut locals = [Value::I32(10).untyped(); 5];
        let mut out = [ValueUntyped::default(); 5];
        executable.run_with(&mut locals, &mut out);
        assert_eq!([Value::I32(10).untyped(); 5], out);
    }
}
