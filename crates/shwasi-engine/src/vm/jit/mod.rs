mod assembler;
mod debug;
mod executable;

use std::{
    cmp::Reverse,
    collections::{BinaryHeap, HashMap},
};

use bitflags::bitflags;
use shwasi_parser::{BlockType, InstrBufferRef, Instruction};
use thiserror::Error;
use tracing::debug;

use crate::{vm::jit::debug::asm_fmt, Instance, ModuleFunc};

use self::assembler::*;
pub use self::executable::*;

#[derive(Debug)]
pub struct Compiler {
    asm: Assembler,
    module: Instance,
    free: FreeMem,
    labels: Vec<Label>,
    to_patch: HashMap<usize, Vec<PatchTarget>>,
    to_unify: HashMap<usize, Vec<UnifyTarget>>,
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
    pub fn new(module: Instance) -> Self {
        Self {
            asm: Assembler::new(),
            module,
            labels: vec![],
            free: FreeMem::default(),
            to_patch: HashMap::new(),
            to_unify: HashMap::new(),
        }
    }

    pub fn compile(mut self, f: &ModuleFunc) -> Result<Executable, CompilationError> {
        debug!("got code to compile: \n{}", f.code);

        self.labels.push(Label {
            init_height: 0,
            arity: f.ty.1.len(),
            result_arity: f.ty.1.len(),
            end: f.code.body.len() - 1,
        });
        self.asm.nop();

        let mut stack: Vec<Operand> = vec![];
        let buf = f.code.body.slice(0..f.code.body.len() - 1);
        self.compile_buf(&mut stack, buf)?;
        self.unify(&mut stack, f.code.body.len() - 1);
        self.patch(f.code.body.len() - 1, self.asm.addr());
        self.labels.pop();

        for (offset, op) in stack.iter().enumerate() {
            self.asm.store(offset as u32, Reg::OutBase, *op);
        }
        if self.free.total_stack_size > 0 {
            // Align the stack size to 16 bytes
            if self.free.total_stack_size % 16 != 0 {
                self.free.total_stack_size = 8;
            }
            // Patch the nop
            self.asm.patch(0, |asm| {
                asm.sub(Reg::Sp, Reg::Sp, self.free.total_stack_size);
            });
            self.asm.add(Reg::Sp, Reg::Sp, self.free.total_stack_size);
        }
        self.asm.ret();

        let code = self.asm.consume();
        // SAFETY: The code is valid, as long as the assembler and compiler are correct. If they
        // aren't, then that's a bug...
        let exec = unsafe { Executable::map(&code).map_err(CompilationError::ExecMapError) }?;

        debug!("compiled into \n{}", asm_fmt(exec.as_bytes()));
        Ok(exec)
    }

    fn compile_buf(
        &mut self,
        stack: &mut Vec<Operand>,
        buf: InstrBufferRef,
    ) -> Result<(), CompilationError> {
        use Instruction as I;

        macro_rules! pop {
            () => {{
                let op = stack.pop().unwrap();
                self.free.release(op);
                op
            }};
        }
        macro_rules! binop {
            ($method:ident or $fold:expr) => {{
                let rhs = pop!();
                let lhs = pop!();
                if let (Operand::Imm64(rhs), Operand::Imm64(lhs)) = (rhs, lhs) {
                    #[allow(clippy::redundant_closure_call)]
                    stack.push(Operand::Imm64($fold(lhs, rhs)));
                } else {
                    self.asm.$method(self.free.current, lhs, rhs);
                    stack.push(self.free.current);
                    self.free.next_free();
                }
            }};
        }

        let mut i = 0;
        while i < buf.len() {
            let instr = buf.get(i).unwrap();
            match instr {
                I::Nop => {}
                I::I32Const(val) => stack.push(Operand::Imm64(val as u64)),
                I::Drop => drop(stack.pop()),
                I::I32Add => binop!(add or |lhs: u64, rhs| lhs.saturating_add(rhs)),
                I::I32Sub => binop!(sub or |lhs: u64, rhs| lhs.wrapping_sub(rhs)),
                I::LocalGet { idx } => {
                    self.asm.load_local(self.free.current, idx);
                    stack.push(self.free.current);
                    self.free.next_free();
                }
                I::LocalSet { idx } => {
                    let op = pop!();
                    self.asm.store_local(idx, op);
                }

                I::Br { depth } => {
                    let label = &self.labels[self.labels.len() - depth as usize - 1];
                    let expect_arity = self.labels.last().unwrap().result_arity;
                    let expect_height = self.labels.last().unwrap().init_height;
                    let end = label.end;
                    stack.drain(label.init_height..stack.len() - label.arity);
                    let branch_stack = stack.clone();
                    debug_assert!(branch_stack.len() - label.init_height == label.arity);
                    let unify_addr = self.asm.addr();
                    self.pad_movs(&branch_stack[label.init_height..]);
                    let patch_addr = self.asm.addr();
                    self.asm.branch(0xdeadbeef);

                    self.set_to_patch(end, PatchTarget::unconditional(patch_addr));
                    self.set_to_unify(
                        end,
                        UnifyTarget {
                            addr: unify_addr,
                            stack: branch_stack,
                        },
                    );
                    if stack.len() - expect_height > expect_arity {
                        stack.truncate(expect_height - expect_arity);
                    }

                    return Ok(());
                }
                I::BrIf { depth } => {
                    let op = stack.pop().unwrap();
                    self.free.release(op);
                    self.asm.cmp(op, 0);

                    let mut branch_stack = stack.clone();
                    let label = &self.labels[self.labels.len() - depth as usize - 1];
                    let end = label.end;
                    branch_stack.drain(label.init_height..branch_stack.len() - label.arity);
                    debug_assert!(branch_stack.len() - label.init_height == label.arity);
                    let unify_addr = self.asm.addr();
                    self.pad_movs(&branch_stack[label.init_height..]);
                    let patch_addr = self.asm.addr();
                    self.asm.branch(0xdeadbeef);

                    self.set_to_patch(end, PatchTarget::conditional(patch_addr, ConditionCode::Ne));
                    self.set_to_unify(
                        end,
                        UnifyTarget {
                            addr: unify_addr,
                            stack: branch_stack,
                        },
                    );
                }
                I::If { block, else_: None } => {
                    debug_assert!(self.param_arity(block.ty) == self.return_arity(block.ty));
                    let op = pop!();
                    self.asm.cmp(op, 0);
                    let branch_addr = self.asm.addr();
                    self.asm.branch(0xdeadbeef);
                    self.labels.push(Label {
                        init_height: stack.len() - self.param_arity(block.ty),
                        arity: self.return_arity(block.ty),
                        result_arity: self.return_arity(block.ty),
                        end: block.end,
                    });
                    let buf = buf.inner().slice(buf.start() + i + 1..block.end);
                    let len = buf.len();
                    self.compile_buf(stack, buf)?;
                    self.unify(stack, block.end);
                    self.patch(block.end, self.asm.addr());
                    let to = self.asm.addr();
                    self.asm.patch(branch_addr, |asm| {
                        asm.branch_if(to as u64, ConditionCode::Eq);
                    });
                    self.labels.pop().unwrap();
                    i += len + 1;
                }
                I::If {
                    block,
                    else_: Some(else_),
                } => {
                    let op = pop!();
                    self.asm.cmp(op, 0);
                    let cond_branch_addr = self.asm.addr();
                    self.asm.branch(0xdeadbeef);
                    let init_height = stack.len() - self.param_arity(block.ty);
                    self.labels.push(Label {
                        init_height,
                        arity: self.return_arity(block.ty),
                        result_arity: self.return_arity(block.ty),
                        end: block.end,
                    });

                    let if_buf = buf.inner().slice(buf.start() + i + 1..else_);
                    let else_buf = buf.inner().slice(else_ + 1..block.end);
                    let total_len = if_buf.len() + else_buf.len() + 1;

                    let mut branch1 = stack.clone();
                    self.compile_buf(&mut branch1, if_buf)?;
                    let if_unify_addr = self.asm.addr();
                    self.pad_movs(&branch1[init_height..]);
                    self.set_to_unify(
                        block.end,
                        UnifyTarget {
                            addr: if_unify_addr,
                            stack: branch1,
                        },
                    );
                    let if_jump_addr = self.asm.addr();
                    self.asm.branch(0xdeadbeef);
                    let to = self.asm.addr();
                    self.asm.patch(cond_branch_addr, |asm| {
                        asm.branch_if(to as u64, ConditionCode::Eq);
                    });
                    self.compile_buf(stack, else_buf)?;

                    self.unify(stack, block.end);
                    self.patch(block.end, self.asm.addr());

                    let to = self.asm.addr();
                    self.asm.patch(if_jump_addr, |asm| {
                        asm.branch(to as u64);
                    });
                    self.labels.pop().unwrap();
                    i += total_len + 1;
                }
                I::Loop(block) | I::Block(block) => {
                    let is_block = matches!(instr, I::Block(_));
                    let init_height = stack.len() - self.param_arity(block.ty);
                    self.labels.push(Label {
                        init_height,
                        arity: if is_block {
                            self.return_arity(block.ty)
                        } else {
                            self.param_arity(block.ty)
                        },
                        result_arity: self.return_arity(block.ty),
                        end: block.end,
                    });
                    let buf = buf.inner().slice(buf.start() + i + 1..block.end);
                    let len = buf.len();
                    let jump_addr = self.asm.addr();
                    self.compile_buf(stack, buf.clone())?;
                    if is_block {
                        self.unify(stack, block.end);
                        self.patch(block.end, self.asm.addr());
                    } else {
                        // Loop branches do not have to be unified. Their results can differ from
                        // their arity.
                        self.to_unify.remove(&block.end);
                        // This happens when the loop has no actual results, which is possible
                        // because branches go to the start of the block, not to the end. So we
                        // fabricate values.
                        if stack.len() - init_height < self.return_arity(block.ty) {
                            stack.extend(
                                std::iter::repeat(Operand::Imm64(0))
                                    .take(self.return_arity(block.ty) - stack.len() - init_height),
                            );
                        }
                        self.patch(block.end, jump_addr);
                    }
                    self.labels.pop().unwrap();
                    i += len + 1;
                }
                I::End => unreachable!("should never reach end instruction!"),
                I::I32Eq => {
                    let rhs = pop!();
                    let lhs = pop!();
                    self.asm.cmp(lhs, rhs);
                    self.asm.cset(self.free.current, ConditionCode::Eq);
                    stack.push(self.free.current);
                    self.free.next_free();
                }
                I::I32Eqz => {
                    let op = pop!();
                    self.asm.cmp(op, 0);
                    self.asm.cset(self.free.current, ConditionCode::Eq);
                    stack.push(self.free.current);
                    self.free.next_free();
                }
                _ => return Err(CompilationError::UnsupportedInstruction(instr)),
            }
            i += 1;
        }

        Ok(())
    }

    fn patch(&mut self, end_idx: usize, to: usize) {
        let Some(patch_list) = self.to_patch.get(&end_idx) else {
            return;
        };

        for to_patch in patch_list {
            self.asm.patch(to_patch.addr, |asm| match to_patch.ty {
                BranchType::Unconditional => asm.branch(to as u64),
                BranchType::Conditional(cond) => asm.branch_if(to as u64, cond),
            });
        }
        self.to_patch.remove(&end_idx);
    }

    fn set_to_patch(&mut self, end: usize, target: PatchTarget) {
        self.to_patch.entry(end).or_default().push(target);
    }

    fn set_to_unify(&mut self, end: usize, target: UnifyTarget) {
        self.to_unify.entry(end).or_default().push(target);
    }

    fn pad_movs(&mut self, ops: &[Operand]) {
        for op in ops {
            match op {
                Operand::Imm64(imm) if *imm > (u32::MAX as u64 + u16::MAX as u64) => {
                    self.asm.nop();
                    self.asm.nop();
                    self.asm.nop();
                    self.asm.nop();
                }
                Operand::Imm64(imm) if *imm > u32::MAX as u64 => {
                    self.asm.nop();
                    self.asm.nop();
                    self.asm.nop();
                }
                Operand::Imm64(imm) if *imm > u16::MAX as u64 => {
                    self.asm.nop();
                    self.asm.nop();
                }
                _ => {
                    self.asm.nop();
                }
            }
        }
    }

    fn unify(&mut self, stack: &mut Vec<Operand>, end_idx: usize) {
        let Some(unify_list) = self.to_unify.get(&end_idx) else {
            return;
        };
        let mut to_mov = vec![];
        for to_unify in unify_list {
            self.asm.patch(to_unify.addr, |asm| {
                debug_assert!(
                    to_unify.stack.len() == stack.len(),
                    "mismatched branching stack heights: trying to merge {:?} into {:?}",
                    &to_unify.stack,
                    &stack
                );
                for (main, branch) in stack.iter_mut().zip(&to_unify.stack) {
                    if main == branch {
                        continue;
                    }
                    if let Operand::Imm64(imm64) = main {
                        let i = *imm64;
                        *main = self.free.current;
                        to_mov.push((self.free.current, i));
                        self.free.next_free();
                    }
                    asm.mov(*main, *branch);
                }
            });
        }
        for (reg, i) in to_mov {
            self.asm.mov(reg, i);
        }

        self.to_unify.remove(&end_idx);
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

#[derive(Debug)]
struct Label {
    init_height: usize,
    arity: usize,
    end: usize,
    result_arity: usize,
}

#[derive(Debug)]
struct PatchTarget {
    addr: usize,
    ty: BranchType,
}

impl PatchTarget {
    fn unconditional(addr: usize) -> Self {
        Self {
            addr,
            ty: BranchType::Unconditional,
        }
    }

    fn conditional(addr: usize, cond: ConditionCode) -> Self {
        Self {
            addr,
            ty: BranchType::Conditional(cond),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum BranchType {
    Unconditional,
    Conditional(ConditionCode),
}

#[derive(Debug)]
struct UnifyTarget {
    addr: usize,
    stack: Vec<Operand>,
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
    total_stack_size: u64,
}

impl Default for FreeMem {
    fn default() -> Self {
        let mut s = Self {
            current: Operand::Reg(Reg::GPR0),
            reg: FreeReg::all(),
            free_heap: BinaryHeap::new(),
            total_stack_size: 0,
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
        if let Operand::Mem64(_, offset) = self.current {
            // times 8 because each mem64 item is 8 bytes
            self.total_stack_size = self.total_stack_size.max(offset * 8);
        }
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
    use shwasi_parser::{Block, BlockType, Code, FuncType, InstrBuffer, ValType};

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
    fn block_branching_if() {
        let code = Code {
            body: InstrBuffer::from_iter([
                Instruction::Block(Block {
                    ty: BlockType::Type(ValType::I32),
                    end: 6,
                }),
                Instruction::I32Const(10),
                Instruction::I32Const(1),
                Instruction::BrIf { depth: 0 },
                Instruction::Drop,
                Instruction::I32Const(20),
                Instruction::End,
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
        assert_eq!([Value::I32(10).untyped()], out);
    }

    #[test]
    fn early_return() {
        let code = Code {
            body: InstrBuffer::from_iter([
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
