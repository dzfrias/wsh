mod assembler;
mod debug;
mod executable;

use std::collections::HashMap;

use bitflags::bitflags;
use bitvec::vec::BitVec;
use shwasi_parser::{BlockType, InstrBufferRef, Instruction};
use thiserror::Error;
use tracing::debug;

use crate::{vm::jit::debug::asm_fmt, Instance, ModuleFunc, Store, Trap};

use self::assembler::*;
pub use self::executable::*;

#[derive(Debug)]
pub struct Compiler<'s> {
    module: Instance,
    store: &'s Store,

    // Function-specific state
    asm: Assembler,
    free: FreeMem,
    labels: Vec<Label>,
    to_patch: HashMap<usize, Vec<PatchTarget>>,
    to_unify: HashMap<usize, Vec<UnifyTarget>>,
    to_end: Vec<(usize, Option<ConditionCode>)>,
}

#[derive(Debug, Error)]
#[error("compilation error")]
pub enum CompilationError {
    #[error("executable mapping failed: {0}")]
    ExecMapError(ExecMapError),
    #[error("unsupported instruction: `{0}`")]
    UnsupportedInstruction(Instruction),
}

impl<'s> Compiler<'s> {
    pub fn new(module: Instance, store: &'s Store) -> Self {
        Self {
            module,
            store,

            asm: Assembler::new(),
            labels: vec![],
            free: FreeMem::default(),
            to_patch: HashMap::new(),
            to_unify: HashMap::new(),
            to_end: vec![],
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
        self.free.request(2);
        self.asm.stp(Reg::Fp, Reg::Lr, Reg::Sp, 0);

        let mut stack: Vec<Operand> = vec![];
        let buf = f.code.body.slice(0..f.code.body.len() - 1);
        self.compile_buf(&mut stack, buf)?;
        self.unify(&mut stack, f.code.body.len() - 1);
        self.patch(f.code.body.len() - 1, self.asm.addr());
        self.labels.pop();

        for (offset, op) in stack.iter().filter(|op| !op.is_unreachable()).enumerate() {
            self.asm.store(offset as u32, Reg::Arg1, *op);
        }
        self.asm.mov(Reg::Arg0, 0);
        for (addr, cond) in self.to_end {
            let to = self.asm.addr() as u64;
            self.asm.patch(addr, |asm| {
                if let Some(cond) = cond {
                    asm.branch_if(to, cond);
                } else {
                    asm.branch(to);
                }
            });
        }
        if self.free.total_stack_size > 0 {
            // Align the stack size to 16 bytes
            self.free.total_stack_size += self.free.total_stack_size % 16;
            // Patch the nop
            self.asm.patch(0, |asm| {
                asm.sub(Reg::Sp, Reg::Sp, self.free.total_stack_size);
            });
            self.asm.ldp(Reg::Fp, Reg::Lr, Reg::Sp, 0);
            self.asm.add(Reg::Sp, Reg::Sp, self.free.total_stack_size);
        }
        self.asm.ret();

        let code = self.asm.consume();
        // SAFETY: The code is valid, as long as the assembler and compiler are correct. If they
        // aren't, then that's a bug...
        let exec = unsafe {
            Executable::map(&code, f.ty.1.len()).map_err(CompilationError::ExecMapError)
        }?;

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
                I::I32Const(val) => stack.push(Operand::Imm64(val as i32 as u64)),
                I::I64Const(val) => stack.push(Operand::Imm64(val as i64 as u64)),
                I::Drop => drop(stack.pop()),
                I::I32Add | I::I64Add => binop!(add or |lhs: u64, rhs| lhs.saturating_add(rhs)),
                I::I32Sub | I::I64Sub => binop!(sub or |lhs: u64, rhs| lhs.wrapping_sub(rhs)),
                I::LocalGet { idx } => {
                    self.asm.load(self.free.current, idx, Reg::Arg0);
                    stack.push(self.free.current);
                    self.free.next_free();
                }
                I::LocalSet { idx } => {
                    let op = pop!();
                    self.asm.store(idx, Reg::Arg0, op);
                }
                I::Unreachable => {
                    self.asm.mov(Reg::Arg0, Trap::Unreachable as u64);
                    self.to_end.push((self.asm.addr(), None));
                    self.asm.nop();
                    let label = self.labels.last().unwrap();
                    for _ in 0..label.arity {
                        stack.push(Operand::Unreachable);
                    }
                    return Ok(());
                }
                I::I32Eq | I::I64Eq => binop!(eq or |lhs: u64, rhs| (lhs == rhs) as u64),
                I::I32Ne | I::I64Ne => binop!(ne or |lhs: u64, rhs| (lhs != rhs) as u64),
                I::I32And | I::I64And => binop!(and or |lhs: u64, rhs| lhs & rhs),
                I::I32Or | I::I64Or => binop!(or or |lhs: u64, rhs| lhs | rhs),
                I::I32Xor | I::I64Xor => binop!(eor or |lhs: u64, rhs| lhs ^ rhs),
                I::I32Mul | I::I64Mul => binop!(mul or |lhs: u64, rhs| lhs.wrapping_mul(rhs)),
                I::I32Eqz | I::I64Eqz => {
                    let op = pop!();
                    if let Operand::Imm64(imm64) = op {
                        stack.push(Operand::Imm64((imm64 == 0) as u64));
                    } else {
                        self.asm.cmp(op, 0);
                        self.asm.cset(self.free.current, ConditionCode::Eq);
                        stack.push(self.free.current);
                        self.free.next_free();
                    }
                }
                I::Select | I::SelectT(_) => {
                    let cond = pop!();
                    let rhs = pop!();
                    let lhs = pop!();
                    if let (Operand::Imm64(cond), Operand::Imm64(lhs), Operand::Imm64(rhs)) =
                        (cond, lhs, rhs)
                    {
                        stack.push(Operand::Imm64(if cond != 0 { lhs } else { rhs }));
                    } else {
                        self.asm.cmp(cond, 0);
                        self.asm
                            .csel(self.free.current, lhs, rhs, ConditionCode::Ne);
                        stack.push(self.free.current);
                        self.free.next_free();
                    }
                }
                I::Call { func_idx } => {
                    let request = self.free.request(4);
                    let Operand::Mem64(_, offset) = request[0] else {
                        panic!();
                    };
                    self.asm.stp(Reg::Arg0, Reg::Arg1, Reg::Sp, offset as u32);
                    self.asm
                        .stp(Reg::Arg2, Reg::Arg3, Reg::Sp, offset as u32 + 2);

                    let f_addr = self.module.func_addrs()[func_idx as usize];
                    let f = &self.store.functions[f_addr];
                    // Virtual machine pointer
                    self.asm.mov(Reg::Arg0, Reg::Arg3);
                    // Function address
                    self.asm.mov(Reg::Arg1, f_addr.as_usize() as u64);
                    // Args pointer
                    let space = if !f.ty().0.is_empty() {
                        let space = self.free.request(f.ty().0.len());
                        let Operand::Mem64(_, offset) = space[0] else {
                            panic!();
                        };
                        self.asm.add(Reg::Arg2, Reg::Sp, offset * 8);
                        for i in (0..space.len()).rev() {
                            let op = pop!();
                            self.asm.store(i as u32, Reg::Arg2, op);
                        }
                        space
                    } else {
                        vec![]
                    };
                    // Args length
                    self.asm.mov(Reg::Arg3, f.ty().0.len() as u64);
                    // This arg is a pointer to the pointer to the locals array. Since only x0 and
                    // x1 can be used for return values, this is our hypothetical "third" return
                    // (which should be written to by the function being called). There is a
                    // calling convention for returning more than 2 values, but it's not worth the
                    // trouble either side of the FFI boundary.
                    self.asm.add(Reg::Arg4, Reg::Sp, offset * 8);
                    let mut saved = vec![];
                    for gpr in stack
                        .iter_mut()
                        .filter(|op| matches!(op, Operand::Reg(Reg::GPR0 | Reg::GPR1 | Reg::GPR2)))
                    {
                        let save = self.free.save();
                        saved.push((save, *gpr));
                        self.asm.mov(save, *gpr);
                    }
                    for save in space {
                        self.free.release(save);
                    }

                    self.asm.load(Reg::LoadTemp, offset as u32 + 2, Reg::Sp);
                    self.asm.branch_link_register(Reg::LoadTemp);
                    for i in 0..f.ty().1.len() {
                        let op = self.free.current;
                        self.asm.load(op, i as u32, Reg::Arg1);
                        stack.push(op);
                        self.free.next_free();
                    }
                    // Success trap code
                    self.asm.cmp(Reg::Arg0, 0);
                    self.to_end.push((self.asm.addr(), Some(ConditionCode::Ne)));
                    // Will be patched to branch to end
                    self.asm.nop();
                    // self.asm.store(offset as u32, Reg::Sp, Reg::Arg2);

                    for (save, gpr) in saved {
                        self.free.release(save);
                        self.asm.mov(gpr, save);
                    }

                    self.asm.ldp(Reg::Arg0, Reg::Arg1, Reg::Sp, offset as u32);
                    self.asm
                        .ldp(Reg::Arg2, Reg::Arg3, Reg::Sp, offset as u32 + 2);
                    for op in request {
                        self.free.release(op);
                    }
                }

                I::Br { .. } | I::Return => {
                    let depth = match instr {
                        I::Br { depth } => depth,
                        I::Return => self.labels.len() as u32 - 1,
                        _ => unreachable!(),
                    };
                    let label = self.get_label(depth);
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
                    self.make_branch(
                        end,
                        UnifyTarget {
                            addr: unify_addr,
                            stack: branch_stack,
                        },
                    );
                    if stack.len() >= expect_height && stack.len() - expect_height > expect_arity {
                        stack.truncate(expect_height - expect_arity);
                    }

                    return Ok(());
                }
                I::BrIf { depth } => {
                    let op = pop!();
                    self.asm.cmp(op, 0);
                    let mut branch_stack = stack.clone();
                    let label = self.get_label(depth);
                    let end = label.end;
                    branch_stack.drain(label.init_height..branch_stack.len() - label.arity);
                    debug_assert!(branch_stack.len() - label.init_height == label.arity);
                    let unify_addr = self.asm.addr();
                    self.pad_movs(&branch_stack[label.init_height..]);
                    let patch_addr = self.asm.addr();
                    self.asm.branch(0xdeadbeef);
                    self.set_to_patch(end, PatchTarget::conditional(patch_addr, ConditionCode::Ne));
                    self.make_branch(
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
                    let init_height = stack.len() - self.param_arity(block.ty);

                    let buf = buf.inner().slice(buf.start() + i + 1..block.end);
                    let len = buf.len();

                    let mut branch = stack.clone();
                    self.compile_buf(&mut branch, buf)?;
                    let if_unify_addr = self.asm.addr();
                    self.pad_movs(&branch[init_height..]);
                    self.make_branch(
                        block.end,
                        UnifyTarget {
                            addr: if_unify_addr,
                            stack: branch,
                        },
                    );
                    let jump_end_addr = self.asm.addr();
                    self.asm.nop();

                    let to = self.asm.addr();
                    self.asm.patch(branch_addr, |asm| {
                        asm.branch_if(to as u64, ConditionCode::Eq);
                    });
                    self.unify(stack, block.end);
                    self.patch(block.end, self.asm.addr());
                    let to = self.asm.addr();
                    self.asm.patch(jump_end_addr, |asm| {
                        asm.branch(to as u64);
                    });
                    self.labels.pop().unwrap();
                    i += len + 1;
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
                    self.make_branch(
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
                I::End => unreachable!("should never reach end instruction!"),
                _ => return Err(CompilationError::UnsupportedInstruction(instr)),
            }
            i += 1;
        }

        Ok(())
    }

    fn get_label(&self, depth: u32) -> &Label {
        &self.labels[self.labels.len() - depth as usize - 1]
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

    fn make_branch(&mut self, end: usize, target: UnifyTarget) {
        self.to_unify.entry(end).or_default().push(target);
    }

    fn pad_movs(&mut self, ops: &[Operand]) {
        for op in ops {
            match op {
                Operand::Imm64(imm) if ((-(1 << 16) + 1)..0).contains(&(*imm as i64)) => {
                    self.asm.nop();
                }
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

    /// Unifies the stack at a given end index.
    ///
    /// `stack` will be the "main" branch, and other branches (found in `self.to_unify`) will be
    /// merged. This is akin to a phi instruction in SSA form.
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
                    // If it is unreachable, then we can use any operand to merge with, and it'll
                    // be correct.
                    if main.is_unreachable() {
                        *main = self.free.current;
                        self.free.next_free();
                    }
                    if !branch.is_unreachable() {
                        asm.mov(*main, *branch);
                    }
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
    free_stack: BitVec,
    total_stack_size: u64,
}

impl Default for FreeMem {
    fn default() -> Self {
        let mut s = Self {
            current: Operand::Reg(Reg::GPR0),
            reg: FreeReg::all(),
            free_stack: BitVec::new(),
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
        } else if let Some(offset) = self.free_stack.iter().position(|b| !b) {
            self.free_stack.set(offset, true);
            Operand::Mem64(Reg::Sp, offset as u64)
        } else {
            let len = self.free_stack.len();
            self.free_stack.push(true);
            Operand::Mem64(Reg::Sp, len as u64)
        };
        self.current = next;
        if let Operand::Mem64(_, offset) = self.current {
            // times 8 because each mem64 item is 8 bytes
            self.total_stack_size = self.total_stack_size.max(offset * 8);
        }
    }

    fn request(&mut self, n: usize) -> Vec<Operand> {
        if let Some((offset, _)) = self
            .free_stack
            .windows(n)
            .enumerate()
            .find(|(_, w)| w.iter_zeros().count() == n)
        {
            let mut ops = vec![];
            for i in offset..offset + n {
                self.free_stack.set(i, true);
                ops.push(Operand::Mem64(Reg::Sp, i as u64));
            }
            ops
        } else {
            let mut ops = vec![];
            for _ in 0..n {
                ops.push(Operand::Mem64(Reg::Sp, self.free_stack.len() as u64));
                self.free_stack.push(true);
            }
            self.total_stack_size = self.total_stack_size.max(self.free_stack.len() as u64 * 8);
            ops
        }
    }

    fn save(&mut self) -> Operand {
        if let Some(offset) = self.free_stack.iter().position(|b| !b) {
            self.free_stack.set(offset, true);
            self.total_stack_size = self.total_stack_size.max(offset as u64 * 8);
            Operand::Mem64(Reg::Sp, offset as u64)
        } else {
            let len = self.free_stack.len() as u64;
            self.free_stack.push(true);
            self.total_stack_size = self.total_stack_size.max(len * 8);
            Operand::Mem64(Reg::Sp, len)
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
                self.free_stack.set(offset as usize, false);
            }
            Operand::Imm64(_) | Operand::Unreachable => {}
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
        let store = Store::default();
        let compiler = Compiler::new(Instance::default(), &store);
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
        let store = Store::default();
        let compiler = Compiler::new(Instance::default(), &store);
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
        let store = Store::default();
        let compiler = Compiler::new(Instance::default(), &store);
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
        let store = Store::default();
        let compiler = Compiler::new(Instance::default(), &store);
        let executable = compiler
            .compile(&ModuleFunc {
                ty: FuncType(vec![], vec![ValType::I32]),
                code,
                inst: Instance::default(),
            })
            .unwrap();
        asm_assert_eq!(
            &[
                0xd10043ff, 0xa9007bfd, 0xd280014b, 0xf900002b, 0xd2800000, 0xa9407bfd, 0x910043ff,
                0xd65f03c0
            ],
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
        let store = Store::default();
        let compiler = Compiler::new(Instance::default(), &store);
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
        let store = Store::default();
        let compiler = Compiler::new(Instance::default(), &store);
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
        let store = Store::default();
        let compiler = Compiler::new(Instance::default(), &store);
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
        let store = Store::default();
        let compiler = Compiler::new(Instance::default(), &store);
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
