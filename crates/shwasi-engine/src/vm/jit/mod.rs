mod assembler;
mod call_graph;
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

use crate::{
    vm::jit::{call_graph::CallGraph, debug::asm_fmt},
    Addr, Func, Instance, ModuleFunc, Store,
};

use self::assembler::*;
pub use self::executable::*;

#[derive(Debug)]
pub struct Compiler<'s> {
    module: Instance,
    store: &'s Store,
    call_graph: CallGraph,

    // Compiled function related state
    asm: Assembler,
    free: FreeMem,
    labels: Vec<Label>,
    to_patch: HashMap<usize, Vec<PatchTarget>>,
    to_unify: HashMap<usize, Vec<UnifyTarget>>,
    call_patches: HashMap<Addr<Func>, Vec<usize>>,
    calls: Vec<(Addr<Func>, Vec<u8>)>,
    loads: Vec<(u32, Operand, usize)>,
    stores: Vec<(u32, Operand, usize)>,
    args: usize,
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
            module: module.clone(),
            store,
            call_graph: CallGraph::construct(module, store),

            asm: Assembler::new(),
            labels: vec![],
            free: FreeMem::default(),
            to_patch: HashMap::new(),
            to_unify: HashMap::new(),
            call_patches: HashMap::new(),
            calls: vec![],
            loads: vec![],
            stores: vec![],
            args: 0,
        }
    }

    pub fn compile(mut self, f: &ModuleFunc) -> Result<Executable, CompilationError> {
        debug!("got code to compile: \n{}", f.code);

        self.compile_func(f)?;
        let mut code = self.asm.consume();

        let arg_space = (f.ty.0.len().saturating_sub(8) * 8) as u64;
        let mut stack_space = 24 + arg_space + (f.ty.1.len().saturating_sub(8) * 8) as u64;
        stack_space += stack_space % 16;
        self.asm.sub(Reg::Sp, Reg::Sp, stack_space);
        self.asm
            .stp(Reg::Fp, Reg::Lr, Reg::Sp, arg_space as u32 / 8);
        // The address at Reg::Arg1 should hold the address to a slice of return values
        self.asm
            .store((stack_space / 8) as u32 - 1, Reg::Sp, Reg::Arg1);
        for i in (0..f.ty.0.len()).rev() {
            let dst = Reg::try_from_argnum(i as u8)
                .map_or_else(|| Operand::Mem64(Reg::Sp, i as u64 - 8), Into::into);
            // Prepare arguments for the function
            self.asm.load(dst, i as u32, Reg::Arg0);
        }
        if f.ty.1.len() > 8 {
            // Compute base, and store in indirect result location register (GPR0)
            self.asm.add(Reg::GPR0, Reg::Sp, 16 + arg_space);
        }
        let bl_addr = self.asm.addr();
        self.asm.branch(0xdeadbeef);
        // mov into return values
        self.asm
            .load(Reg::LoadTemp3, (stack_space / 8) as u32 - 1, Reg::Sp);
        for i in 0..f.ty.1.len() {
            let ret_val = Reg::try_from_argnum(i as u8).map_or_else(
                || Operand::Mem64(Reg::NoWrite0, (i - 8) as u64),
                Operand::Reg,
            );
            // // TODO: fix this NoWrite1 hack by actually computing addr
            self.asm.store(i as u32, Reg::LoadTemp3, ret_val);
        }
        self.asm
            .ldp(Reg::Fp, Reg::Lr, Reg::Sp, arg_space as u32 / 8);
        self.asm.add(Reg::Sp, Reg::Sp, stack_space);
        self.asm.ret();

        let addr = self.asm.addr();
        self.asm.patch(bl_addr, |asm| {
            asm.branch_link(addr as u64);
        });
        self.asm.append(&mut code);

        let code = self.asm.consume();
        // SAFETY: The code is valid, as long as the assembler and compiler are correct. If they
        // aren't, then that's a bug...
        let exec = unsafe { Executable::map(&code).map_err(CompilationError::ExecMapError) }?;

        debug!("compiled into \n{}", asm_fmt(exec.as_bytes()));
        Ok(exec)
    }

    fn compile_func(&mut self, f: &ModuleFunc) -> Result<(), CompilationError> {
        self.labels.push(Label {
            init_height: 0,
            arity: f.ty.1.len(),
            result_arity: f.ty.1.len(),
            end: f.code.body.len() - 1,
        });
        self.args = f.ty.0.len();
        // Space for function prologue. Here, there will be instructions for subtracting from the
        // stack pointer, storing the frame pointer, and storing the link register.
        let prologue_addr = self.asm.addr();
        self.asm.nop();
        self.asm.nop();
        let num_locals = f
            .code
            .locals
            .iter()
            .map(|num_locals| num_locals.num)
            .sum::<u32>();
        // Store locals on the stack
        for i in 0..num_locals {
            self.asm.store(i, Reg::Sp, 0);
        }
        if f.ty.1.len() > 8 {
            // Store indirect result location register
            self.asm.mov(Reg::NoWrite0, Reg::GPR0);
        }
        // Set the free memory offset to the end of the locals
        self.free.set_offset(num_locals);

        let mut stack = vec![];
        let buf = f.code.body.slice(0..f.code.body.len() - 1);
        self.compile_buf(&mut stack, buf)?;
        self.unify(&mut stack, f.code.body.len() - 1);
        self.patch(f.code.body.len() - 1, self.asm.addr());
        self.labels.pop();

        // Return values
        for op in stack.iter_mut() {
            if let Operand::Reg(reg) = op {
                if reg.is_arg() {
                    self.asm.mov(self.free.current, *reg);
                    *op = self.free.current;
                    self.free.next_free();
                }
            }
        }
        for (offset, op) in stack.iter().enumerate() {
            let dst = Reg::try_from_argnum(offset as u8).map_or_else(
                || Operand::Mem64(Reg::NoWrite0, (offset - 8) as u64),
                Operand::Reg,
            );
            self.asm.mov(dst, *op);
        }

        let has_calls = !self.calls.is_empty();
        // Stack space for storing lr and fp
        if has_calls {
            self.free.total_stack_size += 16;
        }
        if self.free.total_stack_size > 0 {
            // Align the stack size to 16 bytes
            self.free.total_stack_size += self.free.total_stack_size % 16;
            // Patch function prologue
            self.asm.patch(prologue_addr, |asm| {
                asm.sub(Reg::Sp, Reg::Sp, self.free.total_stack_size);
                if has_calls {
                    asm.stp(
                        Reg::Fp,
                        Reg::Lr,
                        Reg::Sp,
                        ((self.free.total_stack_size / 8) - 2) as u32,
                    );
                }
            });
            if has_calls {
                // Function epilogue
                self.asm.ldp(
                    Reg::Fp,
                    Reg::Lr,
                    Reg::Sp,
                    ((self.free.total_stack_size / 8) - 2) as u32,
                );
            }
            self.asm.add(Reg::Sp, Reg::Sp, self.free.total_stack_size);
        }
        self.asm.ret();
        self.handle_calls();
        for (i, dst, addr) in std::mem::take(&mut self.loads) {
            self.asm.patch(addr, |asm| {
                asm.load(dst, (self.free.total_stack_size / 8) as u32 + i, Reg::Sp);
            });
        }
        for (i, op, addr) in std::mem::take(&mut self.stores) {
            self.asm.patch(addr, |asm| {
                asm.mov(
                    Operand::Mem64(Reg::Sp, (self.free.total_stack_size / 8) + i as u64),
                    op,
                );
            });
        }

        Ok(())
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
                // TODO: maybe this needs pop!()
                I::Drop => drop(stack.pop()),
                I::I32Add | I::I64Add => binop!(add or |lhs: u64, rhs| lhs.saturating_add(rhs)),
                I::I32Sub | I::I64Sub => binop!(sub or |lhs: u64, rhs| lhs.wrapping_sub(rhs)),
                I::LocalGet { idx } => {
                    let op = if (idx as usize) < self.args {
                        if let 0..=7 = idx {
                            Operand::Reg(Reg::try_from_argnum(idx as u8).unwrap())
                        } else {
                            let dst = self.free.current;
                            // Will be patched
                            self.loads.push((idx - 8, dst, self.asm.addr()));
                            self.asm.load(dst, idx, Reg::GPR1);
                            self.free.next_free();
                            dst
                        }
                    } else {
                        Operand::Mem64(Reg::Sp, (idx - self.args as u32) as u64)
                    };
                    stack.push(op);
                }
                I::LocalSet { idx } => {
                    let op = pop!();
                    let stores_len = self.stores.len();
                    let dst = if (idx as usize) < self.args {
                        if let 0..=7 = idx {
                            Operand::Reg(Reg::try_from_argnum(idx as u8).unwrap())
                        } else {
                            self.stores.push((idx - 8, op, self.asm.addr()));
                            Operand::Reg(Reg::GPR0)
                        }
                    } else {
                        Operand::Mem64(Reg::Sp, (idx - self.args as u32) as u64)
                    };
                    self.asm.mov(dst, op);
                    if stores_len != self.stores.len() {
                        self.asm.nop();
                    }
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
                    let f_addr = self.module.func_addrs()[func_idx as usize];
                    let f = &self.store.functions[f_addr];
                    let mut saved = vec![];

                    for i in (0..f.ty().0.len()).rev() {
                        let op = pop!();
                        if i < self.args {
                            if let 0..=7 = i {
                                let save_loc = self.free.save();
                                let reg = Reg::try_from_argnum(i as u8).unwrap();
                                self.asm.mov(save_loc, reg);
                                saved.push((save_loc, reg.into()));
                            } else {
                                let offset = (i - 8) as u64;
                                let save_loc = self.free.save();
                                let current_loc = Operand::Mem64(Reg::Sp, offset);
                                self.asm.mov(save_loc, current_loc);
                                saved.push((save_loc, current_loc));
                            }
                        }
                        let dst = Reg::try_from_argnum(i as u8)
                            .map_or_else(|| Operand::Mem64(Reg::Sp, (i - 8) as u64), Into::into);
                        self.asm.mov(dst, op);
                    }
                    for op in stack
                        .iter_mut()
                        .filter(|op| matches!(op, Operand::Reg(Reg::GPR0 | Reg::GPR1 | Reg::GPR2)))
                    {
                        let save = self.free.save();
                        self.asm.mov(save, *op);
                        self.free.release(*op);
                        saved.push((save, *op));
                    }
                    if f.ty().1.len() > 8 {
                        let over = f.ty().1.len() - 8;
                        // Compute base, and store in indirect result location register (GPR0)
                        self.asm.add(Reg::GPR0, Reg::Sp, self.free.total_stack_size);
                        // Reserve memory for indirect results
                        for _ in 0..over {
                            let loc = self.free.save();
                            self.free.release(loc);
                        }
                    }
                    // TODO: handle recursive functions
                    // TODO: handle host functions
                    match f {
                        Func::Module(f)
                            if &f.code.body == buf.inner() || self.call_graph.in_cycle(f_addr) =>
                        {
                            return Err(CompilationError::UnsupportedInstruction(Instruction::End));
                        }
                        Func::Module(_) if self.call_patches.get(&f_addr).is_some() => {
                            self.call_patches
                                .entry(f_addr)
                                .or_default()
                                .push(self.asm.addr());
                            self.asm.branch(0xdeadbeef);
                        }
                        Func::Module(f) => {
                            let mut compiler = Compiler::new(self.module.clone(), self.store);
                            compiler.compile_func(f)?;
                            let code = compiler.asm.consume();
                            self.call_patches
                                .entry(f_addr)
                                .or_default()
                                .push(self.asm.addr());
                            self.asm.branch(0xdeadbeef);
                            self.calls.push((f_addr, code));
                        }
                        Func::Host(_) => {
                            return Err(CompilationError::UnsupportedInstruction(Instruction::End))
                        }
                    }
                    for i in 0..f.ty().1.len() {
                        let ret_val = Reg::try_from_argnum(i as u8)
                            // TODO: fix this NoWrite0 hack by actually computing addr
                            .map_or_else(
                                || Operand::Mem64(Reg::NoWrite0, (i - 8) as u64),
                                Operand::Reg,
                            );
                        self.asm.mov(self.free.current, ret_val);
                        stack.push(self.free.current);
                        self.free.next_free();
                    }
                    for (save_loc, op) in saved {
                        self.asm.mov(op, save_loc);
                        self.free.release(save_loc);
                        if let Operand::Reg(reg) = op {
                            self.free.use_reg(reg);
                        }
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

    fn handle_calls(&mut self) {
        let calls = self.calls.drain(..);
        for (addr, mut code) in calls {
            let assembler_addr = self.asm.addr();
            self.call_patches
                .remove(&addr)
                .unwrap()
                .iter()
                .for_each(|patch_addr| {
                    self.asm
                        .patch(*patch_addr, |asm| asm.branch_link(assembler_addr as u64));
                });
            self.asm.append(&mut code);
        }
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
    offset: u32,
}

impl Default for FreeMem {
    fn default() -> Self {
        let mut s = Self {
            current: Operand::Reg(Reg::GPR0),
            reg: FreeReg::all(),
            free_heap: BinaryHeap::new(),

            total_stack_size: 0,
            offset: 0,
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
            self.free_heap.push(Reverse(offset.0 + 1));
            Operand::Mem64(Reg::Sp, offset.0)
        } else {
            self.free_heap.push(Reverse(self.offset as u64 + 1));
            Operand::Mem64(Reg::Sp, self.offset as u64)
        };
        self.current = next;
        if let Operand::Mem64(_, offset) = self.current {
            // times 8 because each mem64 item is 8 bytes
            self.total_stack_size = self.total_stack_size.max(offset * 8);
        }
    }

    fn save(&mut self) -> Operand {
        if let Some(mem) = self.free_heap.pop() {
            self.free_heap.push(Reverse(mem.0 + 1));
            self.total_stack_size = self.total_stack_size.max(mem.0 * 8);
            Operand::Mem64(Reg::Sp, mem.0)
        } else {
            self.free_heap.push(Reverse(self.offset as u64 + 1));
            self.total_stack_size = self.total_stack_size.max((self.offset as u64 + 1) * 8);
            Operand::Mem64(Reg::Sp, self.offset as u64)
        }
    }

    fn release(&mut self, op: Operand) {
        match op {
            Operand::Reg(reg) => {
                if reg.is_arg() {
                    return;
                }
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

    fn set_offset(&mut self, offset: u32) {
        self.offset = offset;
        self.total_stack_size = offset as u64 * 8;
    }

    fn use_reg(&mut self, reg: Reg) {
        if reg.is_arg() {
            return;
        }
        let reg = match reg {
            Reg::GPR0 => FreeReg::GPR0,
            Reg::GPR1 => FreeReg::GPR1,
            Reg::GPR2 => FreeReg::GPR2,
            _ => panic!("cannot use special register"),
        };

        self.reg ^= reg;
    }
}

#[cfg(test)]
mod tests {
    use shwasi_parser::{Block, BlockType, Code, FuncType, InstrBuffer, ValType};

    use super::*;
    use crate::{value::ValueUntyped, Value};
    use test_log::test;

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

    #[test]
    fn lots_of_args() {
        let code = Code {
            body: InstrBuffer::from_iter([
                Instruction::LocalGet { idx: 0 },
                Instruction::LocalGet { idx: 1 },
                Instruction::LocalGet { idx: 2 },
                Instruction::LocalGet { idx: 3 },
                Instruction::LocalGet { idx: 4 },
                Instruction::LocalGet { idx: 5 },
                Instruction::LocalGet { idx: 6 },
                Instruction::LocalGet { idx: 7 },
                Instruction::LocalGet { idx: 8 },
                Instruction::LocalGet { idx: 9 },
                Instruction::LocalGet { idx: 10 },
                Instruction::LocalGet { idx: 11 },
                Instruction::Drop,
                Instruction::Drop,
                Instruction::Drop,
                Instruction::Drop,
                Instruction::Drop,
                Instruction::Drop,
                Instruction::Drop,
                Instruction::Drop,
                Instruction::Drop,
                Instruction::Drop,
                Instruction::I32Add,
                Instruction::End,
            ]),
            locals: vec![],
        };
        let store = Store::default();
        let compiler = Compiler::new(Instance::default(), &store);
        let executable = compiler
            .compile(&ModuleFunc {
                ty: FuncType(vec![ValType::I32; 12], vec![ValType::I32]),
                code,
                inst: Instance::default(),
            })
            .unwrap();
        let mut locals = [Value::I32(10).untyped(); 12];
        let mut out = [ValueUntyped::default()];
        executable.run_with(&mut locals, &mut out);
        assert_eq!([Value::I32(20).untyped()], out);
    }

    #[test]
    fn lots_of_returns() {
        let code = Code {
            body: InstrBuffer::from_iter([
                Instruction::I32Const(10),
                Instruction::I32Const(10),
                Instruction::I32Const(10),
                Instruction::I32Const(10),
                Instruction::I32Const(10),
                Instruction::I32Const(10),
                Instruction::I32Const(10),
                Instruction::I32Const(10),
                Instruction::I32Const(10),
                Instruction::I32Const(10),
                Instruction::End,
            ]),
            locals: vec![],
        };
        let store = Store::default();
        let compiler = Compiler::new(Instance::default(), &store);
        let executable = compiler
            .compile(&ModuleFunc {
                ty: FuncType(vec![], vec![ValType::I32; 10]),
                code,
                inst: Instance::default(),
            })
            .unwrap();
        let mut locals = [];
        let mut out = [ValueUntyped::default(); 10];
        executable.run_with(&mut locals, &mut out);
        assert_eq!([Value::I32(10).untyped(); 10], out);
    }
}
