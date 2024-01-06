#[cfg(all(target_arch = "aarch64", feature = "jit"))]
mod jit;
mod ops;

use std::mem;

use num_enum::TryFromPrimitive;
#[cfg(all(target_arch = "aarch64", feature = "jit"))]
use rustc_hash::FxHashMap;
use shwasi_parser::{BlockType, InitExpr, InstrBuffer, Instruction, MemArg, NumLocals};
use thiserror::Error;
#[cfg(debug_assertions)]
use tracing::trace;
#[cfg(all(target_arch = "aarch64", feature = "jit"))]
use tracing::{info, warn};

use self::ops::*;
use crate::{
    store::{Addr, Func, Global},
    value::{Value, ValueUntyped},
    Error, Instance, Ref, Store,
};

/// The WebAssembly virtual machine that this crate uses internally.
///
/// Note that this is a very internal struct, and has a very low-level interface.
#[derive(Debug)]
pub struct Vm<'s> {
    /// The stack of values. This is used to store locals, arguments, and intermediate values.
    pub(crate) stack: Vec<ValueUntyped>,
    /// A "stack" of frames.
    ///
    /// Note that this is note actually a stack, as it has no reason to be.
    /// Because only one frame can be active at a time, we can just keep track of the current frame
    /// and replace it with the old frame when the function call ends.
    frame: StackFrame,
    /// Stack of labels.
    labels: Vec<Label>,
    store: &'s mut Store,
    /// A map of compiled functions.
    ///
    /// This is used to cache JIT compiled functions. We use FxHashMap here because it's much
    /// faster for integer keys. This is important so we can call `call_raw` with as little
    /// overhead as possible.
    #[cfg(all(target_arch = "aarch64", feature = "jit"))]
    compiled: FxHashMap<Addr<Func>, jit::Executable>,
}

/// The maximum number of nested levels we can have on the frame stack.
const STACK_BUDGET: usize = 300;

/// A WebAssembly stack frame.
///
/// See [`Vm`] for more information on how this is used.
#[derive(Debug)]
struct StackFrame {
    /// The module this frame is for.
    module: Instance,
    /// Used to get locals and arguments.
    bp: usize,
    /// Used as a counter to make sure we don't overflow the stack (our budget is STACK_BUDGET).
    /// This starts at STACK_BUDGET and decrements every time we push a new frame. If it reaches
    /// zero, we cannot push a new frame.
    nested_levels: usize,
    label_idx: usize,
}

impl StackFrame {
    fn new(module: Instance) -> Self {
        Self {
            module,
            bp: 0,
            nested_levels: STACK_BUDGET,
            label_idx: 0,
        }
    }

    // Because we don't actually have a real stack, this push operation is a bit different.
    #[inline(always)]
    fn push(&self, module: Instance, bp: usize, label_idx: usize) -> Option<Self> {
        Some(Self {
            module,
            bp,
            // Cannot push if our stack budget is exhausted
            nested_levels: self.nested_levels.checked_sub(1)?,
            label_idx,
        })
    }
}

#[derive(Debug)]
struct Label {
    /// The continuation arity of the label. This is the number of values that should be on the
    /// stack when the label's continuation is jumped to (the return address). This happens when a
    /// branch is taken.
    ///
    /// For `if`, `block` and function labels, this is the same as the end types. For `loop`
    /// labels, this is the params of the block.
    arity: usize,
    /// Return address of the block. In the spec, this is called the continuation.
    ra: usize,
    /// The stack height when the label was pushed
    stack_height: usize,
}

#[derive(Debug, Error, TryFromPrimitive, Clone, Copy)]
#[repr(u8)]
pub enum Trap {
    #[error("stack overflow")]
    StackOverflow = 1,
    #[error("unreachable encountered")]
    Unreachable,
    #[error("division by zero")]
    DivideByZero,
    #[error("integer overflow")]
    IntegerOverflow,
    #[error("bad float truncation")]
    BadTruncate,
    #[error("table get out of bounds")]
    TableGetOutOfBounds,
    #[error("call indirect type mismatch")]
    CallIndirectTypeMismatch,
    #[error("attempted to call a null reference")]
    CallNullRef,
    #[error("out of bounds memory access")]
    MemoryAccessOutOfBounds,
}

pub type Result<T> = std::result::Result<T, Error>;

impl<'s> Vm<'s> {
    #[allow(private_interfaces)]
    pub fn new(store: &'s mut Store, module: Instance) -> Self {
        Self {
            store,
            stack: vec![],
            frame: StackFrame::new(module),
            labels: vec![],
            #[cfg(all(target_arch = "aarch64", feature = "jit"))]
            compiled: FxHashMap::default(),
        }
    }

    pub fn call<I>(&mut self, f_addr: Addr<Func>, args: I) -> Result<Vec<Value>>
    where
        I: IntoIterator<Item = ValueUntyped>,
    {
        // Push arguments onto the stack
        for arg in args {
            self.push(arg);
        }

        // SAFETY: As long as the function is never mutated, we should be fine here. This is also a
        // non-null pointer.
        unsafe { self.call_raw(f_addr) }?;
        // We can just take the stack because we know that call_inner will clear any values that
        // are not results.
        let res = std::mem::take(&mut self.stack);
        let f = &self.store.functions[f_addr];
        Ok(res
            .into_iter()
            .enumerate()
            .map(|(i, v)| v.into_typed(f.ty().1[i]))
            .collect())
    }

    /// Get a mutable reference to the underlying store of the vm.
    pub fn get_store(&mut self) -> &mut Store {
        self.store
    }

    pub fn get_module(&self) -> Instance {
        self.frame.module.clone()
    }

    /// Call a raw function pointer.
    ///
    /// # Safety
    /// This function is unsafe because it dereferences a raw pointer. Additionally, given that the
    /// function pointer will come from a store, there is no guarantee about the validity of the
    /// underlying function post-execution.
    ///
    /// This function is only safe to call if the function pointer is valid, and if the funtion
    /// field of the store is **never**, **ever** mutated. This is an invariant that must be
    /// upheld diligently throughout the **entire** virtual machine. In WebAssembly, there's no
    /// reason why a function should ever be mutated.
    unsafe fn call_raw(&mut self, f_addr: Addr<Func>) -> Result<()> {
        let f = (&mut self.store.functions[f_addr]) as *mut Func;
        // This dereference is the only unsafe part of this function. We cast it to a shared
        // reference out of convenience.
        match &mut unsafe { &mut *f } {
            Func::Module(f) => {
                // Args should already be on the stack (due to validation), so push locals
                let mut pushed_locals = 0;
                for NumLocals { num, locals_type } in &f.code.locals {
                    for _ in 0..*num {
                        // Locals are initialized to their default value
                        self.push(ValueUntyped::type_default(*locals_type));
                        pushed_locals += 1;
                    }
                }

                // Base pointer, used to get locals and arguments
                let bp = self.stack.len() - f.ty.0.len() - pushed_locals;
                let new_frame = self
                    .frame
                    .push(f.inst.clone(), bp, self.labels.len())
                    .ok_or(Trap::StackOverflow)?;
                // Keep track of the old frame to replace later. This is what emulates a call stack
                let old_frame = mem::replace(&mut self.frame, new_frame);

                #[cfg(all(target_arch = "aarch64", feature = "jit"))]
                if let Some(executable) = self.compiled.get(&f_addr) {
                    let executable = executable as *const jit::Executable;
                    (*executable).run(self)?;
                    self.clear_block(bp, f.ty.1.len());
                    self.frame = old_frame;
                    return Ok(());
                } else {
                    // For now, JIT compilation only works on aarch64 platforms
                    match jit::Compiler::new(self.frame.module.clone(), self.store).compile(f) {
                        Ok(executable) => {
                            info!("successfully JIT compiled function");
                            self.compiled.insert(f_addr, executable);
                            let executable =
                                self.compiled.get(&f_addr).unwrap() as *const jit::Executable;
                            (*executable).run(self)?;
                            self.clear_block(bp, f.ty.1.len());
                            self.frame = old_frame;
                            return Ok(());
                        }
                        Err(e) => {
                            warn!("failed to JIT compile function: {e}");
                        }
                    }
                }

                self.labels.push(Label {
                    arity: f.ty.1.len(),
                    ra: f.code.body.len() - 1,
                    stack_height: bp,
                });
                if cfg!(debug_assertions) {
                    // See https://github.com/rust-lang/rust/issues/34283.
                    // This is a workaround for some annyoing rustc behavior. In debug mode, match
                    // arms will tend to allocate stack space proportional to the number of arms,
                    // instead of the maximum one. This is only a real problem because we have a
                    // ton of arms in the `execute` method, each matching an instruction.
                    //
                    // The solution for this is to use the `stacker` crate, which will allocate
                    // more stack space when we're in a certain "red zone" (here is 64kb). If we
                    // reach this, we allocate more stack space (1mb in this case).
                    //
                    // This behavior is **not** present in release mode.
                    stacker::maybe_grow(128 * 1024, 1024 * 1024, || -> Result<()> {
                        self.execute(&f.code.body)
                    })?;
                } else {
                    self.execute(&f.code.body)?;
                }

                self.frame = old_frame;

                // Clear locals and arguments. Other stuff from the block should have already been
                // cleared (due to the mandatory `End` instruction at the end of functions).
                self.clear_block(bp, f.ty.1.len());
            }
            Func::Host(host_func) => {
                let res = (host_func.code)(self)?;
                self.stack.extend_from_slice(&res);
            }
        }

        Ok(())
    }

    /// Executes a set of instructions.
    fn execute(&mut self, body: &InstrBuffer) -> Result<()> {
        use Instruction as I;

        macro_rules! bool_binop {
            ($op:tt for $conv:ty) => {{
                let b = self.pop::<$conv>();
                let last: $conv = (*self.stack.last().unwrap()).into();
                *self.stack.last_mut().unwrap() = (last $op b).into();
            }};
        }
        macro_rules! binop {
            ($method:ident for $conv:ty$(, $err:expr)?) => {{
                let b = self.pop::<$conv>();
                let last: $conv = (*self.stack.last().unwrap()).into();
                *self.stack.last_mut().unwrap() = last
                    .$method(b)$(.ok_or($err)?)?
                    .into();
            }};
        }
        macro_rules! unop {
            ($method:ident for $conv:ty$(, $err:expr)?) => {{
                let a: $conv = (*self.stack.last().unwrap()).into();
                *self.stack.last_mut().unwrap() = a.$method()$(.ok_or($err)?)?.into();
            }};
        }
        macro_rules! load {
            ($ty:ty, $encode:ty, $offset:expr) => {{
                let offset = self.pop::<u32>().saturating_add($offset);
                let bytes = self.load(offset)?;
                self.push(<$ty>::from_le_bytes(bytes) as $encode);
            }};
        }
        macro_rules! store {
            ($encode:ty, $offset:expr) => {{
                let val = self.pop::<$encode>();
                let offset = self.pop::<u32>() + $offset;
                self.store(offset, val.to_le_bytes())?;
            }};
        }

        // Our instruction pointer. We don't use a for loop because we need to be able to jump to
        // instructions.
        let mut ip = 0;
        loop {
            // SAFETY: We know that the instruction pointer is always in because we break when we
            // get to the last `end` instruction. We make sure that we always hit the last `end`
            let instr = unsafe { body.get_unchecked(ip) };
            #[cfg(debug_assertions)]
            trace!("executing instruction: {instr}");

            match instr {
                I::Nop => {}
                I::Unreachable => return Err(Trap::Unreachable.into()),
                I::I32Const(i32) => self.push(i32),
                I::I32Add => binop!(add for u32),
                I::I32Sub => binop!(sub for u32),
                I::I32Mul => binop!(mul for u32),
                I::I32DivS => binop!(divs for u32, Trap::DivideByZero),
                I::I32DivU => binop!(divu for u32, Trap::DivideByZero),
                I::End => {
                    self.labels.pop().unwrap();
                    if ip == body.len() - 1 {
                        return Ok(());
                    }
                }
                I::Else => {
                    let label = self.labels.pop().unwrap();
                    // We jump to continuation on an else block because if we receive this
                    // instruction, it means that the `If` block main body was executed, so treat
                    // this as an `End` instruction.
                    ip = label.ra;
                }
                I::Return => {
                    self.labels.drain(self.frame.label_idx..);
                    return Ok(());
                }
                I::Drop => drop(self.stack.pop()),
                I::Select | I::SelectT(_) => {
                    let (a, b, cond) = self.pop3::<ValueUntyped, ValueUntyped, bool>();
                    self.push(if cond { a } else { b });
                }

                // Control instructions
                I::Loop(block) => {
                    self.labels.push(Label {
                        arity: self.param_arity(block.ty),
                        // Return to the beginning of the block
                        ra: ip + 1,
                        stack_height: self.stack.len() - self.param_arity(block.ty),
                    });
                }
                I::Block(block) => {
                    self.labels.push(Label {
                        arity: self.return_arity(block.ty),
                        ra: block.end,
                        stack_height: self.stack.len() - self.param_arity(block.ty),
                    });
                }
                I::If { block, else_ } => {
                    let cond = self.pop::<bool>();
                    // False with no else fast path
                    if !cond && else_.is_none() {
                        ip = block.end + 1;
                        continue;
                    }

                    self.labels.push(Label {
                        arity: self.return_arity(block.ty),
                        ra: block.end,
                        stack_height: self.stack.len() - self.param_arity(block.ty),
                    });
                    if !cond {
                        if let Some(else_) = else_ {
                            // NOTE: we do intentionally let it fall through to the ip increment
                            ip = else_;
                        }
                    }
                }
                I::Br { depth } => {
                    for _ in 0..depth {
                        self.labels.pop();
                    }
                    // Due to validation, there must be at least one label on the stack, so unwrap
                    // is safe here.
                    let label = self.labels.last().unwrap();
                    ip = label.ra;
                    self.clear_block(label.stack_height, label.arity);
                    continue;
                }
                I::BrTable(br_table) => {
                    let i = self.pop::<u32>();
                    let depth = *br_table
                        .depths
                        .get(i as usize)
                        .unwrap_or(&br_table.default_depth);
                    for _ in 0..depth {
                        self.labels.pop();
                    }
                    let label = self.labels.last().unwrap();
                    ip = label.ra;
                    self.clear_block(label.stack_height, label.arity);
                    continue;
                }
                I::BrIf { depth } => {
                    let cond = self.pop::<bool>();
                    if cond {
                        for _ in 0..depth {
                            self.labels.pop();
                        }
                        let label = self.labels.last().unwrap();
                        ip = label.ra;
                        self.clear_block(label.stack_height, label.arity);
                        continue;
                    }
                }

                // Test instructions
                I::I32Eqz => unop!(eqz for u32),
                I::I64Eqz => unop!(eqz for u64),

                I::I32Clz => unop!(clz for u32),
                I::I32Ctz => unop!(ctz for u32),
                I::I32Popcnt => unop!(popcnt for u32),
                I::I32RemS => binop!(rems for u32, Trap::DivideByZero),
                I::I32RemU => binop!(remu for u32, Trap::DivideByZero),
                I::I32And => binop!(and for u32),
                I::I32Or => binop!(or for u32),
                I::I32Xor => binop!(xor for u32),
                I::I32Shl => binop!(shl for u32),
                I::I32ShrS => binop!(shrs for u32),
                I::I32ShrU => binop!(shru for u32),
                I::I32Rotl => binop!(rotl for u32),
                I::I32Rotr => binop!(rotr for u32),
                I::I64Clz => unop!(clz for u64),
                I::I64Ctz => unop!(ctz for u64),
                I::I64Popcnt => unop!(popcnt for u64),
                I::I64RemS => binop!(rems for u64, Trap::DivideByZero),
                I::I64RemU => binop!(remu for u64, Trap::DivideByZero),
                I::I64And => binop!(and for u64),
                I::I64Or => binop!(or for u64),
                I::I64Xor => binop!(xor for u64),
                I::I64Shl => binop!(shl for u64),
                I::I64ShrS => binop!(shrs for u64),
                I::I64ShrU => binop!(shru for u64),
                I::I64Rotl => binop!(rotl for u64),
                I::I64Rotr => binop!(rotr for u64),

                I::I64Add => binop!(add for u64),
                I::I64Sub => binop!(sub for u64),
                I::I64Mul => binop!(mul for u64),
                I::I64DivS => binop!(divs for u64, Trap::DivideByZero),
                I::I64DivU => binop!(divu for u64, Trap::DivideByZero),
                I::F32Abs => unop!(abs for f32),
                I::F32Neg => unop!(neg for f32),
                I::F32Ceil => unop!(ceil for f32),
                I::F32Floor => unop!(floor for f32),
                I::F32Trunc => unop!(trunc for f32),
                I::F32Nearest => unop!(nearest for f32),
                I::F32Sqrt => unop!(sqrt for f32),
                I::F32Add => binop!(add for f32),
                I::F32Sub => binop!(sub for f32),
                I::F32Mul => binop!(mul for f32),
                I::F32Div => binop!(div for f32),
                I::F32Min => binop!(nan_min for f32),
                I::F32Max => binop!(nan_max for f32),
                I::F32Copysign => binop!(copysign for f32),
                I::F64Abs => unop!(abs for f64),
                I::F64Neg => unop!(neg for f64),
                I::F64Ceil => unop!(ceil for f64),
                I::F64Floor => unop!(floor for f64),
                I::F64Trunc => unop!(trunc for f64),
                I::F64Nearest => unop!(nearest for f64),

                // Comparision instructions
                I::I32Eq => bool_binop!(== for u32),
                I::I32Ne => bool_binop!(!= for u32),
                I::I32LtS => bool_binop!(< for i32),
                I::I32LtU => bool_binop!(< for u32),
                I::I32GtS => bool_binop!(> for i32),
                I::I32GtU => bool_binop!(> for u32),
                I::I32LeS => bool_binop!(<= for i32),
                I::I32LeU => bool_binop!(<= for u32),
                I::I32GeS => bool_binop!(>= for i32),
                I::I32GeU => bool_binop!(>= for u32),
                I::I64Eq => bool_binop!(== for u64),
                I::I64Ne => bool_binop!(!= for u64),
                I::I64LtS => bool_binop!(< for i64),
                I::I64LtU => bool_binop!(< for u64),
                I::I64GtS => bool_binop!(> for i64),
                I::I64GtU => bool_binop!(> for u64),
                I::I64LeS => bool_binop!(<= for i64),
                I::I64LeU => bool_binop!(<= for u64),
                I::I64GeS => bool_binop!(>= for i64),
                I::I64GeU => bool_binop!(>= for u64),
                I::F32Eq => bool_binop!(== for f32),
                I::F32Ne => bool_binop!(!= for f32),
                I::F32Lt => bool_binop!(< for f32),
                I::F32Gt => bool_binop!(> for f32),
                I::F32Le => bool_binop!(<= for f32),
                I::F32Ge => bool_binop!(>= for f32),
                I::F64Eq => bool_binop!(== for f64),
                I::F64Ne => bool_binop!(!= for f64),
                I::F64Lt => bool_binop!(< for f64),
                I::F64Gt => bool_binop!(> for f64),
                I::F64Le => bool_binop!(<= for f64),
                I::F64Ge => bool_binop!(>= for f64),
                I::F64Sqrt => unop!(sqrt for f64),
                I::F64Add => binop!(add for f64),
                I::F64Sub => binop!(sub for f64),
                I::F64Mul => binop!(mul for f64),
                I::F64Div => binop!(div for f64),
                I::F64Min => binop!(nan_min for f64),
                I::F64Max => binop!(nan_max for f64),
                I::F64Copysign => binop!(copysign for f64),

                I::F32Const(f32) => self.push(f32::from_bits(f32.raw())),
                I::F64Const(f64) => self.push(f64::from_bits(f64.raw())),
                I::I64Const(i64) => self.push(i64),

                I::I32WrapI64 => unop!(wrap for u64),
                I::I32TruncF32S => unop!(trunc_i32 for f32, Trap::BadTruncate),
                I::I32TruncF32U => unop!(trunc_u32 for f32, Trap::BadTruncate),
                I::I32TruncF64S => unop!(trunc_i32 for f64, Trap::BadTruncate),
                I::I32TruncF64U => unop!(trunc_u32 for f64, Trap::BadTruncate),
                I::I64ExtendI32S => unop!(to_i64 for u32),
                I::I64ExtendI32U => unop!(to_u64 for u32),
                I::I64TruncF32S => unop!(trunc_i64 for f32, Trap::BadTruncate),
                I::I64TruncF32U => unop!(trunc_u64 for f32, Trap::BadTruncate),
                I::I64TruncF64S => unop!(trunc_i64 for f64, Trap::BadTruncate),
                I::I64TruncF64U => unop!(trunc_u64 for f64, Trap::BadTruncate),
                I::F32ConvertI32S => unop!(convert_f32_s for u32),
                I::F32ConvertI32U => unop!(convert_f32_u for u32),
                I::F32ConvertI64S => unop!(convert_f32_s for u64),
                I::F32ConvertI64U => unop!(convert_f32_u for u64),
                I::F64ConvertI32S => unop!(convert_f64_s for u32),
                I::F64ConvertI32U => unop!(convert_f64_u for u32),
                I::F64ConvertI64S => unop!(convert_f64_s for u64),
                I::F64ConvertI64U => unop!(convert_f64_u for u64),
                I::F32DemoteF64 => unop!(demote for f64),
                I::F64PromoteF32 => unop!(promote for f32),
                I::I32ReinterpretF32 => unop!(reinterpret for u32),
                I::I64ReinterpretF64 => unop!(reinterpret for u64),
                I::F32ReinterpretI32 => unop!(reinterpret for f32),
                I::F64ReinterpretI64 => unop!(reinterpret for f64),
                I::MemorySize => {
                    let addr = self.frame.module.mem_addrs()[0];
                    let mem = &self.store.memories[addr];
                    self.push(mem.size() as u32);
                }
                I::MemoryGrow => {
                    let new_size = self.pop::<u32>();
                    let addr = self.frame.module.mem_addrs()[0];
                    let mem = &mut self.store.memories[addr];
                    if let Some(size) = mem.grow(new_size as usize) {
                        self.push(size as u32);
                    } else {
                        self.push(-1i32);
                    }
                }
                I::MemoryCopy => {
                    let (dst, src, len) = self.pop3::<u32, u32, u32>();
                    let mem = &mut self.store.memories[self.frame.module.mem_addrs()[0]];
                    if src.saturating_add(len) > mem.len() as u32
                        || dst.saturating_add(len) > mem.len() as u32
                    {
                        return Err(Trap::MemoryAccessOutOfBounds.into());
                    }

                    mem.data
                        .copy_within(src as usize..(src + len) as usize, dst as usize);
                }
                I::MemoryFill => {
                    let (dst, val, len) = self.pop3::<u32, u8, u32>();
                    let mem = &mut self.store.memories[self.frame.module.mem_addrs()[0]];
                    if dst.saturating_add(len) > mem.len() as u32 {
                        return Err(Trap::MemoryAccessOutOfBounds.into());
                    }
                    mem.data[dst as usize..(dst + len) as usize].fill(val);
                }
                I::RefIsNull => {
                    let val = self.pop::<ValueUntyped>();
                    self.push(val.is_null());
                }
                I::I32TruncSatF32S => unop!(trunc_i32_sat for f32),
                I::I32TruncSatF32U => unop!(trunc_u32_sat for f32),
                I::I32TruncSatF64S => unop!(trunc_i32_sat for f64),
                I::I32TruncSatF64U => unop!(trunc_u32_sat for f64),
                I::I64TruncSatF32S => unop!(trunc_i64_sat for f32),
                I::I64TruncSatF32U => unop!(trunc_u64_sat for f32),
                I::I64TruncSatF64S => unop!(trunc_i64_sat for f64),
                I::I64TruncSatF64U => unop!(trunc_u64_sat for f64),
                I::I32Extend8S => unop!(extend8_s for u32),
                I::I32Extend16S => unop!(extend16_s for u32),
                I::I64Extend8S => unop!(extend8_s for u64),
                I::I64Extend16S => unop!(extend16_s for u64),
                I::I64Extend32S => {
                    let val = self.pop::<u64>() as i64;
                    self.push(val as i32 as i64);
                }
                I::I32Load(MemArg { offset, align: _ }) => load!(u32, u32, offset),
                I::I64Load(MemArg { offset, align: _ }) => load!(u64, u64, offset),
                I::F32Load(MemArg { offset, align: _ }) => {
                    let offset = self.pop::<u32>().saturating_add(offset);
                    let bytes = self.load(offset)?;
                    self.push(f32::from_bits(u32::from_le_bytes(bytes)));
                }
                I::F64Load(MemArg { offset, align: _ }) => {
                    let offset = self.pop::<u32>().saturating_add(offset);
                    let bytes = self.load(offset)?;
                    self.push(f64::from_bits(u64::from_le_bytes(bytes)));
                }
                I::I32Load8S(MemArg { offset, align: _ }) => load!(i8, u32, offset),
                I::I32Load8U(MemArg { offset, align: _ }) => load!(u8, u32, offset),
                I::I32Load16S(MemArg { offset, align: _ }) => load!(i16, u32, offset),
                I::I32Load16U(MemArg { offset, align: _ }) => load!(u16, u32, offset),
                I::I64Load8S(MemArg { offset, align: _ }) => load!(i8, u64, offset),
                I::I64Load8U(MemArg { offset, align: _ }) => load!(u8, u64, offset),
                I::I64Load16S(MemArg { offset, align: _ }) => load!(i16, u64, offset),
                I::I64Load16U(MemArg { offset, align: _ }) => load!(u16, u64, offset),
                I::I64Load32S(MemArg { offset, align: _ }) => load!(i32, u64, offset),
                I::I64Load32U(MemArg { offset, align: _ }) => load!(u32, u64, offset),
                I::I32Store(MemArg { offset, align: _ }) => store!(u32, offset),
                I::I64Store(MemArg { offset, align: _ }) => store!(u64, offset),
                I::F32Store(MemArg { offset, align: _ }) => {
                    let val = self.pop::<f32>();
                    let offset = self.pop::<u32>() + offset;
                    self.store(offset, val.to_bits().to_le_bytes())?;
                }
                I::F64Store(MemArg { offset, align: _ }) => {
                    let val = self.pop::<f64>();
                    let offset = self.pop::<u32>() + offset;
                    self.store(offset, val.to_bits().to_le_bytes())?;
                }
                I::I32Store8(MemArg { offset, align: _ }) => store!(u8, offset),
                I::I32Store16(MemArg { offset, align: _ }) => store!(u16, offset),
                I::I64Store8(MemArg { offset, align: _ }) => store!(u8, offset),
                I::I64Store16(MemArg { offset, align: _ }) => store!(u16, offset),
                I::I64Store32(MemArg { offset, align: _ }) => store!(u32, offset),
                I::Call { func_idx } => {
                    let f_addr = &self.frame.module.func_addrs()[func_idx as usize];
                    // SAFETY: the pointer is not null because we just coerced it from a reference.
                    // Because functions are never mutated it is safe to think of this as a shared
                    // reference (more or less).
                    //
                    // Additional details can be found in the `call` method.
                    unsafe { self.call_raw(*f_addr) }?;
                }
                I::LocalGet { idx } => {
                    let val = self.stack[self.frame.bp + idx as usize];
                    self.push(val);
                }
                I::LocalSet { idx } => {
                    let val = self.pop();
                    self.stack[self.frame.bp + idx as usize] = val;
                }
                I::LocalTee { idx } => {
                    self.stack[self.frame.bp + (idx as usize)] = *self.stack.last().unwrap();
                }
                I::GlobalGet { idx } => {
                    let addr = self.frame.module.global_addrs()[idx as usize];
                    let val = self.store.globals[addr].value;
                    self.push(val);
                }
                I::GlobalSet { idx } => {
                    let addr = self.frame.module.global_addrs()[idx as usize];
                    let val = self.pop::<ValueUntyped>();
                    let typed = val.into_typed(self.store.globals[addr].value.ty());
                    self.store.globals[addr].value = typed;
                }
                I::DataDrop { data_idx } => {
                    let addr = self.frame.module.data_addrs()[data_idx as usize];
                    self.store.datas[addr].data_drop();
                }
                I::ElemDrop { elem_idx } => {
                    let addr = self.frame.module.elem_addrs()[elem_idx as usize];
                    self.store.elems[addr].elem_drop();
                }
                I::MemoryInit { data_idx } => {
                    let data_addr = self.frame.module.data_addrs()[data_idx as usize];
                    let mem_addr = self.frame.module.mem_addrs()[0];
                    let (dst, src, n) = self.pop3::<u32, u32, u32>();

                    if src.saturating_add(n) > self.store.datas[data_addr].0.len() as u32
                        || dst.saturating_add(n) > self.store.memories[mem_addr].len() as u32
                    {
                        return Err(Trap::MemoryAccessOutOfBounds.into());
                    }

                    let data = &self.store.datas[data_addr].0[src as usize..(src + n) as usize];
                    let mem = &mut self.store.memories[mem_addr];
                    mem.data[dst as usize..(dst + n) as usize].copy_from_slice(data);
                }
                I::RefFunc { func_idx } => {
                    let f = &self.frame.module.func_addrs()[func_idx as usize];
                    self.push(Some(f.as_usize() as u32));
                }
                I::TableGet { table } => {
                    let idx = self.pop::<u32>();
                    let addr = self.frame.module.table_addrs()[table as usize];
                    let table = &self.store.tables[addr];
                    let ref_ = table.get(idx)?;
                    self.push(ref_);
                }
                I::TableSet { table } => {
                    let val = self.pop::<Option<u32>>();
                    let idx = self.pop::<u32>();

                    let addr = self.frame.module.table_addrs()[table as usize];
                    let table = &mut self.store.tables[addr];
                    table.set(idx, val)?;
                }
                I::TableGrow { table } => {
                    let (val, n) = self.pop2::<Ref, u32>();
                    let addr = self.frame.module.table_addrs()[table as usize];
                    let table = &mut self.store.tables[addr];
                    if let Some(size) = table.grow(n, val) {
                        self.push(size);
                    } else {
                        self.push(-1i32);
                    }
                }
                I::TableSize { table } => {
                    let addr = self.frame.module.table_addrs()[table as usize];
                    let table = &self.store.tables[addr];
                    self.push(table.size());
                }
                I::TableFill { table } => {
                    let (start, val, n) = self.pop3::<u32, Ref, u32>();
                    let addr = self.frame.module.table_addrs()[table as usize];
                    let table = &mut self.store.tables[addr];
                    table.fill(start, n, val)?;
                }
                I::RefNull { ty } => {
                    self.push(ValueUntyped::type_default(ty.into()));
                }
                I::CallIndirect {
                    table_idx,
                    type_idx,
                } => {
                    let tbl_idx = self.pop::<u32>();
                    let table_addr = self.frame.module.table_addrs()[table_idx as usize];
                    let table = &self.store.tables[table_addr];
                    let expect_ty = &self.frame.module.types()[type_idx as usize];

                    let ref_ = table.get(tbl_idx)?.ok_or(Trap::CallNullRef)?;
                    let f_addr = Addr::new(ref_ as usize);
                    let f = &self.store.functions[Addr::new(ref_ as usize)];
                    let actual_ty = f.ty();

                    if expect_ty != actual_ty {
                        return Err(Trap::CallIndirectTypeMismatch.into());
                    }

                    // SAFETY: the pointer is not null because we just coerced it from a reference.
                    // Because functions are never mutated it is safe to think of this as a shared
                    // reference (more or less).
                    //
                    // Additional details can be found in the `call` method.
                    unsafe { self.call_raw(f_addr) }?;
                }
                I::TableCopy {
                    src_table,
                    dst_table,
                } => {
                    let src_addr = self.frame.module.table_addrs()[src_table as usize];
                    let dst_addr = self.frame.module.table_addrs()[dst_table as usize];
                    let (dst_start, src_start, n) = self.pop3::<u32, u32, u32>();

                    if src_addr == dst_addr {
                        let tbl = &mut self.store.tables[src_addr];
                        tbl.copy_within(dst_start, src_start, n)?;
                    } else {
                        let (src, dst) = self
                            .store
                            .tables
                            .get_pair_mut(src_addr, dst_addr)
                            .expect("should not fail: same table case already covered");
                        dst.copy(src, dst_start, src_start, n)?;
                    }
                }
                I::TableInit {
                    table_idx,
                    elem_idx,
                } => {
                    let table_addr = self.frame.module.table_addrs()[table_idx as usize];
                    let elem_addr = self.frame.module.elem_addrs()[elem_idx as usize];
                    let (dst_start, src_start, n) = self.pop3::<u32, u32, u32>();
                    let elem = &self.store.elems[elem_addr];
                    let table = &mut self.store.tables[table_addr];

                    table.init(elem, dst_start, src_start, n)?;
                }
            }

            #[cfg(debug_assertions)]
            trace!("executed instruction with stack: {:?}", self.stack);

            ip += 1;
        }
    }

    /// Clears any extra stuff on the stack, leaving only the block's result.
    #[inline(always)]
    fn clear_block(&mut self, begin: usize, arity: usize) {
        // Maybe we should pop until `begin` and then re-push the result? Would need some
        // performance testing
        self.stack.drain(begin..self.stack.len() - arity);
    }

    #[inline(always)]
    fn push(&mut self, val: impl Into<ValueUntyped>) {
        self.stack.push(val.into());
    }

    #[inline(always)]
    fn pop<T: From<ValueUntyped>>(&mut self) -> T {
        self.stack
            .pop()
            .expect("due to validation, stack cannot be empty")
            .into()
    }

    #[inline(always)]
    fn pop2<T, U>(&mut self) -> (T, U)
    where
        T: From<ValueUntyped>,
        U: From<ValueUntyped>,
    {
        let b = self.pop::<U>();
        let a = self.pop::<T>();
        (a, b)
    }

    #[inline(always)]
    fn pop3<T, U, S>(&mut self) -> (T, U, S)
    where
        T: From<ValueUntyped>,
        U: From<ValueUntyped>,
        S: From<ValueUntyped>,
    {
        let c = self.pop::<S>();
        let b = self.pop::<U>();
        let a = self.pop::<T>();
        (a, b, c)
    }

    #[inline(always)]
    fn return_arity(&self, blockty: BlockType) -> usize {
        match blockty {
            BlockType::Empty => 0,
            BlockType::Type(_) => 1,
            BlockType::FuncType(idx) => self.frame.module.types()[idx as usize].1.len(),
        }
    }

    #[inline(always)]
    fn param_arity(&self, blockty: BlockType) -> usize {
        match blockty {
            BlockType::Empty | BlockType::Type(_) => 0,
            BlockType::FuncType(idx) => self.frame.module.types()[idx as usize].0.len(),
        }
    }

    /// Get the `N` bytes of data at the given offset in the current memory.
    #[inline(always)]
    fn load<const N: usize>(&self, offset: u32) -> Result<[u8; N]> {
        let mem = &self.store.memories[self.frame.module.mem_addrs()[0]];
        if offset.saturating_add(N as u32) > mem.len() as u32 {
            return Err(Trap::MemoryAccessOutOfBounds.into());
        }
        let val: [u8; N] = mem.data[offset as usize..offset as usize + N]
            .try_into()
            .unwrap();
        Ok(val)
    }

    /// Store the `N` bytes of data at the given offset in the current memory.
    #[inline(always)]
    fn store<const N: usize>(&mut self, offset: u32, val: [u8; N]) -> Result<()> {
        let mem = &mut self.store.memories[self.frame.module.mem_addrs()[0]];
        if offset as usize + N > mem.len() {
            return Err(Trap::MemoryAccessOutOfBounds.into());
        }
        mem.data[offset as usize..(offset + N as u32) as usize].copy_from_slice(&val);
        Ok(())
    }
}

pub fn eval_const_expr(
    globals: &[Global],
    module_globals: &[Addr<Global>],
    module_funcs: &[Addr<Func>],
    expr: &InitExpr,
) -> ValueUntyped {
    match expr {
        InitExpr::I32Const(i32) => (*i32).into(),
        InitExpr::I64Const(i64) => (*i64).into(),
        InitExpr::F32Const(f32) => f32::from_bits(f32.raw()).into(),
        InitExpr::F64Const(f64) => f64::from_bits(f64.raw()).into(),
        InitExpr::ConstGlobalGet(idx) => globals[module_globals[*idx as usize].as_usize()]
            .value
            .into(),
        InitExpr::RefNull(_) => None.into(),
        InitExpr::RefFunc(idx) => Some(module_funcs[*idx as usize].as_usize() as u32).into(),
    }
}

// Some basic unit tests. Not exhaustive by any means, but spectests cover the rest, along with
// integration tests.
#[cfg(test)]
mod tests {
    use crate::Store;
    use shwasi_parser::{Code, FuncType, Function, Module, ValType};
    use test_log::test;
    use Instruction::*;
    use Value::*;

    use super::*;

    macro_rules! test_function {
        ([$($ret:ident),*] => [$($instr:expr),* $(,)?] $(with locals [$($local:ident * $n:expr),*])?, [$($val:expr),* $(,)?]) => {
            let mut store = Store::default();
            let module = Module {
                types: vec![FuncType(vec![], vec![$(ValType::$ret),*])],
                functions: vec![Function { index: 0 }],
                codes: vec![Code {
                    body: vec![
                        $($instr),*
                    ]
                    .into_iter()
                    .collect::<InstrBuffer>(),
                    locals: vec![
                        $(
                            $(
                                NumLocals {
                                    num: $n,
                                    locals_type: ValType::$local,
                                }
                             ),*
                         )?
                    ],
                }],
                ..Default::default()
            };
            let inst = Instance::instantiate(&mut store, module).unwrap();
            let mut vm = Vm::new(&mut store, inst.clone());
            let res = vm.call(Addr::default(), []).unwrap();
            let expect: Vec<Value> = vec![$($val),*];
            assert_eq!(expect, res);
        };
    }

    #[test]
    fn i32_stack_arith() {
        test_function!(
            [I32] => [
                I32Const(1),
                I32Const(2),
                I32Add,
                Return,
                End,
            ],
            [
              I32(3),
            ]
        );
    }

    #[test]
    fn branch_outer() {
        test_function!(
            [] => [
                Loop(shwasi_parser::Block {
                    ty: BlockType::Empty,
                    end: 2,
                }),
                Br { depth: 1 },
                End,
                End,
            ],
            []
        );
    }

    #[test]
    fn br_if() {
        test_function!(
            [] => [
                Loop(shwasi_parser::Block {
                    ty: BlockType::Empty,
                    end: 3,
                }),
                I32Const(1),
                BrIf { depth: 1 },
                End,
                End,
            ],
            []
        );
    }

    #[test]
    fn blocks_basic() {
        test_function!(
            [I32] => [
                Block(shwasi_parser::Block {
                    ty: BlockType::Type(ValType::I32),
                    end: 2,
                }),
                I32Const(1),
                End,
                End,
            ],
            [
                I32(1),
            ]
        );
    }

    #[test]
    fn if_block() {
        test_function!(
            [I32] => [
                I32Const(1),
                If {
                    block: shwasi_parser::Block {
                        ty: BlockType::Type(ValType::I32),
                        end: 5,
                    },
                    else_: Some(3)
                },
                I32Const(1),
                Else,
                I32Const(2),
                End,
                End,
            ],
            [
                I32(1),
            ]
        );
    }
}
