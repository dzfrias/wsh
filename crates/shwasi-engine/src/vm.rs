#![allow(dead_code)]

use std::{mem, rc::Rc};

use shwasi_parser::{BlockType, FuncType, InitExpr, InstrBuffer, Instruction, NumLocals, RefType};
use thiserror::Error;

use crate::{
    ops::*,
    store::{Addr, GlobalInst},
    values::Value,
    FuncInst, Instance, StoreData, StoreMut,
};

#[derive(Debug)]
pub struct Vm<'s> {
    /// The stack of values. This is used to store locals, arguments, and intermediate values.
    stack: Vec<Value>,
    /// A "stack" of frames.
    ///
    /// Note that this is note actually a stack, as it has no reason to be.
    /// Because only one frame can be active at a time, we can just keep track of the current frame
    /// and replace it with the old frame when the function call ends.
    frame: StackFrame,
    /// Stack of labels.
    ///
    /// This is used to keep track of the arity of blocks, and their return
    /// address when they're finished. For a `loop` block, the return address is the beginning of
    /// the block. For other blocks, the return address is the end of the block. The return address
    /// should be jumped to by the VM when a block is finished or a branch to that block is taken.
    labels: Vec<Label>,
    store: &'s StoreData,
    store_mut: &'s mut StoreMut,
}

/// The maximum number of nested levels we can have on the frame stack.
const STACK_BUDGET: usize = 300;

/// A WebAssembly stack frame.
///
/// See [`Vm`] for more information on how this is used.
#[derive(Debug)]
struct StackFrame {
    /// The module this frame is for.
    module: Rc<Instance>,
    /// Used to get locals and arguments.
    bp: usize,
    /// Used as a counter to make sure we don't overflow the stack (our budget is STACK_BUDGET).
    /// This starts at STACK_BUDGET and decrements every time we push a new frame. If it reaches
    /// zero, we cannot push a new frame.
    nested_levels: usize,
}

impl StackFrame {
    fn new(module: Rc<Instance>) -> Self {
        Self {
            module,
            bp: 0,
            nested_levels: STACK_BUDGET,
        }
    }

    // Because we don't actually have a real stack, this push operation is a bit different.
    fn push(&self, module: Rc<Instance>, bp: usize) -> Option<Self> {
        Some(Self {
            module,
            bp,
            // Cannot push if our stack budget is exhausted
            nested_levels: self.nested_levels.checked_sub(1)?,
        })
    }
}

#[derive(Debug)]
struct Label {
    arity: usize,
    /// Return address of the block.
    ra: usize,
    /// The stack height when the label was pushed
    stack_height: usize,
}

#[derive(Debug, Error)]
pub enum Trap {
    #[error("stack overflow")]
    StackOverflow,
    #[error("unreachable encountered")]
    Unreachable,
    #[error("division by zero")]
    DivideByZero,
    #[error("bad float truncation")]
    BadTruncate,
    #[error("table get out of bounds: {index} >= {table_size}")]
    TableGetOutOfBounds { index: u32, table_size: u32 },
    #[error("call indirect type mismatch: expected {expected}, got {got}")]
    CallIndirectTypeMismatch { expected: FuncType, got: FuncType },
    #[error("attempted to call a null reference")]
    CallNullRef,
}

pub type Result<T> = std::result::Result<T, Trap>;

impl<'s> Vm<'s> {
    pub fn new(store: &'s StoreData, store_mut: &'s mut StoreMut, module: Rc<Instance>) -> Self {
        Self {
            stack: vec![],
            frame: StackFrame::new(module),
            labels: vec![],
            store,
            store_mut,
        }
    }

    pub fn call(&mut self, f: &FuncInst) -> Result<()> {
        match f {
            FuncInst::Module(f) => {
                // Args should already be on the stack (due to validation), so push locals
                let mut pushed_locals = 0;
                for NumLocals { num, locals_type } in &f.code.locals {
                    for _ in 0..*num {
                        // Locals are initialized to their default value
                        self.push(Value::type_default(*locals_type));
                        pushed_locals += 1;
                    }
                }

                // Base pointer, used to get locals and arguments
                let bp = self.stack.len() - f.ty.0.len() - pushed_locals;
                self.labels.push(Label {
                    arity: f.ty.1.len(),
                    ra: f.code.body.len(),
                    stack_height: self.stack.len(),
                });
                let new_frame = self
                    .frame
                    .push(Rc::clone(&f.inst), bp)
                    .ok_or(Trap::StackOverflow)?;
                // Keep track of the old frame to replace later. This is what emulate a call stack
                let old_frame = mem::replace(&mut self.frame, new_frame);
                self.execute(&f.code.body)?;
                self.frame = old_frame;

                // Clear locals and arguments. Other stuff from the block should have already been
                // cleared (due to the mandatory `End` instruction at the end of functions).
                self.clear_block(bp, f.ty.1.len());
            }
            FuncInst::Host(_) => todo!("host functions"),
        }

        Ok(())
    }

    /// Executes a set of instructions.
    fn execute(&mut self, body: &InstrBuffer) -> Result<()> {
        use Instruction as I;

        macro_rules! bool_binop {
            ($op:tt for $conv:ident) => {{
                let (a, b) = self.pop2();
                self.push(a.$conv() $op b.$conv());
            }};
        }
        macro_rules! binop {
            ($method:ident for $conv:ident$(, $err:expr)?) => {{
                let (a, b) = self.pop2();
                self.push(a.$conv().$method(b.$conv())$(.ok_or($err)?)?);
            }};
        }
        macro_rules! unop {
            ($method:ident for $conv:ident$(, $err:expr)?) => {{
                let a = self.pop();
                self.push(a.$conv().$method()$(.ok_or($err)?)?);
            }};
        }

        // Our instruction pointer. We don't use a for loop because we need to be able to jump to
        // instructions.
        let mut ip = 0;
        while ip < body.len() {
            // TODO: unchecked here?
            let instr = &body.get(ip).unwrap();

            match instr {
                I::Nop => {}
                I::Unreachable => return Err(Trap::Unreachable),
                I::I32Const(i32) => self.push(Value::I32(*i32)),
                I::I32Add => binop!(add for as_u32),
                I::I32Sub => binop!(sub for as_u32),
                I::I32Mul => binop!(mul for as_u32),
                I::I32DivS => binop!(divs for as_u32, Trap::DivideByZero),
                I::I32DivU => binop!(divu for as_u32, Trap::DivideByZero),
                I::End => {
                    let label = self.labels.pop().unwrap();
                    // Jump to continuation, which is the return address of the block that is found
                    // at compile time.
                    ip = label.ra;
                    self.clear_block(label.stack_height, label.arity);
                    continue;
                }
                I::Else => {
                    let label = self.labels.pop().unwrap();
                    // We jump to continuation on an else block because if we receive this
                    // instruction, it means that the `If` block main body was executed, so treat
                    // this as an `End` instruction.
                    ip = label.ra;
                    continue;
                }
                I::Return => return Ok(()),
                I::Drop => drop(self.stack.pop()),
                I::Select | I::SelectT(_) => {
                    let (a, b, cond) = self.pop3();
                    self.push(if cond.is_true() { a } else { b });
                }

                // Control instructions
                I::Loop(block) => {
                    self.labels.push(Label {
                        arity: self.param_arity(block.ty),
                        // Return to the beginning of the block
                        ra: ip,
                        stack_height: self.stack.len(),
                    });
                }
                I::Block(block) => {
                    self.labels.push(Label {
                        arity: self.return_arity(block.ty),
                        ra: block.end,
                        stack_height: self.stack.len(),
                    });
                }
                I::If { block, else_ } => {
                    let cond = self.pop();
                    self.labels.push(Label {
                        arity: self.return_arity(block.ty),
                        ra: block.end,
                        stack_height: self.stack.len(),
                    });
                    if cond.is_false() {
                        if let Some(else_) = else_ {
                            // NOTE: we do intentionally let it fall through to the ip increment
                            ip = *else_;
                        } else {
                            // If there is no else block, we need to jump to the normal end
                            // continuation.
                            let label = self.labels.pop().unwrap();
                            ip = label.ra;
                            self.clear_block(label.stack_height, label.arity);
                            continue;
                        }
                    }
                }
                I::Br { depth } => {
                    for _ in 0..*depth {
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
                    let i = self.pop().as_u32();
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
                    let cond = self.pop();
                    if cond.is_true() {
                        for _ in 0..*depth {
                            self.labels.pop();
                        }
                        let label = self.labels.last().unwrap();
                        ip = label.ra;
                        self.clear_block(label.stack_height, label.arity);
                        continue;
                    }
                }

                // Test instructions
                I::I32Eqz => unop!(eqz for as_u32),
                I::I64Eqz => unop!(eqz for as_u64),

                I::I32Clz => unop!(clz for as_u32),
                I::I32Ctz => unop!(ctz for as_u32),
                I::I32Popcnt => unop!(popcnt for as_u32),
                I::I32RemS => binop!(rems for as_u32, Trap::DivideByZero),
                I::I32RemU => binop!(remu for as_u32, Trap::DivideByZero),
                I::I32And => binop!(and for as_u32),
                I::I32Or => binop!(or for as_u32),
                I::I32Xor => binop!(xor for as_u32),
                I::I32Shl => binop!(shl for as_u32),
                I::I32ShrS => binop!(shrs for as_u32),
                I::I32ShrU => binop!(shru for as_u32),
                I::I32Rotl => binop!(rotl for as_u32),
                I::I32Rotr => binop!(rotr for as_u32),
                I::I64Clz => unop!(clz for as_u64),
                I::I64Ctz => unop!(ctz for as_u64),
                I::I64Popcnt => unop!(popcnt for as_u64),
                I::I64RemS => binop!(rems for as_u64, Trap::DivideByZero),
                I::I64RemU => binop!(remu for as_u64, Trap::DivideByZero),
                I::I64And => binop!(and for as_u64),
                I::I64Or => binop!(or for as_u64),
                I::I64Xor => binop!(xor for as_u64),
                I::I64Shl => binop!(shl for as_u64),
                I::I64ShrS => binop!(shrs for as_u64),
                I::I64ShrU => binop!(shru for as_u64),
                I::I64Rotl => binop!(rotl for as_u64),
                I::I64Rotr => binop!(rotr for as_u64),

                I::I64Add => binop!(add for as_u64),
                I::I64Sub => binop!(sub for as_u64),
                I::I64Mul => binop!(mul for as_u64),
                I::I64DivS => binop!(divs for as_u64, Trap::DivideByZero),
                I::I64DivU => binop!(divu for as_u64, Trap::DivideByZero),
                I::F32Abs => unop!(abs for as_f32),
                I::F32Neg => unop!(neg for as_f32),
                I::F32Ceil => unop!(ceil for as_f32),
                I::F32Floor => unop!(floor for as_f32),
                I::F32Trunc => unop!(trunc for as_f32),
                I::F32Nearest => unop!(nearest for as_f32),
                I::F32Sqrt => unop!(sqrt for as_f32),
                I::F32Add => binop!(add for as_f32),
                I::F32Sub => binop!(sub for as_f32),
                I::F32Mul => binop!(mul for as_f32),
                I::F32Div => binop!(div for as_f32),
                I::F32Min => binop!(min for as_f32),
                I::F32Max => binop!(max for as_f32),
                I::F32Copysign => binop!(copysign for as_f32),
                I::F64Abs => unop!(abs for as_f64),
                I::F64Neg => unop!(neg for as_f64),
                I::F64Ceil => unop!(ceil for as_f64),
                I::F64Floor => unop!(floor for as_f64),
                I::F64Trunc => unop!(trunc for as_f64),
                I::F64Nearest => unop!(nearest for as_f64),

                // Comparision instructions
                I::I32Eq => bool_binop!(== for as_u32),
                I::I32Ne => bool_binop!(!= for as_u32),
                I::I32LtS => bool_binop!(< for as_i32),
                I::I32LtU => bool_binop!(< for as_u32),
                I::I32GtS => bool_binop!(> for as_i32),
                I::I32GtU => bool_binop!(> for as_u32),
                I::I32LeS => bool_binop!(<= for as_i32),
                I::I32LeU => bool_binop!(<= for as_u32),
                I::I32GeS => bool_binop!(>= for as_i32),
                I::I32GeU => bool_binop!(>= for as_u32),
                I::I64Eq => bool_binop!(== for as_u64),
                I::I64Ne => bool_binop!(!= for as_u64),
                I::I64LtS => bool_binop!(< for as_i64),
                I::I64LtU => bool_binop!(< for as_u64),
                I::I64GtS => bool_binop!(> for as_i64),
                I::I64GtU => bool_binop!(> for as_u64),
                I::I64LeS => bool_binop!(<= for as_i64),
                I::I64LeU => bool_binop!(<= for as_u64),
                I::I64GeS => bool_binop!(>= for as_i64),
                I::I64GeU => bool_binop!(>= for as_u64),
                I::F32Eq => bool_binop!(== for as_f32),
                I::F32Ne => bool_binop!(!= for as_f32),
                I::F32Lt => bool_binop!(< for as_f32),
                I::F32Gt => bool_binop!(> for as_f32),
                I::F32Le => bool_binop!(<= for as_f32),
                I::F32Ge => bool_binop!(>= for as_f32),
                I::F64Eq => bool_binop!(== for as_f64),
                I::F64Ne => bool_binop!(!= for as_f64),
                I::F64Lt => bool_binop!(< for as_f64),
                I::F64Gt => bool_binop!(> for as_f64),
                I::F64Le => bool_binop!(<= for as_f64),
                I::F64Ge => bool_binop!(>= for as_f64),
                I::F64Sqrt => unop!(sqrt for as_f64),
                I::F64Add => binop!(add for as_f64),
                I::F64Sub => binop!(sub for as_f64),
                I::F64Mul => binop!(mul for as_f64),
                I::F64Div => binop!(div for as_f64),
                I::F64Min => binop!(min for as_f64),
                I::F64Max => binop!(max for as_f64),
                I::F64Copysign => binop!(copysign for as_f64),

                I::F32Const(f32) => self.push(f32::from_bits(f32.raw())),
                I::F64Const(f64) => self.push(f64::from_bits(f64.raw())),
                I::I64Const(i64) => self.push(Value::I64(*i64)),

                I::I32WrapI64 => unop!(wrap for as_u64),
                I::I32TruncF32S => unop!(trunc_i32 for as_f32, Trap::BadTruncate),
                I::I32TruncF32U => unop!(trunc_u32 for as_f32, Trap::BadTruncate),
                I::I32TruncF64S => unop!(trunc_i32 for as_f64, Trap::BadTruncate),
                I::I32TruncF64U => unop!(trunc_u32 for as_f64, Trap::BadTruncate),
                I::I64ExtendI32S => unop!(to_i64 for as_u32),
                I::I64ExtendI32U => unop!(to_u64 for as_u32),
                I::I64TruncF32S => unop!(trunc_i64 for as_f32, Trap::BadTruncate),
                I::I64TruncF32U => unop!(trunc_u64 for as_f32, Trap::BadTruncate),
                I::I64TruncF64S => unop!(trunc_i64 for as_f64, Trap::BadTruncate),
                I::I64TruncF64U => unop!(trunc_u64 for as_f64, Trap::BadTruncate),
                I::F32ConvertI32S => unop!(convert_f32_s for as_u32),
                I::F32ConvertI32U => unop!(convert_f32_u for as_u32),
                I::F32ConvertI64S => unop!(convert_f32_s for as_u64),
                I::F32ConvertI64U => unop!(convert_f32_u for as_u64),
                I::F64ConvertI32S => unop!(convert_f64_s for as_u32),
                I::F64ConvertI32U => unop!(convert_f64_u for as_u32),
                I::F64ConvertI64S => unop!(convert_f64_s for as_u64),
                I::F64ConvertI64U => unop!(convert_f64_u for as_u64),
                I::F32DemoteF64 => unop!(demote for as_f64),
                I::F64PromoteF32 => unop!(promote for as_f32),
                I::I32ReinterpretF32 => unop!(reinterpret for as_u32),
                I::I64ReinterpretF64 => unop!(reinterpret for as_u64),
                I::F32ReinterpretI32 => unop!(reinterpret for as_f32),
                I::F64ReinterpretI64 => unop!(reinterpret for as_f64),
                I::MemorySize => {
                    let addr = self.frame.module.mem_addrs[0];
                    let mem = &self.store_mut.memories[addr];
                    self.push(mem.size() as u32);
                }
                I::MemoryGrow => {
                    let new_size = self.pop().as_u32();
                    let addr = self.frame.module.mem_addrs[0];
                    let mem = &mut self.store_mut.memories[addr];
                    let old_size = mem.size();
                    if mem.grow(new_size as usize).is_some() {
                        self.push(old_size as u32);
                    } else {
                        self.push(-1);
                    }
                }
                I::MemoryCopy => todo!(),
                I::MemoryFill => todo!(),
                I::RefIsNull => {
                    let val = self.pop();
                    self.push(val.is_null());
                }
                I::I32TruncSatF32S => unop!(trunc_i32_sat for as_f32),
                I::I32TruncSatF32U => unop!(trunc_u32_sat for as_f32),
                I::I32TruncSatF64S => unop!(trunc_i32_sat for as_f64),
                I::I32TruncSatF64U => unop!(trunc_u32_sat for as_f64),
                I::I64TruncSatF32S => unop!(trunc_i64_sat for as_f32),
                I::I64TruncSatF32U => unop!(trunc_u64_sat for as_f32),
                I::I64TruncSatF64S => unop!(trunc_i64_sat for as_f64),
                I::I64TruncSatF64U => unop!(trunc_u64_sat for as_f64),
                I::I32Extend8S => todo!(),
                I::I32Extend16S => todo!(),
                I::I64Extend8S => todo!(),
                I::I64Extend16S => todo!(),
                I::I64Extend32S => todo!(),
                I::I32Load(_) => todo!(),
                I::I64Load(_) => todo!(),
                I::F32Load(_) => todo!(),
                I::F64Load(_) => todo!(),
                I::I32Load8S(_) => todo!(),
                I::I32Load8U(_) => todo!(),
                I::I32Load16S(_) => todo!(),
                I::I32Load16U(_) => todo!(),
                I::I64Load8S(_) => todo!(),
                I::I64Load8U(_) => todo!(),
                I::I64Load16S(_) => todo!(),
                I::I64Load16U(_) => todo!(),
                I::I64Load32S(_) => todo!(),
                I::I64Load32U(_) => todo!(),
                I::I32Store(_) => todo!(),
                I::I64Store(_) => todo!(),
                I::F32Store(_) => todo!(),
                I::F64Store(_) => todo!(),
                I::I32Store8(_) => todo!(),
                I::I32Store16(_) => todo!(),
                I::I64Store8(_) => todo!(),
                I::I64Store16(_) => todo!(),
                I::I64Store32(_) => todo!(),
                I::Call { func_idx } => {
                    let f = &self.frame.module.func_addrs[*func_idx as usize];
                    let f = &self.store.functions[*f];
                    self.call(f)?;
                }
                I::LocalGet { idx } => {
                    let val = self.stack[self.frame.bp + *idx as usize];
                    self.push(val);
                }
                I::LocalSet { idx } => {
                    let val = self.pop();
                    self.stack[self.frame.bp + *idx as usize] = val;
                }
                I::LocalTee { idx } => {
                    self.stack[self.frame.bp + (*idx as usize)] = *self.stack.last().unwrap();
                }
                I::GlobalGet { idx } => {
                    let addr = self.frame.module.global_addrs[*idx as usize];
                    let val = self.store_mut.globals[addr].value;
                    self.push(val);
                }
                I::GlobalSet { idx } => {
                    let addr = self.frame.module.global_addrs[*idx as usize];
                    let val = self.pop();
                    self.store_mut.globals[addr].value = val;
                }
                I::DataDrop { data_idx } => {
                    let addr = self.frame.module.data_addrs[*data_idx as usize];
                    self.store_mut.datas[addr].data_drop();
                }
                I::ElemDrop { elem_idx } => {
                    let addr = self.frame.module.elem_addrs[*elem_idx as usize];
                    self.store_mut.elems[addr].elem_drop();
                }
                I::MemoryInit { .. } => todo!(),
                I::RefFunc { func_idx } => {
                    let f = &self.frame.module.func_addrs[*func_idx as usize];
                    self.push(Value::Ref(*f));
                }
                I::TableGet { table } => {
                    let idx = self.pop().as_u32();
                    let addr = self.frame.module.table_addrs[*table as usize];
                    let table = &self.store_mut.tables[addr];
                    let ref_ =
                        table
                            .elements
                            .get(idx as usize)
                            .ok_or(Trap::TableGetOutOfBounds {
                                table_size: table.size() as u32,
                                index: idx,
                            })?;
                    let val = match (*ref_, table.ty.elem_type) {
                        (Some(ref_), RefType::Func) => Value::Ref(ref_),
                        (Some(ref_), RefType::Extern) => Value::ExternRef(ref_),
                        (None, ty) => Value::NullRef(ty),
                    };
                    self.push(val);
                }
                I::TableSet { table } => {
                    let val = self.pop();
                    let idx = self.pop().as_u32();

                    let addr = self.frame.module.table_addrs[*table as usize];
                    let table = &mut self.store_mut.tables[addr];
                    let size = table.size();
                    *table
                        .elements
                        .get_mut(idx as usize)
                        .ok_or(Trap::TableGetOutOfBounds {
                            table_size: size as u32,
                            index: idx,
                        })? = match val {
                        Value::NullRef(_) => None,
                        Value::ExternRef(r) | Value::Ref(r) => Some(r),
                        _ => unreachable!("BUG: due to validation, this should never happen"),
                    };
                }
                I::TableGrow { .. } => todo!(),
                I::TableSize { table } => {
                    let addr = self.frame.module.table_addrs[*table as usize];
                    let table = &self.store_mut.tables[addr];
                    self.push(table.elements.len() as u32);
                }
                I::TableFill { .. } => todo!(),
                I::RefNull { ty } => {
                    self.push(Value::NullRef(*ty));
                }
                I::CallIndirect {
                    table_idx,
                    type_idx,
                } => {
                    let tbl_idx = self.pop().as_u32();
                    let table_addr = self.frame.module.table_addrs[*table_idx as usize];
                    let table = &self.store_mut.tables[table_addr];
                    let expect_ty = &self.frame.module.types[*type_idx as usize];

                    let f_addr = table
                        .elements
                        .get(tbl_idx as usize)
                        .ok_or(Trap::TableGetOutOfBounds {
                            table_size: table.size() as u32,
                            index: tbl_idx,
                        })?
                        .ok_or(Trap::CallNullRef)?;
                    let f = &self.store.functions[f_addr];
                    let actual_ty = match f {
                        FuncInst::Host(host) => &host.ty,
                        FuncInst::Module(module) => &module.ty,
                    };

                    if expect_ty != actual_ty {
                        return Err(Trap::CallIndirectTypeMismatch {
                            expected: expect_ty.clone(),
                            got: actual_ty.clone(),
                        });
                    }

                    self.call(f)?;
                }
                I::TableCopy {
                    src_table,
                    dst_table,
                } => {
                    let src_addr = self.frame.module.table_addrs[*src_table as usize];
                    let dst_addr = self.frame.module.table_addrs[*dst_table as usize];
                    let n = self.pop().as_u32();
                    let src_start = self.pop().as_u32();
                    let dst_start = self.pop().as_u32();
                    let src_table = &self.store_mut.tables[src_addr];
                    let dst_table = &self.store_mut.tables[dst_addr];

                    if src_start + n > src_table.size() as u32
                        || dst_start + n > dst_table.size() as u32
                    {
                        todo!("err")
                    }

                    // TODO: no clone here
                    let src =
                        src_table.elements[src_start as usize..(src_start + n) as usize].to_vec();
                    self.store_mut.tables[dst_addr].elements
                        [dst_start as usize..(dst_start + n) as usize]
                        .copy_from_slice(&src);
                }
                I::TableInit { .. } => todo!(),
            }

            ip += 1;
        }

        Ok(())
    }

    /// Clears any extra stuff on the stack, leaving only the block's result.
    fn clear_block(&mut self, begin: usize, arity: usize) {
        // Maybe we should pop until `begin` and then re-push the result? Would need some
        // performance testing
        self.stack.drain(begin..self.stack.len() - arity);
    }

    fn push(&mut self, val: impl Into<Value>) {
        self.stack.push(val.into());
    }

    // TODO: inline?
    fn pop(&mut self) -> Value {
        self.stack
            .pop()
            .expect("due to validation, stack cannot be empty")
    }

    fn pop2(&mut self) -> (Value, Value) {
        let b = self.pop();
        let a = self.pop();
        (a, b)
    }

    fn pop3(&mut self) -> (Value, Value, Value) {
        let c = self.pop();
        let b = self.pop();
        let a = self.pop();
        (a, b, c)
    }

    fn return_arity(&self, blockty: BlockType) -> usize {
        match blockty {
            BlockType::Empty => 0,
            BlockType::Type(_) => 1,
            BlockType::FuncType(idx) => self.frame.module.types[idx as usize].1.len(),
        }
    }

    fn param_arity(&self, blockty: BlockType) -> usize {
        match blockty {
            BlockType::Empty | BlockType::Type(_) => 0,
            BlockType::FuncType(idx) => self.frame.module.types[idx as usize].0.len(),
        }
    }
}

pub fn eval_const_expr(globals: &[GlobalInst], module_globals: &[Addr], expr: &InitExpr) -> Value {
    match expr {
        InitExpr::I32Const(i32) => Value::I32(*i32),
        InitExpr::I64Const(i64) => Value::I64(*i64),
        InitExpr::F32Const(f32) => Value::F32(f32::from_bits(f32.raw())),
        InitExpr::F64Const(f64) => Value::F64(f64::from_bits(f64.raw())),
        InitExpr::ConstGlobalGet(idx) => globals[module_globals[*idx as usize]].value,
        InitExpr::RefNull(t) => Value::NullRef(*t),
        InitExpr::RefFunc(idx) => Value::Ref(*idx as usize),
    }
}

// Some basic unit tests. Not exhaustive by any means, but spectests cover the rest, along with
// integration tests.
#[cfg(test)]
mod tests {
    use crate::Store;
    use shwasi_parser::{Code, FuncType, Function, Module, ValType};
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
            let inst = Instance::instantiate(module, &mut store, &[]).unwrap();
            let mut vm = Vm::new(&store.data, &mut store.mut_, Rc::clone(&inst));
            vm.call(&store.data.functions[0]).unwrap();
            let expect: Vec<Value> = vec![$($val),*];
            assert_eq!(expect, vm.stack);
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
                    end: 0,
                }),
                Br { depth: 1 },
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
                    end: 0,
                }),
                I32Const(1),
                BrIf { depth: 1 },
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
            ],
            [
                I32(1),
            ]
        );
    }

    #[test]
    fn locals() {
        test_function!(
            [I32] => [
                Loop(shwasi_parser::Block {
                    ty: BlockType::Empty,
                    end: 0,
                }),
                    LocalGet { idx: 0 },
                    I32Const(1),
                    I32Add,
                    LocalSet { idx: 0 },
                    LocalGet { idx: 0 },
                    I32Const(10),
                    I32Eq,
                    If {
                        block: shwasi_parser::Block {
                            ty: BlockType::Empty,
                            end: 11,
                        },
                        else_: None,
                    },
                        LocalGet { idx: 0 },
                        Return,
                    End,
                End,
            ] with locals [I32 * 1],
            [
                I32(10),
            ]
        );
    }
}
