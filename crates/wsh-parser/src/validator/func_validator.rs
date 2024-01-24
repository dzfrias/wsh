use std::{borrow::Cow, fmt, iter};

use anyhow::{ensure, Context, Result};
#[cfg(debug_assertions)]
use tracing::trace;
use tracing::{debug, instrument};

use crate::{
    validator::Validator, Block, BlockType, Code, Function, Instruction, MemArg, RefType, ValType,
};

// Necessary to distinguish the two because the base funtion frame should have no param types
#[derive(Debug, Clone, Copy)]
enum FrameType {
    Block(BlockType),
    Function(u32),
}

/// A WebAssembly control frame.
#[derive(Debug)]
struct Frame {
    ty: FrameType,
    init_height: usize,
    /// Handles stack-polymorphic typing.
    unreachable: bool,
    kind: FrameKind,
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum FrameKind {
    Block,
    Loop,
    If,
    Else,
    Function,
}

/// The outermost frame of validation, which contains the function's locals and result type.
#[derive(Debug)]
struct FuncCtx<'a> {
    locals: Vec<ValType>,
    result: &'a [ValType],
}

/// An operand on the value stack.
///
/// [`Operand::Any`] is used for value-polymorphic instructions like [`Instruction::Select`].
#[derive(Debug, PartialEq, Clone, Copy)]
enum Operand {
    Any,
    Exact(ValType),
}

impl Operand {
    #[must_use]
    fn matches(&self, other: Operand) -> bool {
        let Operand::Exact(self_ty) = self else {
            return true;
        };
        let Operand::Exact(other_ty) = other else {
            return true;
        };

        *self_ty == other_ty
    }

    #[must_use]
    fn is_any(&self) -> bool {
        matches!(self, Self::Any)
    }

    #[must_use]
    pub fn is_ref(&self) -> bool {
        matches!(self, Self::Exact(ValType::Func | ValType::Extern))
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Any => write!(f, "any"),
            Operand::Exact(ty) => write!(f, "{ty}"),
        }
    }
}

impl From<ValType> for Operand {
    fn from(vt: ValType) -> Self {
        Operand::Exact(vt)
    }
}

impl From<&ValType> for Operand {
    fn from(vt: &ValType) -> Self {
        Operand::Exact(*vt)
    }
}

/// A validator for a single function.
#[derive(Debug)]
pub struct FuncValidator<'a> {
    validator: &'a Validator<'a>,

    // TODO: reuse the same vector across all functions?
    frames: Vec<Frame>,
    vals: Vec<Operand>,
    current_func: FuncCtx<'a>,
}

impl<'a> FuncValidator<'a> {
    /// Create a function validator, using a shared reference to the module validator.
    pub fn new(validator: &'a Validator<'a>) -> Self {
        Self {
            validator,
            frames: Vec::new(),
            vals: Vec::new(),
            current_func: FuncCtx {
                locals: Vec::new(),
                result: &[],
            },
        }
    }

    /// Validate a function.
    pub fn validate(&mut self, code: &Code, func: &Function) -> Result<()> {
        debug!("started validating function with index: {func}");

        let ty = self
            .validator
            .module_tys
            .get(func.index as usize)
            .context("code section type not found")?;
        // Locals begin with function parameters
        self.current_func.locals = ty.0.to_vec();
        self.current_func
            .locals
            // Should be flattened because `code.locals` contains the number of locals of a
            // certain type
            .extend(code.locals.iter().flat_map(|num_locals| {
                iter::repeat(num_locals.locals_type).take(num_locals.num as usize)
            }));
        self.current_func.result = &ty.1;
        debug!("current function: {:?}", self.current_func);
        self.push_func_frame(func.index)?;

        for instr in &code.body {
            self.validate_instr(instr)?;
        }

        Ok(())
    }

    fn push_vals(&mut self, label_ty: &[ValType]) {
        self.vals.reserve(label_ty.len());
        for val in label_ty {
            self.push_val(Operand::Exact(*val));
        }
    }

    #[inline]
    fn push_val(&mut self, val: Operand) {
        self.vals.push(val);
    }

    fn pop_val(&mut self, expected: Option<ValType>) -> Result<Operand> {
        // Since most instructions will pop a value, we optimize for that case by eagerly popping.
        // If this assumption is incorrect, we go to the slow path.
        //
        // Credits to wasmparser for this optimization.
        let popped = match self.vals.pop() {
            Some(Operand::Exact(actual_ty)) => {
                if Some(actual_ty) == expected {
                    if let Some(control) = self.frames.last() {
                        if self.vals.len() >= control.init_height {
                            return Ok(Operand::Exact(actual_ty));
                        }
                    }
                }
                Some(Operand::Exact(actual_ty))
            }
            other => other,
        };

        self._pop_val(expected, popped)
    }

    /// Cold path of `pop_val`.
    #[cold]
    fn _pop_val(&mut self, expected: Option<ValType>, popped: Option<Operand>) -> Result<Operand> {
        self.vals.extend(popped);

        let last_frame = self.frames.last_mut().expect("should have a frame");
        if self.vals.len() == last_frame.init_height && last_frame.unreachable {
            return Ok(Operand::Any);
        }
        // Value stack must not be empty
        ensure!(
            self.vals.len() > last_frame.init_height,
            "stack height mismatch when popping value: expected > {}, got {}",
            last_frame.init_height,
            self.vals.len()
        );
        let got = self
            .vals
            .pop()
            .expect("should have an operand on the stack");

        let Operand::Exact(got) = got else {
            return Ok(Operand::Any);
        };
        let Some(expected) = expected else {
            return Ok(Operand::Exact(got));
        };
        ensure!(got == expected, "unexpected type: {expected} != {got}");

        Ok(Operand::Exact(got))
    }

    fn pop_vals(&mut self, expected: &[ValType]) -> Result<()> {
        for expected in expected.iter().rev() {
            self.pop_val(Some(*expected))?;
        }

        Ok(())
    }

    /// Version of `expect_vals` that collects the values that are popped. This is kept as a
    /// seperate function for optimization purposes.
    fn pop_vals_collect(&mut self, expected: &[ValType]) -> Result<Vec<Operand>> {
        let mut operands = vec![];
        for expected in expected.iter().rev() {
            let op = self.pop_val(Some(*expected))?;
            operands.push(op);
        }

        Ok(operands)
    }

    #[instrument(level = "debug", skip(self))]
    fn push_frame(&mut self, kind: FrameKind, blockty: BlockType) -> Result<()> {
        self.frames.push(Frame {
            ty: FrameType::Block(blockty),
            kind,
            init_height: self.vals.len(),
            unreachable: false,
        });
        self.push_vals(self.start_types(blockty)?);
        debug!("pushed frame");

        Ok(())
    }

    #[instrument(level = "debug", skip(self))]
    fn push_func_frame(&mut self, idx: u32) -> Result<()> {
        self.frames.push(Frame {
            ty: FrameType::Function(idx),
            kind: FrameKind::Function,
            init_height: self.vals.len(),
            unreachable: false,
        });
        debug!("pushed function frame");

        Ok(())
    }

    fn start_types(&self, blockty: BlockType) -> Result<&'a [ValType]> {
        Ok(match blockty {
            BlockType::Empty | BlockType::Type(_) => &[],
            BlockType::FuncType(idx) => {
                let ty = self
                    .validator
                    .module_tys
                    .get(idx as usize)
                    .context("type not found")?;
                &ty.0
            }
        })
    }

    fn labels(&self, frame: &Frame) -> Result<Cow<'a, [ValType]>> {
        let ty = match frame.ty {
            FrameType::Block(b) => b,
            FrameType::Function(idx) => {
                return Ok(Cow::Borrowed(
                    &self
                        .validator
                        .module_tys
                        .get(idx as usize)
                        .context("type not found")?
                        .1,
                ))
            }
        };

        Ok(match ty {
            BlockType::Empty => Cow::Borrowed(&[]),
            BlockType::Type(_) if frame.kind == FrameKind::Loop => Cow::Borrowed(&[]),
            BlockType::Type(t) => Cow::Owned(vec![t]),
            BlockType::FuncType(idx) => {
                let ty = self
                    .validator
                    .module_tys
                    .get(idx as usize)
                    .context("type not found")?;
                Cow::Borrowed(match frame.kind {
                    FrameKind::Loop => &ty.0,
                    _ => &ty.1,
                })
            }
        })
    }

    fn end_types(&self, blockty: FrameType) -> Result<Cow<'a, [ValType]>> {
        Ok(match blockty {
            FrameType::Block(blockty) => match blockty {
                BlockType::Empty => Cow::Borrowed(&[][..]),
                BlockType::Type(ty) => Cow::Owned(vec![ty]),
                BlockType::FuncType(idx) => {
                    let ty = self
                        .validator
                        .module_tys
                        .get(idx as usize)
                        .context("type not found")?;
                    Cow::Borrowed(&ty.1)
                }
            },
            FrameType::Function(idx) => Cow::Borrowed(
                &self
                    .validator
                    .module_tys
                    .get(idx as usize)
                    .context("type not found")?
                    .1,
            ),
        })
    }

    fn pop_frame(&mut self) -> Result<Frame> {
        debug_assert!(!self.frames.is_empty());

        let last_frame = self.frames.last().expect("should have a frame");
        let result = self.end_types(last_frame.ty)?;
        self.pop_vals(&result).with_context(|| {
            format!("error getting result values from frame, expected: {result:?}",)
        })?;
        let last_frame = self.frames.pop().expect("should have a frame");
        ensure!(
            self.vals.len() == last_frame.init_height,
            "stack height mismatch when popping frame, expected {}, got {}",
            last_frame.init_height,
            self.vals.len()
        );
        debug!("popped frame: {last_frame:?}");

        Ok(last_frame)
    }

    fn unreachable(&mut self) {
        debug_assert!(!self.frames.is_empty());

        let last_frame = self.frames.last_mut().expect("should have a frame");
        last_frame.unreachable = true;
        self.vals.truncate(last_frame.init_height);
        debug!("set current frame unreachable: {last_frame:?}");
    }

    #[inline]
    fn get_frame_n(&self, n: usize) -> Option<&Frame> {
        self.frames.get((self.frames.len() - 1).checked_sub(n)?)
    }

    // Inlined because validate_instr is a hot function that has very few call sites
    #[inline]
    fn validate_instr(&mut self, instr: Instruction) -> Result<()> {
        // Make sure that this is not called in optimized builds. This improves performance by
        // around 6%.
        #[cfg(debug_assertions)]
        debug!("got instruction: {instr:?}");

        match instr {
            Instruction::Unreachable => self.unreachable(),
            Instruction::Nop => {}
            Instruction::End => {
                // If this is true, then the `if` had no else. In that case, validate that the
                // empty else block produces the correct type.
                if self.frames.last().expect("frames should not be empty").kind == FrameKind::If {
                    self.validate_instr(Instruction::Else)?;
                }
                let frame = self.pop_frame()?;
                self.push_vals(&self.end_types(frame.ty)?);
            }
            Instruction::Return => {
                let func_ctx = self.current_func.result;
                self.pop_vals(func_ctx)?;
                self.unreachable();
            }
            Instruction::Drop => drop(self.pop_val(None)?),
            Instruction::Select => {
                self.pop_val(Some(ValType::I32))?;
                let val1 = self.pop_val(None)?;
                let val2 = self.pop_val(None)?;
                ensure!(
                    val1.matches(val2),
                    "select operands must be the same type, got {val1} and {val2}"
                );
                ensure!(
                    !val1.is_ref() && !val2.is_ref(),
                    "cannot have any reference types in select"
                );
                let push = if val1.is_any() { val2 } else { val1 };
                self.push_val(push);
            }

            // itestop
            Instruction::I32Eqz => {
                self.pop_val(Some(ValType::I32))?;
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::I64Eqz => {
                self.pop_val(Some(ValType::I64))?;
                self.push_val(Operand::Exact(ValType::I32));
            }

            // irelop
            Instruction::I32Eq
            | Instruction::I32Ne
            | Instruction::I32LtS
            | Instruction::I32LtU
            | Instruction::I32GtS
            | Instruction::I32GtU
            | Instruction::I32LeS
            | Instruction::I32LeU
            | Instruction::I32GeS
            | Instruction::I32GeU => {
                self.pop_val(Some(ValType::I32))?;
                self.pop_val(Some(ValType::I32))?;
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::I64Eq
            | Instruction::I64Ne
            | Instruction::I64LtS
            | Instruction::I64LtU
            | Instruction::I64GtS
            | Instruction::I64GtU
            | Instruction::I64LeS
            | Instruction::I64LeU
            | Instruction::I64GeS
            | Instruction::I64GeU => {
                self.pop_val(Some(ValType::I64))?;
                self.pop_val(Some(ValType::I64))?;
                self.push_val(Operand::Exact(ValType::I32));
            }

            // frelop
            Instruction::F32Eq
            | Instruction::F32Ne
            | Instruction::F32Lt
            | Instruction::F32Gt
            | Instruction::F32Le
            | Instruction::F32Ge => {
                self.pop_val(Some(ValType::F32))?;
                self.pop_val(Some(ValType::F32))?;
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::F64Eq
            | Instruction::F64Ne
            | Instruction::F64Lt
            | Instruction::F64Gt
            | Instruction::F64Le
            | Instruction::F64Ge => {
                self.pop_val(Some(ValType::F64))?;
                self.pop_val(Some(ValType::F64))?;
                self.push_val(Operand::Exact(ValType::I32));
            }

            // iunop
            Instruction::I32Clz | Instruction::I32Ctz | Instruction::I32Popcnt => {
                self.pop_val(Some(ValType::I32))?;
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::I64Clz | Instruction::I64Ctz | Instruction::I64Popcnt => {
                self.pop_val(Some(ValType::I64))?;
                self.push_val(Operand::Exact(ValType::I64));
            }

            // ibinop
            Instruction::I32Add
            | Instruction::I32Sub
            | Instruction::I32Mul
            | Instruction::I32DivS
            | Instruction::I32DivU
            | Instruction::I32RemS
            | Instruction::I32RemU
            | Instruction::I32And
            | Instruction::I32Or
            | Instruction::I32Xor
            | Instruction::I32Shl
            | Instruction::I32ShrS
            | Instruction::I32ShrU
            | Instruction::I32Rotl
            | Instruction::I32Rotr => {
                self.pop_val(Some(ValType::I32))?;
                self.pop_val(Some(ValType::I32))?;
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::I64Add
            | Instruction::I64Sub
            | Instruction::I64Mul
            | Instruction::I64DivS
            | Instruction::I64DivU
            | Instruction::I64RemS
            | Instruction::I64RemU
            | Instruction::I64And
            | Instruction::I64Or
            | Instruction::I64Xor
            | Instruction::I64Shl
            | Instruction::I64ShrS
            | Instruction::I64ShrU
            | Instruction::I64Rotl
            | Instruction::I64Rotr => {
                self.pop_val(Some(ValType::I64))?;
                self.pop_val(Some(ValType::I64))?;
                self.push_val(Operand::Exact(ValType::I64));
            }

            // funop
            Instruction::F32Abs
            | Instruction::F32Neg
            | Instruction::F32Ceil
            | Instruction::F32Floor
            | Instruction::F32Trunc
            | Instruction::F32Nearest
            | Instruction::F32Sqrt => {
                self.pop_val(Some(ValType::F32))?;
                self.push_val(Operand::Exact(ValType::F32));
            }
            Instruction::F64Abs
            | Instruction::F64Neg
            | Instruction::F64Ceil
            | Instruction::F64Floor
            | Instruction::F64Trunc
            | Instruction::F64Nearest
            | Instruction::F64Sqrt => {
                self.pop_val(Some(ValType::F64))?;
                self.push_val(Operand::Exact(ValType::F64));
            }

            // fbinop
            Instruction::F32Add
            | Instruction::F32Sub
            | Instruction::F32Mul
            | Instruction::F32Div
            | Instruction::F32Min
            | Instruction::F32Max
            | Instruction::F32Copysign => {
                self.pop_val(Some(ValType::F32))?;
                self.pop_val(Some(ValType::F32))?;
                self.push_val(Operand::Exact(ValType::F32));
            }
            Instruction::F64Add
            | Instruction::F64Sub
            | Instruction::F64Mul
            | Instruction::F64Div
            | Instruction::F64Min
            | Instruction::F64Max
            | Instruction::F64Copysign => {
                self.pop_val(Some(ValType::F64))?;
                self.pop_val(Some(ValType::F64))?;
                self.push_val(Operand::Exact(ValType::F64));
            }

            Instruction::I32WrapI64 => {
                self.pop_val(Some(ValType::I64))?;
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::I32TruncF32S | Instruction::I32TruncF32U => {
                self.pop_val(Some(ValType::F32))?;
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::I32TruncF64S | Instruction::I32TruncF64U => {
                self.pop_val(Some(ValType::F64))?;
                self.push_val(Operand::Exact(ValType::I32));
            }

            Instruction::I64ExtendI32S | Instruction::I64ExtendI32U => {
                self.pop_val(Some(ValType::I32))?;
                self.push_val(Operand::Exact(ValType::I64));
            }
            Instruction::I64TruncF32S | Instruction::I64TruncF32U => {
                self.pop_val(Some(ValType::F32))?;
                self.push_val(Operand::Exact(ValType::I64));
            }
            Instruction::I64TruncF64S | Instruction::I64TruncF64U => {
                self.pop_val(Some(ValType::F64))?;
                self.push_val(Operand::Exact(ValType::I64));
            }
            Instruction::F32ConvertI32S | Instruction::F32ConvertI32U => {
                self.pop_val(Some(ValType::I32))?;
                self.push_val(Operand::Exact(ValType::F32));
            }
            Instruction::F32ConvertI64S | Instruction::F32ConvertI64U => {
                self.pop_val(Some(ValType::I64))?;
                self.push_val(Operand::Exact(ValType::F32));
            }
            Instruction::F32DemoteF64 => {
                self.pop_val(Some(ValType::F64))?;
                self.push_val(Operand::Exact(ValType::F32));
            }
            Instruction::F64ConvertI32S | Instruction::F64ConvertI32U => {
                self.pop_val(Some(ValType::I32))?;
                self.push_val(Operand::Exact(ValType::F64));
            }
            Instruction::F64ConvertI64S | Instruction::F64ConvertI64U => {
                self.pop_val(Some(ValType::I64))?;
                self.push_val(Operand::Exact(ValType::F64));
            }
            Instruction::F64PromoteF32 => {
                self.pop_val(Some(ValType::F32))?;
                self.push_val(Operand::Exact(ValType::F64));
            }
            Instruction::I32ReinterpretF32 => {
                self.pop_val(Some(ValType::F32))?;
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::I64ReinterpretF64 => {
                self.pop_val(Some(ValType::F64))?;
                self.push_val(Operand::Exact(ValType::I64));
            }
            Instruction::F32ReinterpretI32 => {
                self.pop_val(Some(ValType::I32))?;
                self.push_val(Operand::Exact(ValType::F32));
            }
            Instruction::F64ReinterpretI64 => {
                self.pop_val(Some(ValType::I64))?;
                self.push_val(Operand::Exact(ValType::F64));
            }
            Instruction::I32TruncSatF32S | Instruction::I32TruncSatF32U => {
                self.pop_val(Some(ValType::F32))?;
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::I32TruncSatF64S | Instruction::I32TruncSatF64U => {
                self.pop_val(Some(ValType::F64))?;
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::I64TruncSatF32S | Instruction::I64TruncSatF32U => {
                self.pop_val(Some(ValType::F32))?;
                self.push_val(Operand::Exact(ValType::I64));
            }
            Instruction::I64TruncSatF64S | Instruction::I64TruncSatF64U => {
                self.pop_val(Some(ValType::F64))?;
                self.push_val(Operand::Exact(ValType::I64));
            }
            Instruction::I32Extend8S | Instruction::I32Extend16S => {
                self.pop_val(Some(ValType::I32))?;
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::I64Extend8S | Instruction::I64Extend16S | Instruction::I64Extend32S => {
                self.pop_val(Some(ValType::I64))?;
                self.push_val(Operand::Exact(ValType::I64));
            }

            // Memory operations
            Instruction::MemorySize => {
                ensure!(self.validator.mems.len() == 1, "memory not found");
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::MemoryGrow => {
                ensure!(self.validator.mems.len() == 1, "memory not found");
                self.pop_val(Some(ValType::I32))?;
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::MemoryCopy | Instruction::MemoryFill => {
                ensure!(self.validator.mems.len() == 1, "memory not found");
                self.pop_val(Some(ValType::I32))?;
                self.pop_val(Some(ValType::I32))?;
                self.pop_val(Some(ValType::I32))?;
            }
            Instruction::MemoryInit { data_idx } => {
                ensure!(self.validator.mems.len() == 1, "memory not found");
                ensure!(
                    (data_idx as usize) < self.validator.datas_found,
                    "data not found: {data_idx}"
                );
                self.pop_val(Some(ValType::I32))?;
                self.pop_val(Some(ValType::I32))?;
                self.pop_val(Some(ValType::I32))?;
            }
            Instruction::DataDrop { data_idx } => {
                ensure!(
                    (data_idx as usize) < self.validator.datas_found,
                    "data not found: {data_idx}"
                );
            }

            Instruction::RefIsNull => {
                let val = self.pop_val(None)?;
                ensure!(
                    val.is_ref() || val.is_any(),
                    "ref.is_null must be called on a reference type, got {val}"
                );
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::SelectT(t) => {
                self.pop_val(Some(ValType::I32))?;
                self.pop_val(Some(t))?;
                self.pop_val(Some(t))?;
                self.push_val(Operand::Exact(t));
            }
            Instruction::I32Load(memarg) => {
                self.validate_mem_load(memarg, ValType::I32, None)?;
            }
            Instruction::I64Load(memarg) => {
                self.validate_mem_load(memarg, ValType::I64, None)?;
            }
            Instruction::F32Load(memarg) => {
                self.validate_mem_load(memarg, ValType::F32, None)?;
            }
            Instruction::F64Load(memarg) => {
                self.validate_mem_load(memarg, ValType::F64, None)?;
            }
            Instruction::I32Load8S(memarg) => {
                self.validate_mem_load(memarg, ValType::I32, Some(8))?;
            }
            Instruction::I32Load8U(memarg) => {
                self.validate_mem_load(memarg, ValType::I32, Some(8))?;
            }
            Instruction::I32Load16S(memarg) => {
                self.validate_mem_load(memarg, ValType::I32, Some(16))?;
            }
            Instruction::I32Load16U(memarg) => {
                self.validate_mem_load(memarg, ValType::I32, Some(16))?;
            }
            Instruction::I64Load8S(memarg) => {
                self.validate_mem_load(memarg, ValType::I64, Some(8))?;
            }
            Instruction::I64Load8U(memarg) => {
                self.validate_mem_load(memarg, ValType::I64, Some(8))?;
            }
            Instruction::I64Load16S(memarg) => {
                self.validate_mem_load(memarg, ValType::I64, Some(16))?;
            }
            Instruction::I64Load16U(memarg) => {
                self.validate_mem_load(memarg, ValType::I64, Some(16))?;
            }
            Instruction::I64Load32S(memarg) => {
                self.validate_mem_load(memarg, ValType::I64, Some(32))?;
            }
            Instruction::I64Load32U(memarg) => {
                self.validate_mem_load(memarg, ValType::I64, Some(32))?;
            }
            Instruction::I32Store(memarg) => {
                self.validate_mem_store(memarg, ValType::I32, None)?;
            }
            Instruction::I64Store(memarg) => {
                self.validate_mem_store(memarg, ValType::I64, None)?;
            }
            Instruction::F32Store(memarg) => {
                self.validate_mem_store(memarg, ValType::F32, None)?;
            }
            Instruction::F64Store(memarg) => {
                self.validate_mem_store(memarg, ValType::F64, None)?;
            }
            Instruction::I32Store8(memarg) => {
                self.validate_mem_store(memarg, ValType::I32, Some(8))?;
            }
            Instruction::I32Store16(memarg) => {
                self.validate_mem_store(memarg, ValType::I32, Some(16))?;
            }
            Instruction::I64Store8(memarg) => {
                self.validate_mem_store(memarg, ValType::I64, Some(8))?;
            }
            Instruction::I64Store16(memarg) => {
                self.validate_mem_store(memarg, ValType::I64, Some(16))?;
            }
            Instruction::I64Store32(memarg) => {
                self.validate_mem_store(memarg, ValType::I64, Some(32))?;
            }

            // Control instructions
            Instruction::Loop(Block { ty: blockty, .. }) => {
                self.pop_vals(self.start_types(blockty)?)?;
                self.push_frame(FrameKind::Loop, blockty)?;
            }
            Instruction::Block(Block { ty: blockty, .. }) => {
                self.pop_vals(self.start_types(blockty)?)?;
                self.push_frame(FrameKind::Block, blockty)?;
            }
            Instruction::If {
                block: Block { ty: blockty, .. },
                ..
            } => {
                self.pop_val(Some(ValType::I32))?;
                self.pop_vals(self.start_types(blockty)?)?;
                self.push_frame(FrameKind::If, blockty)?;
            }
            Instruction::Else => {
                let frame = self.pop_frame()?;
                ensure!(frame.kind == FrameKind::If, "unexpected else opcode");
                let blockty = match frame.ty {
                    FrameType::Block(b) => b,
                    FrameType::Function(_) => {
                        panic!("BUG: should not have `FrameType::Function` after `FrameKind::If")
                    }
                };
                self.push_frame(FrameKind::Else, blockty)?;
            }
            Instruction::Br { depth } => {
                let frame = self
                    .get_frame_n(depth as usize)
                    .context("br to invalid depth")?;
                self.pop_vals(&self.labels(frame)?)?;
                self.unreachable();
            }
            Instruction::BrTable(br_tbl) => {
                self.pop_val(Some(ValType::I32))?;
                let default_labels = self.labels(
                    self.get_frame_n(br_tbl.default_depth as usize)
                        .context("br_table to invalid depth")?,
                )?;
                let arity = default_labels.len();
                for depth in br_tbl.depths.iter() {
                    let vals = self.labels(
                        self.get_frame_n(*depth as usize)
                            .context("br_table to invalid depth")?,
                    )?;
                    ensure!(
                        vals.len() == arity,
                        "br_table arity mismatch, got {}, want {arity}",
                        vals.len()
                    );
                    for pushed in self.pop_vals_collect(&vals)?.into_iter().rev() {
                        self.push_val(pushed);
                    }
                }
                self.pop_vals(&default_labels)?;
                self.unreachable();
            }
            Instruction::BrIf { depth } => {
                self.pop_val(Some(ValType::I32))?;
                let frame = self
                    .get_frame_n(depth as usize)
                    .context("br_if to invalid depth")?;
                self.pop_vals(&self.labels(frame)?)?;
                // Need to get new reference for borrowck. Should be the same frame as before as
                // long as `expect_vals` doesn't push or pop a frame (which it doesn't)
                let frame = self.get_frame_n(depth as usize).unwrap();
                self.push_vals(&self.labels(frame)?);
            }

            // Const instrs
            Instruction::I32Const(_) => self.push_val(Operand::Exact(ValType::I32)),
            Instruction::F32Const(_) => self.push_val(Operand::Exact(ValType::F32)),
            Instruction::I64Const(_) => self.push_val(Operand::Exact(ValType::I64)),
            Instruction::F64Const(_) => self.push_val(Operand::Exact(ValType::F64)),

            Instruction::Call { func_idx } => {
                let ty = self.validator.get_ty(func_idx)?;
                self.pop_vals(&ty.0)?;
                self.push_vals(&ty.1);
            }

            Instruction::LocalGet { idx } => {
                let ty = *self
                    .current_func
                    .locals
                    .get(idx as usize)
                    .context("local.get out of bounds")?;
                self.push_val(Operand::Exact(ty));
            }
            Instruction::LocalSet { idx } => {
                let ty = *self
                    .current_func
                    .locals
                    .get(idx as usize)
                    .context("local.set out of bounds")?;
                self.pop_val(Some(ty))?;
            }
            Instruction::LocalTee { idx } => {
                let ty = *self
                    .current_func
                    .locals
                    .get(idx as usize)
                    .context("local.tee out of bounds")?;
                self.pop_val(Some(ty))?;
                self.push_val(Operand::Exact(ty));
            }
            Instruction::GlobalGet { idx } => {
                let ty = self
                    .validator
                    .globals
                    .get(idx as usize)
                    .context("global.get out of bounds")?
                    .ty
                    .content_type;
                self.push_val(Operand::Exact(ty));
            }
            Instruction::GlobalSet { idx } => {
                let ty = self
                    .validator
                    .globals
                    .get(idx as usize)
                    .context("global.set out of bounds")?
                    .ty;
                ensure!(ty.mutable, "cannot global set on an immutable global");
                self.pop_val(Some(ty.content_type))?;
            }
            Instruction::ElemDrop { elem_idx } => {
                ensure!(
                    (elem_idx as usize) < self.validator.elems.len(),
                    "elem not found: {elem_idx}"
                );
            }
            Instruction::RefFunc { func_idx } => {
                ensure!(
                    (func_idx as usize) < self.validator.funcs.len(),
                    "function type not found: {func_idx}"
                );
                ensure!(
                    self.validator.declared_funcs.contains(&func_idx),
                    "ref_func: no declared initializers found for function at index {func_idx}"
                );
                self.push_val(Operand::Exact(ValType::Func));
            }
            Instruction::TableGet { table } => {
                let ty = self
                    .validator
                    .tables
                    .get(table as usize)
                    .context("table.get targets invalid table")?
                    .elem_type;
                self.pop_val(Some(ValType::I32))?;
                self.push_val(Operand::Exact(ty.into()));
            }
            Instruction::TableSet { table } => {
                let ty = self
                    .validator
                    .tables
                    .get(table as usize)
                    .context("table.set targets invalid table")?
                    .elem_type;
                self.pop_val(Some(ty.into()))?;
                self.pop_val(Some(ValType::I32))?;
            }
            Instruction::TableGrow { table } => {
                let ty = self
                    .validator
                    .tables
                    .get(table as usize)
                    .context("table.grow targets invalid table")?
                    .elem_type;
                self.pop_val(Some(ValType::I32))?;
                self.pop_val(Some(ty.into()))?;
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::TableSize { table } => {
                ensure!(
                    self.validator.tables.len() > table as usize,
                    "table.size table not found: {table}"
                );
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::TableFill { table } => {
                let ty = self
                    .validator
                    .tables
                    .get(table as usize)
                    .context("table.fill targets invalid table")?
                    .elem_type;
                self.pop_val(Some(ValType::I32))?;
                self.pop_val(Some(ty.into()))?;
                self.pop_val(Some(ValType::I32))?;
            }
            Instruction::RefNull { ty } => {
                self.push_val(Operand::Exact(ty.into()));
            }
            Instruction::CallIndirect {
                type_idx,
                table_idx,
            } => {
                let table = &self
                    .validator
                    .tables
                    .get(table_idx as usize)
                    .context("call.indirect table not found")?;
                ensure!(
                    table.elem_type == RefType::Func,
                    "call.indirect table elem type must be of type funcref, got: {}",
                    table.elem_type
                );
                let ty = self
                    .validator
                    .module_tys
                    .get(type_idx as usize)
                    .context("call_indirect type not found")?;
                self.pop_val(Some(ValType::I32))?;
                self.pop_vals(&ty.0)?;
                self.push_vals(&ty.1);
            }
            Instruction::TableCopy {
                src_table,
                dst_table,
            } => {
                ensure!(
                    self.validator.tables.len() > src_table as usize,
                    "table.copy src table not found: {src_table}"
                );
                ensure!(
                    self.validator.tables.len() > dst_table as usize,
                    "table.copy dst table not found: {dst_table}"
                );
                let src_ty = self.validator.tables[src_table as usize].elem_type;
                let dst_ty = self.validator.tables[dst_table as usize].elem_type;
                ensure!(
                    src_ty == dst_ty,
                    "table.copy src and dst types must match, got {src_ty} and {dst_ty}"
                );
                self.pop_val(Some(ValType::I32))?;
                self.pop_val(Some(ValType::I32))?;
                self.pop_val(Some(ValType::I32))?;
            }
            Instruction::TableInit {
                elem_idx,
                table_idx,
            } => {
                ensure!(
                    (table_idx as usize) < self.validator.tables.len(),
                    "table not found: {table_idx}"
                );
                ensure!(
                    (elem_idx as usize) < self.validator.elems.len(),
                    "elem not found: {elem_idx}"
                );
                let table = &self.validator.tables[table_idx as usize];
                let elem = &self.validator.elems[elem_idx as usize];
                ensure!(
                    table.elem_type == elem.types,
                    "table.init table and elem types must match, got {} and {}",
                    table.elem_type,
                    elem.types
                );
                self.pop_val(Some(ValType::I32))?;
                self.pop_val(Some(ValType::I32))?;
                self.pop_val(Some(ValType::I32))?;
            }
        }

        #[cfg(debug_assertions)]
        {
            trace!("values: {:?}", self.vals);
            trace!("frames: {:?}", self.frames);
            debug!("validated instruction");
        }

        Ok(())
    }

    #[instrument(level = "debug", skip(self))]
    fn validate_mem_load(
        &mut self,
        memarg: MemArg,
        valtype: ValType,
        size_override: Option<u32>,
    ) -> Result<()> {
        ensure!(self.validator.mems.len() == 1, "memory not found");
        let size = size_override.unwrap_or_else(|| match valtype {
            ValType::I32 | ValType::F32 => 32,
            ValType::I64 | ValType::F64 => 64,
            _ => panic!("should not be called with reftypes"),
        });
        ensure!(
            1 << memarg.align <= size / 8,
            "memory load operation alignment must not be larger than natural alignment, got {memarg:?}, size: {size}"
        );
        self.pop_val(Some(ValType::I32))?;
        self.push_val(Operand::Exact(valtype));

        debug!("validated memory load instruction");
        Ok(())
    }

    #[instrument(level = "debug", skip(self))]
    fn validate_mem_store(
        &mut self,
        memarg: MemArg,
        valtype: ValType,
        size_override: Option<u32>,
    ) -> Result<()> {
        ensure!(self.validator.mems.len() == 1, "memory not found");
        let size = size_override.unwrap_or_else(|| match valtype {
            ValType::I32 | ValType::F32 => 32,
            ValType::I64 | ValType::F64 => 64,
            _ => panic!("should not be called with reftypes"),
        });
        ensure!(
            1 << memarg.align <= size / 8,
            "memory store operation alignment must not be larger than natural alignment, got {memarg:?}, size: {size}"
        );
        self.pop_val(Some(valtype))?;
        self.pop_val(Some(ValType::I32))?;

        debug!("validated memory store instruction");
        Ok(())
    }
}
