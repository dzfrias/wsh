use std::{borrow::Cow, fmt, iter};

use anyhow::{bail, ensure, Context, Result};

use crate::{
    BlockType, Code, Data, Element, ElementKind, Export, ExternalKind, FuncType, Function, Global,
    GlobalType, Import, ImportKind, InitExpr, Instruction, Limit, MemArg, Memory, Module, RefType,
    TableType, ValType,
};

/// Validate a WebAssembly module.
pub fn validate(m: &Module) -> Result<()> {
    Validator::new().validate_module(m)
}

/// A WebAssembly control frame.
#[derive(Debug)]
struct Frame<'a> {
    label_ty: &'a [ValType],
    // Cow here because of lifetime issues when using the single-val blocktype shorthand
    // See https://webassembly.github.io/spec/core/syntax/instructions.html#syntax-blocktype
    result: Cow<'a, [ValType]>,
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
        self.is_any() || matches!(self, Self::Exact(ValType::Func | ValType::Extern))
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

/// A validator for a WebAssembly module.
///
/// Contains the current state of validation for the module.
#[derive(Debug)]
struct Validator<'a> {
    // These are cached between function validations
    frames: Vec<Frame<'a>>,
    vals: Vec<Operand>,
    current_func: FuncCtx<'a>,

    tys: &'a [FuncType],
    elems: &'a [Element],
    funcs: &'a [Function],
    func_tys: Vec<&'a FuncType>,
    tables: Vec<&'a TableType>,
    mems: Vec<&'a Memory>,
    globals: Vec<&'a GlobalType>,
    datas_found: usize,
}

impl<'a> Validator<'a> {
    pub fn new() -> Self {
        Self {
            tys: &[],
            funcs: &[],
            frames: vec![],
            current_func: FuncCtx {
                locals: vec![],
                result: &[],
            },

            vals: vec![],
            func_tys: vec![],
            tables: vec![],
            mems: vec![],
            globals: vec![],
            datas_found: 0,
            elems: &[],
        }
    }

    fn validate_module(mut self, m: &'a Module) -> Result<()> {
        self.tys = &m.types;
        self.elems = &m.elements;
        self.funcs = &m.functions;

        self.validate_imports(&m.imports)?;

        for func in &m.functions {
            self.func_tys.push(self.get_ty(func.index)?);
        }
        self.tables.extend(&m.tables);
        self.mems.extend(&m.memories);
        self.datas_found = m.datas.len();

        // Validate globals before adding them to validator to prevent self-referencing
        self.valdate_globals(&m.globals)?;
        self.globals.extend(m.globals.iter().map(|g| &g.kind));

        self.validate_codes(&m.codes, &m.functions)?;
        self.validate_tables(&m.tables)?;
        self.validate_memories(&m.memories)?;
        self.validate_elems(&m.elements)?;
        self.validate_datas(&m.datas)?;

        if let Some(start) = m.start {
            let ty = self
                .func_tys
                .get(start as usize)
                .context("start function not found")?;
            ensure!(ty.0.is_empty(), "start function params not zero");
            ensure!(ty.1.is_empty(), "start function results not zero");
        }

        self.validate_exports(&m.exports)?;

        // Multi-memories not supported
        ensure!(
            m.memories.len() <= 1,
            "too many memores, only one is supported"
        );

        Ok(())
    }

    fn validate_imports(&mut self, imports: &'a [Import]) -> Result<()> {
        for import in imports {
            match &import.kind {
                ImportKind::Function(idx) => self.func_tys.push(self.get_ty(*idx)?),
                ImportKind::Table(table) => {
                    self.validate_limit(&table.limit)?;
                    self.tables.push(table);
                }
                ImportKind::Memory(mem) => {
                    self.validate_mem(mem)?;
                    self.mems.push(mem);
                }
                ImportKind::Global(global) => self.globals.push(global),
            }
        }

        Ok(())
    }

    #[allow(clippy::unused_self)]
    fn validate_limit(&self, limit: &Limit) -> Result<()> {
        let Some(max) = limit.max else {
            return Ok(());
        };
        ensure!(limit.initial <= max, "invalid limit: {limit}");
        Ok(())
    }

    fn validate_tables(&self, tables: &[TableType]) -> Result<()> {
        for tbl in tables {
            self.validate_limit(&tbl.limit)?;
        }

        Ok(())
    }

    fn validate_memories(&self, mems: &[Memory]) -> Result<()> {
        for mem in mems {
            self.validate_mem(mem)?;
        }

        Ok(())
    }

    fn validate_mem(&self, mem: &Memory) -> Result<()> {
        // Memory limit must be less than 4GB
        const MAX: u64 = (1u64 << 32) / 65536;

        if let Some(max) = mem.limit.max {
            let max = max as u64;
            ensure!(max <= MAX, "memory too large: {max}");
        }
        let initial = mem.limit.initial as u64;
        ensure!(initial <= MAX, "memory too large: {initial}");

        self.validate_limit(&mem.limit)?;

        Ok(())
    }

    fn validate_datas(&self, datas: &[Data]) -> Result<()> {
        for init in datas.iter().filter_map(|data| data.offset) {
            // Data offset into memory must be an integer
            self.validate_init_expr(init, Operand::Exact(ValType::I32))?;
        }

        Ok(())
    }

    fn get_ty(&self, idx: u32) -> Result<&'a FuncType> {
        // Type must exist
        self.tys
            .get(idx as usize)
            .context("function type not found")
    }

    fn get_functype(&self, idx: u32) -> Result<&'a FuncType> {
        // Function must exist
        let f = self.funcs.get(idx as usize).context("function not found")?;
        self.get_ty(f.index)
    }

    fn validate_init_expr(&self, init: InitExpr, operand: Operand) -> Result<()> {
        let Operand::Exact(expected) = operand else {
            return Ok(());
        };

        let ty = match init {
            InitExpr::I32Const(_) => ValType::I32,
            InitExpr::I64Const(_) => ValType::I64,
            InitExpr::F32Const(_) => ValType::F32,
            InitExpr::F64Const(_) => ValType::F64,
            InitExpr::ConstGlobalGet(idx) => {
                // Global must exist
                self.globals
                    .get(idx as usize)
                    .context("global not found")?
                    .content_type
            }
            InitExpr::RefNull(t) => match t {
                RefType::Func => ValType::Func,
                RefType::Extern => ValType::Extern,
            },
            InitExpr::RefFunc(_) => ValType::Func,
        };

        ensure!(
            expected == ty,
            "unexepected init expr type: {expected} != {ty}"
        );

        Ok(())
    }

    fn valdate_globals(&self, globals: &[Global]) -> Result<()> {
        for global in globals {
            // Global init expr must match global type
            self.validate_init_expr(global.init, Operand::Exact(global.kind.content_type))?;
        }

        Ok(())
    }

    fn validate_elems(&self, elements: &[Element]) -> Result<()> {
        for elem in elements {
            if let ElementKind::Active { tbl_idx, offset } = elem.kind {
                let Some(tbl) = self.tables.get(tbl_idx as usize) else {
                    bail!("table not found: {tbl_idx}");
                };
                // Elem type must match table type
                ensure!(
                    tbl.elem_type == elem.types,
                    "elem type mismatch: {} != {}",
                    tbl.elem_type,
                    elem.types
                );
                // Offset must be an integer
                self.validate_init_expr(offset, Operand::Exact(ValType::I32))?;
            }

            for init in &elem.elems {
                // Init expr must match elem type
                self.validate_init_expr(*init, Operand::Exact(elem.types.into()))?;
            }
        }

        Ok(())
    }

    fn validate_exports(&self, exports: &[Export]) -> Result<()> {
        let mut seen_exports = Vec::with_capacity(exports.len());
        for export in exports {
            // Exports must be unique
            ensure!(
                !seen_exports.contains(&export.field),
                "duplicate export of {}",
                export.field
            );
            let idx = export.external_idx as usize;
            match export.kind {
                ExternalKind::Function => {
                    ensure!(
                        self.func_tys.get(idx).is_some(),
                        "export function type not found"
                    );
                }
                ExternalKind::Table => {
                    ensure!(self.tables.get(idx).is_some(), "export table not found");
                }
                ExternalKind::Memory => {
                    ensure!(self.mems.get(idx).is_some(), "export memory not found");
                }
                ExternalKind::Global => {
                    ensure!(self.globals.get(idx).is_some(), "export global not found");
                }
            }
            seen_exports.push(export.field);
        }

        Ok(())
    }

    fn validate_codes(&mut self, codes: &[Code], funcs: &[Function]) -> Result<()> {
        for (code, func) in codes.iter().zip(funcs) {
            let ty = self.get_ty(func.index)?;
            self.current_func.locals = ty.0.clone();
            self.current_func
                .locals
                .extend(code.locals.iter().flat_map(|num_locals| {
                    iter::repeat(num_locals.locals_type).take(num_locals.num as usize)
                }));
            self.current_func.result = &ty.1;
            self.push_frame(FrameKind::Function, &ty.1, Cow::Borrowed(&ty.1));

            for instr in code.body.instrs() {
                let instr = code.body.instruction(instr);
                self.validate_instr(instr)?;
            }

            self.reset_code_state();
        }
        Ok(())
    }

    fn reset_code_state(&mut self) {
        self.frames.clear();
        self.vals.clear();
        self.current_func.locals.clear();
        self.current_func.result = &[];
    }

    fn push_vals(&mut self, label_ty: &[ValType]) {
        self.vals.reserve(label_ty.len());
        for val in label_ty {
            self.push_val(Operand::Exact(*val));
        }
    }

    fn push_val(&mut self, val: Operand) {
        self.vals.push(val);
    }

    fn pop_val(&mut self) -> Result<Operand> {
        let last_frame = self.frames.last_mut().expect("should have a frame");
        if self.vals.len() == last_frame.init_height && last_frame.unreachable {
            return Ok(Operand::Any);
        }
        // Value stack must not be empty
        ensure!(
            self.vals.len() > last_frame.init_height,
            "stack height mismatch when popping value"
        );

        Ok(self
            .vals
            .pop()
            .expect("should have an operand on the stack"))
    }

    fn expect_val(&mut self, expected: Operand) -> Result<Operand> {
        let got = self.pop_val()?;
        let Operand::Exact(got) = got else {
            return Ok(Operand::Any);
        };
        let Operand::Exact(expected) = expected else {
            return Ok(Operand::Exact(got));
        };
        ensure!(got == expected, "unexpected type: {expected} != {got}");

        Ok(Operand::Exact(got))
    }

    fn expect_vals<T, I>(&mut self, expected: I) -> Result<()>
    where
        T: Into<Operand>,
        I: DoubleEndedIterator<Item = T>,
    {
        for expected in expected.into_iter().rev() {
            self.expect_val(expected.into())?;
        }

        Ok(())
    }

    fn push_frame(&mut self, kind: FrameKind, label_ty: &'a [ValType], result: Cow<'a, [ValType]>) {
        self.frames.push(Frame {
            label_ty,
            result,
            kind,
            init_height: self.vals.len(),
            unreachable: false,
        });
        self.push_vals(label_ty);
    }

    fn pop_frame(&mut self) -> Result<Frame<'a>> {
        debug_assert!(!self.frames.is_empty());

        let last_frame = self.frames.last().expect("should have a frame");
        self.expect_vals(last_frame.result.clone().iter())?;
        let last_frame = self.frames.pop().expect("should have a frame");
        ensure!(
            self.vals.len() == last_frame.init_height,
            "stack height mismatch when popping frame, expected {}, got {}",
            last_frame.init_height,
            self.vals.len()
        );

        Ok(last_frame)
    }

    fn unreachable(&mut self) {
        debug_assert!(!self.frames.is_empty());
        let last_frame = self.frames.last_mut().expect("should have a frame");
        last_frame.unreachable = true;
        self.vals.truncate(last_frame.init_height);
    }

    #[inline]
    fn get_frame_n(&self, n: usize) -> Option<&Frame<'a>> {
        self.frames.get(self.frames.len() - n - 1)
    }

    fn validate_instr(&mut self, instr: Instruction) -> Result<()> {
        match instr {
            Instruction::Unreachable => self.unreachable(),
            Instruction::Nop => {}
            Instruction::Else => {
                let frame = self.pop_frame()?;
                ensure!(frame.kind == FrameKind::If, "unexpected else opcode");
                self.push_frame(FrameKind::Else, frame.label_ty, frame.result);
            }
            Instruction::End => {
                let frame = self.pop_frame()?;
                self.push_vals(&frame.result);
            }
            Instruction::Return => {
                let func_ctx = self.current_func.result;
                self.expect_vals(func_ctx.iter())?;
                self.unreachable();
            }
            Instruction::Drop => drop(self.pop_val()?),
            Instruction::Select => {
                self.expect_val(Operand::Exact(ValType::I32))?;
                let val1 = self.pop_val()?;
                let val2 = self.pop_val()?;
                ensure!(
                    val1.matches(val2),
                    "select operands must be the same type, got {val1} and {val2}"
                );
                let push = if val1.is_any() { val2 } else { val1 };
                self.push_val(push);
            }

            // itestop
            Instruction::I32Eqz => {
                self.expect_val(Operand::Exact(ValType::I32))?;
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::I64Eqz => {
                self.expect_val(Operand::Exact(ValType::I64))?;
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
                self.expect_val(Operand::Exact(ValType::I32))?;
                self.expect_val(Operand::Exact(ValType::I32))?;
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
                self.expect_val(Operand::Exact(ValType::I64))?;
                self.expect_val(Operand::Exact(ValType::I64))?;
                self.push_val(Operand::Exact(ValType::I32));
            }

            // frelop
            Instruction::F32Eq
            | Instruction::F32Ne
            | Instruction::F32Lt
            | Instruction::F32Gt
            | Instruction::F32Le
            | Instruction::F32Ge => {
                self.expect_val(Operand::Exact(ValType::F32))?;
                self.expect_val(Operand::Exact(ValType::F32))?;
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::F64Eq
            | Instruction::F64Ne
            | Instruction::F64Lt
            | Instruction::F64Gt
            | Instruction::F64Le
            | Instruction::F64Ge => {
                self.expect_val(Operand::Exact(ValType::F64))?;
                self.expect_val(Operand::Exact(ValType::F64))?;
                self.push_val(Operand::Exact(ValType::I32));
            }

            // iunop
            Instruction::I32Clz | Instruction::I32Ctz | Instruction::I32Popcnt => {
                self.expect_val(Operand::Exact(ValType::I32))?;
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::I64Clz | Instruction::I64Ctz | Instruction::I64Popcnt => {
                self.expect_val(Operand::Exact(ValType::I64))?;
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
                self.expect_val(Operand::Exact(ValType::I32))?;
                self.expect_val(Operand::Exact(ValType::I32))?;
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
                self.expect_val(Operand::Exact(ValType::I64))?;
                self.expect_val(Operand::Exact(ValType::I64))?;
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
                self.expect_val(Operand::Exact(ValType::F32))?;
                self.push_val(Operand::Exact(ValType::F32));
            }
            Instruction::F64Abs
            | Instruction::F64Neg
            | Instruction::F64Ceil
            | Instruction::F64Floor
            | Instruction::F64Trunc
            | Instruction::F64Nearest
            | Instruction::F64Sqrt => {
                self.expect_val(Operand::Exact(ValType::F64))?;
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
                self.expect_val(Operand::Exact(ValType::F32))?;
                self.expect_val(Operand::Exact(ValType::F32))?;
                self.push_val(Operand::Exact(ValType::F32));
            }
            Instruction::F64Add
            | Instruction::F64Sub
            | Instruction::F64Mul
            | Instruction::F64Div
            | Instruction::F64Min
            | Instruction::F64Max
            | Instruction::F64Copysign => {
                self.expect_val(Operand::Exact(ValType::F64))?;
                self.expect_val(Operand::Exact(ValType::F64))?;
                self.push_val(Operand::Exact(ValType::F64));
            }

            Instruction::I32WrapI64 => {
                self.expect_val(Operand::Exact(ValType::I64))?;
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::I32TruncF32S | Instruction::I32TruncF32U => {
                self.expect_val(Operand::Exact(ValType::F32))?;
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::I32TruncF64S | Instruction::I32TruncF64U => {
                self.expect_val(Operand::Exact(ValType::F64))?;
                self.push_val(Operand::Exact(ValType::I32));
            }

            Instruction::I64ExtendI32S | Instruction::I64ExtendI32U => {
                self.expect_val(Operand::Exact(ValType::I32))?;
                self.push_val(Operand::Exact(ValType::I64));
            }
            Instruction::I64TruncF32S | Instruction::I64TruncF32U => {
                self.expect_val(Operand::Exact(ValType::F32))?;
                self.push_val(Operand::Exact(ValType::I64));
            }
            Instruction::I64TruncF64S | Instruction::I64TruncF64U => {
                self.expect_val(Operand::Exact(ValType::F64))?;
                self.push_val(Operand::Exact(ValType::I64));
            }
            Instruction::F32ConvertI32S | Instruction::F32ConvertI32U => {
                self.expect_val(Operand::Exact(ValType::I32))?;
                self.push_val(Operand::Exact(ValType::F32));
            }
            Instruction::F32ConvertI64S | Instruction::F32ConvertI64U => {
                self.expect_val(Operand::Exact(ValType::I64))?;
                self.push_val(Operand::Exact(ValType::F32));
            }
            Instruction::F32DemoteF64 => {
                self.expect_val(Operand::Exact(ValType::F64))?;
                self.push_val(Operand::Exact(ValType::F32));
            }
            Instruction::F64ConvertI32S | Instruction::F64ConvertI32U => {
                self.expect_val(Operand::Exact(ValType::I32))?;
                self.push_val(Operand::Exact(ValType::F64));
            }
            Instruction::F64ConvertI64S | Instruction::F64ConvertI64U => {
                self.expect_val(Operand::Exact(ValType::I64))?;
                self.push_val(Operand::Exact(ValType::F64));
            }
            Instruction::F64PromoteF32 => {
                self.expect_val(Operand::Exact(ValType::F32))?;
                self.push_val(Operand::Exact(ValType::F64));
            }
            Instruction::I32ReinterpretF32 => {
                self.expect_val(Operand::Exact(ValType::F32))?;
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::I64ReinterpretF64 => {
                self.expect_val(Operand::Exact(ValType::F64))?;
                self.push_val(Operand::Exact(ValType::I64));
            }
            Instruction::F32ReinterpretI32 => {
                self.expect_val(Operand::Exact(ValType::I32))?;
                self.push_val(Operand::Exact(ValType::F32));
            }
            Instruction::F64ReinterpretI64 => {
                self.expect_val(Operand::Exact(ValType::I64))?;
                self.push_val(Operand::Exact(ValType::F64));
            }
            Instruction::I32TruncSatF32S | Instruction::I32TruncSatF32U => {
                self.expect_val(Operand::Exact(ValType::F32))?;
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::I32TruncSatF64S | Instruction::I32TruncSatF64U => {
                self.expect_val(Operand::Exact(ValType::F64))?;
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::I64TruncSatF32S | Instruction::I64TruncSatF32U => {
                self.expect_val(Operand::Exact(ValType::F32))?;
                self.push_val(Operand::Exact(ValType::I64));
            }
            Instruction::I64TruncSatF64S | Instruction::I64TruncSatF64U => {
                self.expect_val(Operand::Exact(ValType::F64))?;
                self.push_val(Operand::Exact(ValType::I64));
            }
            Instruction::I32Extend8S | Instruction::I32Extend16S => {
                self.expect_val(Operand::Exact(ValType::I32))?;
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::I64Extend8S | Instruction::I64Extend16S | Instruction::I64Extend32S => {
                self.expect_val(Operand::Exact(ValType::I64))?;
                self.push_val(Operand::Exact(ValType::I64));
            }

            // Memory operations
            Instruction::MemorySize => {
                ensure!(self.mems.len() == 1, "memory not found");
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::MemoryGrow => {
                ensure!(self.mems.len() == 1, "memory not found");
                self.expect_val(Operand::Exact(ValType::I32))?;
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::MemoryCopy | Instruction::MemoryFill => {
                ensure!(self.mems.len() == 1, "memory not found");
                self.expect_val(Operand::Exact(ValType::I32))?;
                self.expect_val(Operand::Exact(ValType::I32))?;
                self.expect_val(Operand::Exact(ValType::I32))?;
            }
            Instruction::MemoryInit { data_idx } => {
                ensure!(self.mems.len() == 1, "memory not found");
                ensure!(
                    (data_idx as usize) < self.datas_found,
                    "data not found: {data_idx}"
                );
                self.expect_val(Operand::Exact(ValType::I32))?;
                self.expect_val(Operand::Exact(ValType::I32))?;
                self.expect_val(Operand::Exact(ValType::I32))?;
            }
            Instruction::DataDrop { data_idx } => {
                ensure!(
                    (data_idx as usize) < self.datas_found,
                    "data not found: {data_idx}"
                );
            }

            Instruction::RefIsNull => {
                let val = self.expect_val(Operand::Any)?;
                ensure!(
                    val.is_ref(),
                    "ref.is_null must be called on a reference type, got {val}"
                );
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::SelectT(t) => {
                self.expect_val(Operand::Exact(ValType::I32))?;
                // SelectT only allows one operand at this time
                ensure!(t.len() == 1, "typed select must have one operand");
                self.expect_val(Operand::Exact(t[0]))?;
                self.expect_val(Operand::Exact(t[0]))?;
                self.push_val(Operand::Exact(t[0]));
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

            Instruction::Loop(ref blockty) | Instruction::Block(ref blockty) => {
                let (labels, results) = match blockty {
                    BlockType::Empty => (&[][..], Cow::Borrowed(&[][..])),
                    BlockType::Type(ty) => (&[][..], Cow::Owned(vec![*ty])),
                    BlockType::FuncType(idx) => {
                        let ty = self.get_ty(*idx)?;
                        (&ty.0[..], Cow::Borrowed(&ty.1[..]))
                    }
                };
                self.expect_vals(labels.iter())?;
                let kind = match instr {
                    Instruction::Loop(_) => FrameKind::Loop,
                    Instruction::Block(_) => FrameKind::Block,
                    _ => unreachable!(),
                };
                self.push_frame(kind, labels, results);
            }
            Instruction::If(blockty) => {
                self.expect_val(Operand::Exact(ValType::I32))?;
                let (labels, results) = match blockty {
                    BlockType::Empty => (&[][..], Cow::Borrowed(&[][..])),
                    BlockType::Type(ty) => (&[][..], Cow::Owned(vec![ty])),
                    BlockType::FuncType(idx) => {
                        let ty = self.get_ty(idx)?;
                        (&ty.0[..], Cow::Borrowed(&ty.1[..]))
                    }
                };
                self.expect_vals(labels.iter())?;
                self.push_frame(FrameKind::If, labels, results);
            }
            Instruction::Br { depth } => {
                let frame = self
                    .get_frame_n(depth as usize)
                    .context("br to invalid depth")?;
                self.expect_vals(frame.label_ty.iter())?;
                self.unreachable();
            }
            Instruction::BrTable(br_tbl) => {
                self.expect_val(Operand::Exact(ValType::I32))?;
                let default_labels = self
                    .get_frame_n(br_tbl.default_depth as usize)
                    .context("br_table to invalid depth")?
                    .label_ty;
                let arity = default_labels.len();
                for depth in br_tbl.depths {
                    let vals = self
                        .get_frame_n(depth as usize)
                        .context("br_table to invalid depth")?
                        .label_ty;
                    ensure!(
                        vals.len() == arity,
                        "br_table arity mismatch, got {depth}, want {arity}"
                    );
                    self.expect_vals(vals.iter())?;
                }
                self.expect_vals(default_labels.iter())?;
                self.unreachable();
            }
            Instruction::BrIf { depth } => {
                self.expect_val(Operand::Exact(ValType::I32))?;
                let frame = self
                    .get_frame_n(depth as usize)
                    .context("br_if to invalid depth")?;
                self.expect_vals(frame.label_ty.iter())?;
                // Need to get new reference for borrowck. Should be the same frame as before as
                // long as `expect_vals` doesn't push or pop a frame (which it doesn't)
                let frame = self.get_frame_n(depth as usize).unwrap();
                self.push_vals(frame.label_ty);
                self.unreachable();
            }

            // Const instrs
            Instruction::I32Const(_) => self.push_val(Operand::Exact(ValType::I32)),
            Instruction::F32Const(_) => self.push_val(Operand::Exact(ValType::F32)),
            Instruction::I64Const(_) => self.push_val(Operand::Exact(ValType::I64)),
            Instruction::F64Const(_) => self.push_val(Operand::Exact(ValType::F64)),

            Instruction::Call { func_idx } => {
                let ty = self.get_functype(func_idx)?;
                self.expect_vals(ty.0.iter())?;
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
                self.expect_val(Operand::Exact(ty))?;
            }
            Instruction::LocalTee { idx } => {
                let ty = *self
                    .current_func
                    .locals
                    .get(idx as usize)
                    .context("local.tee out of bounds")?;
                self.expect_val(Operand::Exact(ty))?;
                self.push_val(Operand::Exact(ty));
            }
            Instruction::GlobalGet { idx } => {
                let ty = self
                    .globals
                    .get(idx as usize)
                    .context("global.get out of bounds")?
                    .content_type;
                self.push_val(Operand::Exact(ty));
            }
            Instruction::GlobalSet { idx } => {
                let ty = self
                    .globals
                    .get(idx as usize)
                    .context("global.set out of bounds")?
                    .content_type;
                self.expect_val(Operand::Exact(ty))?;
            }
            Instruction::ElemDrop { elem_idx } => {
                ensure!(
                    (elem_idx as usize) < self.elems.len(),
                    "elem not found: {elem_idx}"
                );
            }
            Instruction::RefFunc { func_idx } => {
                ensure!(
                    (func_idx as usize) < self.func_tys.len(),
                    "function type not found: {func_idx}"
                );
                self.push_val(Operand::Exact(ValType::Func));
            }
            Instruction::TableGet { table } => {
                let ty = self
                    .tables
                    .get(table as usize)
                    .context("table.get targets invalid table")?
                    .elem_type;
                self.expect_val(Operand::Exact(ValType::I32))?;
                self.push_val(Operand::Exact(ty.into()));
            }
            Instruction::TableSet { table } => {
                let ty = self
                    .tables
                    .get(table as usize)
                    .context("table.set targets invalid table")?
                    .elem_type;
                self.expect_val(Operand::Exact(ty.into()))?;
                self.expect_val(Operand::Exact(ValType::I32))?;
            }
            Instruction::TableGrow { table } => {
                let ty = self
                    .tables
                    .get(table as usize)
                    .context("table.grow targets invalid table")?
                    .elem_type;
                self.expect_val(Operand::Exact(ValType::I32))?;
                self.expect_val(Operand::Exact(ty.into()))?;
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::TableSize { table } => {
                ensure!(
                    self.tables.len() > table as usize,
                    "table.size table not found: {table}"
                );
                self.push_val(Operand::Exact(ValType::I32));
            }
            Instruction::TableFill { table } => {
                let ty = self
                    .tables
                    .get(table as usize)
                    .context("table.fill targets invalid table")?
                    .elem_type;
                self.expect_val(Operand::Exact(ValType::I32))?;
                self.expect_val(Operand::Exact(ty.into()))?;
                self.expect_val(Operand::Exact(ValType::I32))?;
            }
            Instruction::RefNull { ty } => {
                self.push_val(Operand::Exact(ty.into()));
            }
            Instruction::CallIndirect {
                type_idx,
                table_idx,
            } => {
                let table = &self
                    .tables
                    .get(table_idx as usize)
                    .context("call.indirect table not found")?;
                ensure!(
                    table.elem_type == RefType::Func,
                    "call.indirect table elem type must be of type funcref, got: {}",
                    table.elem_type
                );
                let ty = *self
                    .func_tys
                    .get(type_idx as usize)
                    .context("function type not found")?;
                self.expect_val(Operand::Exact(ValType::I32))?;
                self.expect_vals(ty.0.iter())?;
                self.push_vals(&ty.1);
            }
            Instruction::TableCopy {
                src_table,
                dst_table,
            } => {
                ensure!(
                    self.tables.len() > src_table as usize,
                    "table.copy src table not found: {src_table}"
                );
                ensure!(
                    self.tables.len() > dst_table as usize,
                    "table.copy dst table not found: {dst_table}"
                );
                let src_ty = self.tables[src_table as usize].elem_type;
                let dst_ty = self.tables[dst_table as usize].elem_type;
                ensure!(
                    src_ty == dst_ty,
                    "table.copy src and dst types must match, got {src_ty} and {dst_ty}"
                );
                self.expect_val(Operand::Exact(ValType::I32))?;
                self.expect_val(Operand::Exact(ValType::I32))?;
                self.expect_val(Operand::Exact(ValType::I32))?;
            }
            Instruction::TableInit {
                elem_idx,
                table_idx,
            } => {
                ensure!(
                    (table_idx as usize) < self.tables.len(),
                    "table not found: {table_idx}"
                );
                ensure!(
                    (elem_idx as usize) < self.elems.len(),
                    "elem not found: {elem_idx}"
                );
                let table = &self.tables[table_idx as usize];
                let elem = &self.elems[elem_idx as usize];
                ensure!(
                    table.elem_type == elem.types,
                    "table.init table and elem types must match, got {} and {}",
                    table.elem_type,
                    elem.types
                );
                self.expect_val(Operand::Exact(ValType::I32))?;
                self.expect_val(Operand::Exact(ValType::I32))?;
                self.expect_val(Operand::Exact(ValType::I32))?;
            }
        }

        Ok(())
    }

    fn validate_mem_load(
        &mut self,
        memarg: MemArg,
        valtype: ValType,
        size_override: Option<u32>,
    ) -> Result<()> {
        ensure!(self.mems.len() == 1, "memory not found");
        let size = size_override.unwrap_or_else(|| match valtype {
            ValType::I32 | ValType::F32 => 32,
            ValType::I64 | ValType::F64 => 64,
            _ => panic!("should not be called with reftypes"),
        });
        ensure!(
            1 << memarg.align <= size / 8,
            "memory load operation alignment must not be larger than natural alignment"
        );
        self.expect_val(Operand::Exact(ValType::I32))?;
        self.push_val(Operand::Exact(valtype));

        Ok(())
    }

    fn validate_mem_store(
        &mut self,
        memarg: MemArg,
        valtype: ValType,
        size_override: Option<u32>,
    ) -> Result<()> {
        ensure!(self.mems.len() == 1, "memory not found");
        let size = size_override.unwrap_or_else(|| match valtype {
            ValType::I32 | ValType::F32 => 32,
            ValType::I64 | ValType::F64 => 64,
            _ => panic!("should not be called with reftypes"),
        });
        ensure!(
            1 << memarg.align <= size / 8,
            "memory store operation alignment must not be larger than natural alignment"
        );
        self.expect_val(Operand::Exact(valtype))?;
        self.expect_val(Operand::Exact(ValType::I32))?;

        Ok(())
    }
}
