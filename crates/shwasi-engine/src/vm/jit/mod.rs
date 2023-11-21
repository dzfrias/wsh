mod assembler;
mod executable;

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

        let mut used = Vec::new();
        let mut free = Operand::Reg(Reg::GPR0);
        let mut total_stack_size = 0;
        // This nop will be patched to subtract the stack size from the stack pointer later.
        self.asm.nop();
        for instr in &code.body {
            match instr {
                I::Nop => {}
                I::LocalGet { idx } => {
                    self.asm.load_local(free, idx);
                    used.push(free);
                    free.next_free();
                }
                I::I32Const(val) => {
                    self.asm.mov(free, val as u64);
                    used.push(free);
                    free.next_free();
                }
                I::I32Add => {
                    let reg = used.pop().unwrap();
                    let dst = used.last().unwrap();
                    self.asm.add(*dst, reg);
                    free = reg;
                }
                I::LocalSet { idx } => {
                    let reg = used.pop().unwrap();
                    self.asm.store_local(idx, reg);
                    free = reg;
                }
                I::End => {
                    for (offset, op) in used.iter().enumerate() {
                        self.asm.store_return(offset as u32, *op);
                    }
                    break;
                }
                _ => return Err(CompilationError::UnsupportedInstruction(instr)),
            }
            if let Operand::Mem64(reg, offset) = free {
                debug_assert!(reg == Reg::Sp);
                total_stack_size = total_stack_size.max(offset);
            }
        }
        // times 8 because each mem64 item is 8 bytes
        total_stack_size *= 8;
        if total_stack_size > 0 {
            if total_stack_size % 16 != 0 {
                total_stack_size += 8;
            }
            // Patch the nop
            self.asm.stack_reserve(0, total_stack_size);
            self.asm.stack_restore(total_stack_size);
        }
        self.asm.ret();

        // SAFETY: The code is valid, as long as the assembler and compiler are correct. If they
        // aren't, then that's a bug...
        unsafe { Executable::map(&self.asm.consume()).map_err(CompilationError::ExecMapError) }
    }
}

#[cfg(test)]
mod tests {
    use shwasi_parser::InstrBuffer;

    use crate::{value::ValueUntyped, Value};

    use super::*;

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
        assert_eq!([Value::I32(42).untyped(), Value::I32(32).untyped()], out)
    }
}
