mod assembler;
mod executable;

use shwasi_parser::{Code, Instruction};
use thiserror::Error;

use self::assembler::Assembler;
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

    pub fn compile(self, code: &Code) -> Result<Executable, CompilationError> {
        use Instruction as I;

        for instr in &code.body {
            match instr {
                // Somewhat counterintuitively, we don't need to push a nop at a wasm nop
                I::Nop => {}
                _ => return Err(CompilationError::UnsupportedInstruction(instr)),
            }
        }

        Executable::map(&self.asm.consume()).map_err(CompilationError::ExecMapError)
    }
}
