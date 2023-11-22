#![allow(dead_code)]

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Reg {
    GPR0 = 8,
    GPR1 = 9,
    GPR2 = 10,
    /// A temporary register used for moving memory around.
    LoadTemp = 24,
    /// A temporary register used for moving memory around.
    LoadTemp2 = 25,
    /// A temporary register used for moving memory around.
    LoadTemp3 = 26,

    Sp = 31,

    /// The register that holds the base address of the locals array. This should be the first
    /// argument to the JIT-compiled memory.
    LocalsArrayBase = 0,
    /// The register that holds the base address of the output array. This should be the second
    /// argument to the JIT-compiled memory.
    OutBase = 1,
}

impl From<Reg> for Operand {
    fn from(value: Reg) -> Self {
        Self::Reg(value)
    }
}

impl From<u64> for Operand {
    fn from(value: u64) -> Self {
        Self::Imm64(value)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    Reg(Reg),
    Imm64(u64),
    Mem64(Reg, u64),
}

#[derive(Debug)]
pub struct Assembler {
    out: Vec<u8>,
}

impl Assembler {
    pub fn new() -> Self {
        Self { out: Vec::new() }
    }

    /// Return the assembled code.
    pub fn consume(self) -> Vec<u8> {
        self.out
    }

    pub fn mov(&mut self, dst: impl Into<Operand>, src: impl Into<Operand>) {
        match (dst.into(), src.into()) {
            (Operand::Reg(reg), Operand::Imm64(imm64)) => {
                let low = (imm64 & 0xffff) as u32;
                let mid_low = ((imm64 >> 16) & 0xffff) as u32;
                let mid_high = ((imm64 >> 32) & 0xffff) as u32;
                let high = ((imm64 >> 48) & 0xffff) as u32;
                // mov (immediate)
                self.emit_u32(0xd2800000 | (low << 5) | reg as u32);
                // The following are movk's that shift incrementally
                if mid_low != 0 {
                    self.emit_u32(0xf2000000 | (0b101 << 21) | (mid_low << 5) | reg as u32);
                }
                if mid_high != 0 {
                    self.emit_u32(0xf2000000 | (0b110 << 21) | (mid_high << 5) | reg as u32);
                }
                if high != 0 {
                    self.emit_u32(0xf2000000 | (0b111 << 21) | (high << 5) | reg as u32);
                }
            }
            (Operand::Reg(dst), Operand::Reg(src)) => {
                // mov (reg)
                self.emit_u32(0xaa0003e0 | (src as u32) << 16 | (dst as u32));
            }
            (Operand::Mem64(base, offset), Operand::Imm64(imm64)) => {
                // mov into memory is equivalent to a store.
                self.store(offset as u32, base, imm64);
            }
            (dst, src) => panic!("unsupported mov operands: {:?} and {:?}", dst, src),
        }
    }

    pub fn load_local(&mut self, dst: impl Into<Operand>, idx: u32) {
        self.load(dst, idx, Reg::LocalsArrayBase);
    }

    pub fn nop(&mut self) {
        self.emit_u32(0xd503201f);
    }

    pub fn ret(&mut self) {
        self.emit_u32(0xd65f03c0);
    }

    pub fn store_local(&mut self, idx: u32, from: impl Into<Operand>) {
        self.store(idx, Reg::LocalsArrayBase, from);
    }

    /// Patch a set of instructions at the given index. Anything called in the given function will
    /// be redirected to the patch index.
    pub fn patch<F>(&mut self, idx: u32, patch_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        let mut patch = Self::new();
        patch_fn(&mut patch);
        let patch = patch.consume();
        self.out[idx as usize..idx as usize + patch.len()].copy_from_slice(&patch);
    }

    pub fn store_return(&mut self, idx: u32, src: impl Into<Operand>) {
        self.store(idx, Reg::OutBase, src);
    }

    pub fn add(
        &mut self,
        dst: impl Into<Operand>,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
    ) {
        let old_dst = dst.into();
        let dst_reg = self.dst_reg(old_dst);
        let (lhs, rhs) = self.register_or_immediate(lhs, rhs);
        match (lhs, rhs) {
            (Operand::Reg(lhs), Operand::Reg(rhs)) => {
                self.emit_u32(
                    0x8b000000 | (rhs as u32) << 16 | (lhs as u32) << 5 | (dst_reg as u32),
                );
            }
            (Operand::Reg(lhs), Operand::Imm64(rhs)) => {
                // TODO: split up imm64 so that this actually works
                self.emit_u32(
                    0x91000000 | (rhs as u32) << 10 | (lhs as u32) << 5 | (dst_reg as u32),
                );
            }
            (Operand::Imm64(lhs), Operand::Imm64(rhs)) => {
                self.mov(dst_reg, lhs.saturating_add(rhs));
            }
            (op1, op2) => {
                // We switch the operands here. This really only accounts for one case (the second
                // one in this match arm), but it's easier to just switch them here than to repeat.
                self.add(dst_reg, op2, op1);
            }
        }
        if let Operand::Mem64(base, offset) = old_dst {
            self.store(offset as u32, base, dst_reg);
        }
    }

    pub fn sub(
        &mut self,
        dst: impl Into<Operand>,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
    ) {
        let old_dst = dst.into();
        let dst_reg = self.dst_reg(old_dst);
        let (lhs, rhs) = self.register_or_immediate(lhs, rhs);
        match (lhs, rhs) {
            (Operand::Reg(lhs), Operand::Reg(rhs)) => {
                self.emit_u32(
                    0xcb000000 | (rhs as u32) << 16 | (lhs as u32) << 5 | (dst_reg as u32),
                );
            }
            (Operand::Reg(lhs), Operand::Imm64(rhs)) => {
                // TODO: split up imm64 so that this actually works
                self.emit_u32(
                    0xd1000000 | (rhs as u32) << 10 | (lhs as u32) << 5 | (dst_reg as u32),
                );
            }
            (Operand::Imm64(lhs), Operand::Imm64(rhs)) => {
                self.mov(dst_reg, lhs.saturating_sub(rhs));
            }
            (op1, op2) => {
                self.sub(dst_reg, op2, op1);
            }
        }
        if let Operand::Mem64(base, offset) = old_dst {
            self.store(offset as u32, base, dst_reg);
        }
    }

    pub fn store(&mut self, idx: u32, base: Reg, src: impl Into<Operand>) {
        match src.into() {
            Operand::Reg(src) => {
                // str src, [base, idx]
                self.emit_u32(0xf9000000 | (idx << 10) | (base as u32) << 5 | src as u32);
            }
            Operand::Mem64(mem_base, offset) => {
                self.load(Reg::LoadTemp, offset as u32, mem_base);
                // We effectively store the memory in the new location by copying it from one area
                // to another.
                self.store(idx, base, Reg::LoadTemp);
            }
            Operand::Imm64(imm64) => {
                self.mov(Reg::LoadTemp, imm64);
                self.store(idx, base, Reg::LoadTemp);
            }
        }
    }

    pub fn load(&mut self, dst: impl Into<Operand>, idx: u32, base: Reg) {
        match dst.into() {
            Operand::Reg(dst) => {
                // ldr dst, [base, idx]
                self.emit_u32(0xf9400000 | (idx << 10) | (base as u32) << 5 | dst as u32);
            }
            Operand::Mem64(mem_base, offset) => {
                self.load(Reg::LoadTemp, idx, base);
                self.store(offset as u32, mem_base, Reg::LoadTemp);
            }
            Operand::Imm64(_) => panic!("cannot load into an immediate"),
        }
    }

    fn emit(&mut self, b: u8) {
        self.out.push(b);
    }

    fn emit_u32(&mut self, u: u32) {
        self.emit((u & 0xff) as u8);
        self.emit(((u >> 8) & 0xff) as u8);
        self.emit(((u >> 16) & 0xff) as u8);
        self.emit(((u >> 24) & 0xff) as u8);
    }

    /// Returns the register, or loads it into a temporary register if it is a memory offset.
    /// [`Reg::LoadTemp3`] is used as a temporary register.
    ///
    /// # Panics
    /// Panics if the operand is an immediate.
    fn dst_reg(&mut self, op: impl Into<Operand>) -> Reg {
        match op.into() {
            Operand::Reg(reg) => reg,
            Operand::Mem64(base, offset) => {
                self.load(Reg::LoadTemp3, offset as u32, base);
                Reg::LoadTemp3
            }
            Operand::Imm64(_) => panic!("cannot use an immediate as a register"),
        }
    }

    /// Returns the operands as registers, or loads them into registers if they are memory offsets.
    /// Immediates are unchanged.
    ///
    /// Specifically, [`Reg::LoadTemp`] and [`Reg::LoadTemp2`] are used as temporary registers.
    fn register_or_immediate(
        &mut self,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
    ) -> (Operand, Operand) {
        let mut result = (lhs.into(), rhs.into());
        let mut used_load_temp = false;
        if let Operand::Mem64(base, offset) = result.0 {
            self.load(Reg::LoadTemp, offset as u32, base);
            result.0 = Reg::LoadTemp.into();
            used_load_temp = true;
        }
        if let Operand::Mem64(base, offset) = result.1 {
            let dst = if used_load_temp {
                Reg::LoadTemp2.into()
            } else {
                Reg::LoadTemp.into()
            };
            self.load(dst, offset as u32, base);
            result.1 = dst;
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use std::fmt;

    use capstone::prelude::*;

    use super::*;

    /// Wrapper around a vector of u32s that implements prints as hex.
    #[derive(PartialEq)]
    struct Hex<'a>(&'a [u8]);

    impl fmt::Display for Hex<'_> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            for b in to_32(self.0) {
                write!(f, "{:x}", b)?;
            }
            Ok(())
        }
    }

    fn to_32(bytes: &[u8]) -> &[u32] {
        unsafe {
            std::slice::from_raw_parts(
                bytes.as_ptr().cast::<u32>(),
                bytes.len() / std::mem::size_of::<u32>(),
            )
        }
    }

    fn to_8(bytes: &[u32]) -> &[u8] {
        unsafe {
            std::slice::from_raw_parts(bytes.as_ptr().cast::<u8>(), std::mem::size_of_val(bytes))
        }
    }

    macro_rules! pretty_assert_eq {
        ($left:expr, $right:expr) => {
            if $left != to_32($right) {
                let capstone = Capstone::new()
                    .arm64()
                    .mode(capstone::arch::arm64::ArchMode::Arm)
                    .build()
                    .unwrap();
                let got_instrs = capstone.disasm_all($right, 0x1000).unwrap();
                let expect_instrs = capstone.disasm_all(&to_8($left), 0x1000).unwrap();
                panic!(
                    "assertion failed:
got:
{}
want:
{}",
                    got_instrs.iter().fold(String::new(), |acc, i| format!(
                        "{acc} 0x{:x}  {}    {} {}\n",
                        i.address(),
                        Hex(i.bytes()),
                        i.mnemonic().unwrap(),
                        i.op_str().unwrap(),
                    )),
                    expect_instrs.iter().fold(String::new(), |acc, i| format!(
                        "{acc} 0x{:x}  {}    {} {}\n",
                        i.address(),
                        Hex(i.bytes()),
                        i.mnemonic().unwrap(),
                        i.op_str().unwrap(),
                    )),
                );
            }
        };
        () => {};
    }

    #[test]
    fn mov_imm64() {
        let mut asm = Assembler::new();
        asm.mov(Reg::GPR0, 0xbabecafef00dface);
        let code = asm.consume();
        pretty_assert_eq!(&[0xd29f59c8, 0xf2be01a8, 0xf2d95fc8, 0xf2f757c8], &code);
    }

    #[test]
    fn mov_reg() {
        let mut asm = Assembler::new();
        asm.mov(Reg::GPR0, Reg::GPR0);
        let code = asm.consume();
        pretty_assert_eq!(&[0xaa0803e8], &code);
    }

    #[test]
    fn load_local() {
        let mut asm = Assembler::new();
        asm.load_local(Reg::GPR0, 0);
        asm.load_local(Reg::GPR1, 1);
        asm.load_local(Reg::GPR2, 2);
        let code = asm.consume();
        pretty_assert_eq!(&[0xf9400008, 0xf9400409, 0xf940080a], &code);
    }

    #[test]
    fn store_local() {
        let mut asm = Assembler::new();
        asm.store_local(0, Reg::GPR0);
        asm.store_local(1, Reg::GPR0);
        asm.store_local(2, Reg::GPR0);
        let code = asm.consume();
        pretty_assert_eq!(&[0xf9000008, 0xf9000408, 0xf9000808], &code);
    }

    #[test]
    fn store_return() {
        let mut asm = Assembler::new();
        asm.store_return(0, Reg::GPR0);
        asm.store_return(1, Reg::GPR0);
        asm.store_return(2, Reg::GPR0);
        let code = asm.consume();
        pretty_assert_eq!(&[0xf9000028, 0xf9000428, 0xf9000828], &code);
    }

    #[test]
    fn add() {
        let mut asm = Assembler::new();
        asm.add(Reg::GPR0, Reg::GPR0, Reg::GPR1);
        let code = asm.consume();
        pretty_assert_eq!(&[0x8b090108], &code);
    }

    #[test]
    fn add_mem_addrs() {
        let mut asm = Assembler::new();
        asm.add(
            Reg::GPR0,
            Operand::Mem64(Reg::Sp, 0),
            Operand::Mem64(Reg::Sp, 1),
        );
        let code = asm.consume();
        pretty_assert_eq!(&[0xf94003f8, 0xf94007f9, 0x8b190308], &code);
    }

    #[test]
    fn tiny_mov() {
        let mut asm = Assembler::new();
        asm.mov(Reg::GPR0, 0x1);
        let code = asm.consume();
        // Make sure `asm.mov()` doesn't emit a superfluous `movk` instructions
        pretty_assert_eq!(&[0xd2800028], &code);
    }
}
