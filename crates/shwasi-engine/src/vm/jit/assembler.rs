#![allow(dead_code)]

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Reg {
    GPR0 = 8,
    GPR1 = 9,
    GPR2 = 10,
    /// A temporary register used for loading and storing memory.
    LoadTemp = 24,
    /// A temporary register used for loading and storing memory.
    LoadTemp2 = 25,

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

impl Operand {
    pub fn next_free(&mut self) {
        match self {
            Self::Reg(reg) => match reg {
                Reg::GPR0 => *self = Self::Reg(Reg::GPR1),
                Reg::GPR1 => *self = Self::Reg(Reg::GPR2),
                // Use stack space when there are no more GPRs
                Reg::GPR2 => *self = Self::Mem64(Reg::Sp, 0),
                _ => panic!("invalid register"),
            },
            Self::Mem64(Reg::Sp, offset) => *offset += 1,
            Self::Mem64(reg, _) => panic!("mem invalid register: {reg:?}"),
            Self::Imm64(_) => panic!("cannot increment an immediate"),
        }
    }
}

#[derive(Debug)]
pub struct Assembler {
    out: Vec<u8>,
}

impl Assembler {
    pub fn new() -> Self {
        Self { out: Vec::new() }
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
                self.emit_u32(0xf2000000 | (0b101 << 21) | (mid_low << 5) | reg as u32);
                self.emit_u32(0xf2000000 | (0b110 << 21) | (mid_high << 5) | reg as u32);
                self.emit_u32(0xf2000000 | (0b111 << 21) | (high << 5) | reg as u32);
            }
            (Operand::Reg(dst), Operand::Reg(src)) => {
                // mov (reg)
                self.emit_u32(0xaa0003e0 | (src as u32) << 16 | (dst as u32));
            }
            (Operand::Mem64(base, offset), Operand::Imm64(imm64)) => {
                // mov into memory is equivalent to a store.
                self.emit_store(offset as u32, base, imm64);
            }
            (dst, src) => panic!("unsupported mov operands: {:?} and {:?}", dst, src),
        }
    }

    pub fn load_local(&mut self, dst: impl Into<Operand>, idx: u32) {
        self.emit_load(idx, Reg::LocalsArrayBase, dst);
    }

    pub fn nop(&mut self) {
        self.emit_u32(0xd503201f);
    }

    pub fn ret(&mut self) {
        self.emit_u32(0xd65f03c0);
    }

    /// Return the assembled code.
    pub fn consume(self) -> Vec<u8> {
        self.out
    }

    pub fn store_local(&mut self, idx: u32, from: impl Into<Operand>) {
        self.emit_store(idx, Reg::LocalsArrayBase, from);
    }

    pub fn add(&mut self, dst: impl Into<Operand>, src: impl Into<Operand>) {
        match (dst.into(), src.into()) {
            (Operand::Reg(dst), Operand::Reg(src)) => {
                // add dst, dst, src
                self.emit_u32(0x8b000000 | (src as u32) << 16 | (dst as u32) << 5 | (dst as u32));
            }
            (Operand::Reg(dst), Operand::Imm64(imm64)) => {
                // add dst, dst, imm64
                // TODO: split up imm64 so that this actually works
                self.emit_u32(0x91000000 | (imm64 as u32) << 10 | (dst as u32) << 5 | (dst as u32));
            }
            (Operand::Mem64(base, offset), Operand::Mem64(base2, offset2)) => {
                self.emit_load(offset as u32, base, Reg::LoadTemp);
                self.emit_load(offset2 as u32, base2, Reg::LoadTemp2);
                self.add(Reg::LoadTemp, Reg::LoadTemp2);
                self.emit_store(offset as u32, base, Reg::LoadTemp);
            }
            (op1, op2) => panic!("unsupported add operands: {op1:?} and {op2:?}"),
        }
    }

    pub fn sub(&mut self, dst: impl Into<Operand>, src: impl Into<Operand>) {
        match (dst.into(), src.into()) {
            (Operand::Reg(dst), Operand::Reg(src)) => {
                // sub dst, dst, src
                self.emit_u32(0xcb000000 | (src as u32) << 16 | (dst as u32) << 5 | (dst as u32));
            }
            (Operand::Reg(dst), Operand::Imm64(imm64)) => {
                // sub dst, dst, imm64
                // TODO: split up imm64 so that this actually works
                self.emit_u32(0xd1000000 | (imm64 as u32) << 10 | (dst as u32) << 5 | (dst as u32));
            }
            _ => todo!(),
        }
    }

    pub fn stack_reserve(&mut self, patch_idx: usize, size: u64) {
        // sub sp, sp, size
        // TODO: size should be split up
        self.patch(
            patch_idx,
            0xd1000000 | (size as u32) << 10 | (Reg::Sp as u32) << 5 | (Reg::Sp as u32),
        );
    }

    pub fn stack_restore(&mut self, size: u64) {
        // add sp, sp, size
        self.add(Reg::Sp, size);
    }

    pub fn store_return(&mut self, idx: u32, src: impl Into<Operand>) {
        self.emit_store(idx, Reg::OutBase, src);
    }

    fn emit_store(&mut self, idx: u32, base: Reg, src: impl Into<Operand>) {
        match src.into() {
            Operand::Reg(src) => {
                // str src, [base, idx]
                self.emit_u32(0xf9000000 | (idx << 10) | (base as u32) << 5 | src as u32);
            }
            Operand::Mem64(mem_base, offset) => {
                self.emit_load(offset as u32, mem_base, Reg::LoadTemp);
                // We effectively store the memory in the new location by copying it from one area
                // to another.
                self.emit_store(idx, base, Reg::LoadTemp);
            }
            Operand::Imm64(imm64) => {
                self.mov(Reg::LoadTemp, imm64);
                self.emit_store(idx, base, Reg::LoadTemp);
            }
        }
    }

    fn emit_load(&mut self, idx: u32, base: Reg, dst: impl Into<Operand>) {
        match dst.into() {
            Operand::Reg(dst) => {
                // ldr dst, [base, idx]
                self.emit_u32(0xf9400000 | (idx << 10) | (base as u32) << 5 | dst as u32);
            }
            Operand::Mem64(mem_base, offset) => {
                self.emit_load(idx, base, Reg::LoadTemp);
                self.emit_store(offset as u32, mem_base, Reg::LoadTemp);
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

    fn patch(&mut self, patch: usize, u: u32) {
        self.out[patch] = (u & 0xff) as u8;
        self.out[patch + 1] = ((u >> 8) & 0xff) as u8;
        self.out[patch + 2] = ((u >> 16) & 0xff) as u8;
        self.out[patch + 3] = ((u >> 24) & 0xff) as u8;
    }
}

#[cfg(test)]
mod tests {
    use std::fmt;

    use super::*;

    /// Wrapper around a vector of u32s that implements prints as hex.
    #[derive(PartialEq)]
    struct Hex(Vec<u32>);

    impl fmt::Debug for Hex {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            fmt::Display::fmt(self, f)
        }
    }

    impl fmt::Display for Hex {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            writeln!(f)?;
            for op in &self.0 {
                writeln!(f, "{:x}", op)?;
            }
            Ok(())
        }
    }

    #[derive(PartialEq)]
    /// Wrapper around a vector of u32s that implements prints as binary.
    struct Binary(Vec<u32>);

    impl fmt::Debug for Binary {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            fmt::Display::fmt(self, f)
        }
    }

    impl fmt::Display for Binary {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            writeln!(f)?;
            for op in &self.0 {
                writeln!(f, "{:b}", op)?;
            }
            Ok(())
        }
    }

    fn to_32(bytes: &[u8]) -> Vec<u32> {
        bytes
            .chunks(4)
            .map(|w| u32::from_le_bytes(w.try_into().unwrap()))
            .collect()
    }

    #[test]
    fn mov_imm64() {
        let mut asm = Assembler::new();
        asm.mov(Reg::GPR0, 0xbabecafef00dface);
        let code = to_32(&asm.consume());
        assert_eq!(
            Hex(code),
            Hex(vec![0xd29f59c8, 0xf2be01a8, 0xf2d95fc8, 0xf2f757c8])
        );
    }

    #[test]
    fn mov_reg() {
        let mut asm = Assembler::new();
        asm.mov(Reg::GPR0, Reg::GPR0);
        let code = to_32(&asm.consume());
        assert_eq!(Hex(code), Hex(vec![0xaa0803e8]));
    }

    #[test]
    fn load_local() {
        let mut asm = Assembler::new();
        asm.load_local(Reg::GPR0, 0);
        asm.load_local(Reg::GPR1, 1);
        asm.load_local(Reg::GPR2, 2);
        let code = to_32(&asm.consume());
        assert_eq!(Hex(code), Hex(vec![0xf9400008, 0xf9400409, 0xf940080a]));
    }

    #[test]
    fn store_local() {
        let mut asm = Assembler::new();
        asm.store_local(0, Reg::GPR0);
        asm.store_local(1, Reg::GPR0);
        asm.store_local(2, Reg::GPR0);
        let code = to_32(&asm.consume());
        assert_eq!(Hex(code), Hex(vec![0xf9000008, 0xf9000408, 0xf9000808]));
    }

    #[test]
    fn store_return() {
        let mut asm = Assembler::new();
        asm.store_return(0, Reg::GPR0);
        asm.store_return(1, Reg::GPR0);
        asm.store_return(2, Reg::GPR0);
        let code = to_32(&asm.consume());
        assert_eq!(Hex(code), Hex(vec![0xf9000028, 0xf9000428, 0xf9000828]));
    }

    #[test]
    fn add() {
        let mut asm = Assembler::new();
        asm.add(Reg::GPR0, Reg::GPR1);
        let code = to_32(&asm.consume());
        assert_eq!(Hex(code), Hex(vec![0x8b090108]));
    }

    #[test]
    fn stack_reserve_and_restore() {
        let mut asm = Assembler::new();
        asm.nop();
        asm.stack_reserve(0, 0x60);
        asm.stack_restore(0x60);
        let code = to_32(&asm.consume());
        assert_eq!(Hex(code), Hex(vec![0xd10183ff, 0x910183ff]));
    }
}
