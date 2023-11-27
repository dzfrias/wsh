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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operand {
    Reg(Reg),
    Imm64(u64),
    Mem64(Reg, u64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub enum ConditionCode {
    Eq = 0b0000,
    Ne = 0b0001,
    Hs = 0b0010,
    Lo = 0b0011,
    Mi = 0b0100,
    Pl = 0b0101,
    Vs = 0b0110,
    Vc = 0b0111,
    Hi = 0b1000,
    Ls = 0b1001,
    Ge = 0b1010,
    Lt = 0b1011,
    Gt = 0b1100,
    Le = 0b1101,
    Al = 0b1110,
    Nv = 0b1111,
}

#[derive(Debug, Default, Clone)]
pub struct Assembler {
    out: Vec<u8>,
    addr_offset: usize,

    dst_mem: Option<(Reg, u32)>,
}

impl Assembler {
    pub fn new() -> Self {
        Self {
            out: Vec::new(),
            addr_offset: 0,
            dst_mem: None,
        }
    }

    /// Return the assembled code.
    pub fn consume(&mut self) -> Vec<u8> {
        std::mem::take(&mut self.out)
    }

    pub fn mov(&mut self, dst: impl Into<Operand>, src: impl Into<Operand>) {
        match (dst.into(), src.into()) {
            (Operand::Reg(reg), Operand::Imm64(imm64)) => {
                let [low, mid_low, mid_high, high] = split_u64::<16, 4>(imm64);
                // mov (immediate)
                self.emit_u32(0xd2800000 | ((low as u32) << 5) | reg as u32);
                // The following are movk's that shift incrementally. We don't want to compile them
                // if the immediate's bits are representable by just the least significant 16.
                if mid_low != 0 {
                    self.emit_u32(
                        0xf2000000 | (0b101 << 21) | ((mid_low as u32) << 5) | reg as u32,
                    );
                }
                if mid_high != 0 {
                    self.emit_u32(
                        0xf2000000 | (0b110 << 21) | ((mid_high as u32) << 5) | reg as u32,
                    );
                }
                if high != 0 {
                    self.emit_u32(0xf2000000 | (0b111 << 21) | ((high as u32) << 5) | reg as u32);
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
            (Operand::Mem64(base, offset), Operand::Mem64(base2, offset2)) => {
                self.load(Reg::LoadTemp, offset2 as u32, base2);
                self.store(offset as u32, base, Reg::LoadTemp);
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

    pub fn addr(&self) -> usize {
        self.addr_offset + self.out.len()
    }

    pub fn branch(&mut self, to: u64) {
        let offset = to.wrapping_sub(self.addr() as u64);
        // We only have 26 bits to encode the offset
        let truncated_offset = offset & 0b1111111111111111111111111111;
        let offset = truncated_offset / 4;
        self.emit_u32(0x14000000 | (offset as u32));
    }

    pub fn branch_if(&mut self, to: u64, cond: ConditionCode) {
        let offset = to.wrapping_sub(self.addr() as u64);
        // We only have 19 bits to encode the offset
        let truncated_offset = offset & 0b111111111111111111111;
        let offset = truncated_offset / 4;
        self.emit_u32(0x54000000 | (offset as u32) << 5 | cond as u32);
    }

    /// Patch a set of instructions at the given index. Anything called in the given function will
    /// be redirected to the patch index.
    pub fn patch<F>(&mut self, idx: usize, patch_fn: F)
    where
        F: FnOnce(&mut Self),
    {
        let mut patch = Self::new();
        patch.addr_offset = idx;
        patch_fn(&mut patch);
        let patch = patch.consume();
        self.out[idx..idx + patch.len()].copy_from_slice(&patch);
    }

    pub fn add(
        &mut self,
        dst: impl Into<Operand>,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
    ) {
        let dst_reg = self.resolve_dst(dst);
        let (lhs, rhs) = self.resolve(lhs, rhs);
        match (lhs, rhs) {
            (Operand::Reg(lhs), Operand::Reg(rhs)) => {
                self.emit_u32(
                    0x8b000000 | (rhs as u32) << 16 | (lhs as u32) << 5 | (dst_reg as u32),
                );
            }
            (Operand::Reg(lhs), Operand::Imm64(rhs)) => {
                // 4095 is 2 ^ 12 - 1, which is the maximum value that can be represented in 12
                // bits. The add immediate instruction only supports 12 bits, so in case our rhs >
                // 4095, we need to move it to a temporary register, then add it.
                if rhs > 4095 {
                    self.mov(Reg::LoadTemp, rhs);
                    self.add(dst_reg, lhs, Reg::LoadTemp);
                } else {
                    self.emit_u32(
                        0x91000000 | (rhs as u32) << 10 | (lhs as u32) << 5 | (dst_reg as u32),
                    );
                }
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
        self.restore_dst();
    }

    pub fn eq(
        &mut self,
        dst: impl Into<Operand>,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
    ) {
        let dst = self.resolve_dst(dst);
        let (lhs, rhs) = self.resolve(lhs, rhs);
        self.cmp(lhs, rhs);
        self.cset(dst, ConditionCode::Eq);
        self.restore_dst();
    }

    pub fn ne(
        &mut self,
        dst: impl Into<Operand>,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
    ) {
        let dst = self.resolve_dst(dst);
        let (lhs, rhs) = self.resolve(lhs, rhs);
        self.cmp(lhs, rhs);
        self.cset(dst, ConditionCode::Ne);
        self.restore_dst();
    }

    pub fn cmp(&mut self, lhs: impl Into<Operand>, rhs: impl Into<Operand>) {
        let (lhs, rhs) = self.resolve(lhs, rhs);
        match (lhs, rhs) {
            (Operand::Reg(lhs), Operand::Reg(rhs)) => {
                self.emit_u32(0xeb00001f | (rhs as u32) << 16 | (lhs as u32) << 5);
            }
            (Operand::Reg(lhs), Operand::Imm64(rhs)) => {
                if rhs > 4095 {
                    self.mov(Reg::LoadTemp, rhs);
                    self.cmp(lhs, Reg::LoadTemp);
                } else {
                    self.emit_u32(0xf100001f | (rhs as u32) << 10 | (lhs as u32) << 5);
                }
            }
            (Operand::Imm64(lhs), Operand::Imm64(rhs)) => {
                self.mov(Reg::LoadTemp, lhs);
                self.cmp(Reg::LoadTemp, rhs);
            }
            (op1, op2) => {
                self.cmp(op2, op1);
            }
        }
    }

    pub fn cset(&mut self, dst: impl Into<Operand>, cond: ConditionCode) {
        let dst = self.resolve_dst(dst);
        // cset condition codes have their last bit inverted
        self.emit_u32(0x9a9f07e0 | (cond.invert() as u32) << 12 | (dst as u32));
        self.restore_dst();
    }

    pub fn sub(
        &mut self,
        dst: impl Into<Operand>,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
    ) {
        let dst = self.resolve_dst(dst);
        let (lhs, rhs) = self.resolve(lhs, rhs);
        match (lhs, rhs) {
            (Operand::Reg(lhs), Operand::Reg(rhs)) => {
                self.emit_u32(0xcb000000 | (rhs as u32) << 16 | (lhs as u32) << 5 | (dst as u32));
            }
            (Operand::Reg(lhs), Operand::Imm64(rhs)) => {
                // We do this for the same reason as in `add`. See the comment there for details.
                if rhs > 4095 {
                    self.mov(Reg::LoadTemp, rhs);
                    self.sub(dst, lhs, Reg::LoadTemp);
                } else {
                    self.emit_u32(
                        0xd1000000 | (rhs as u32) << 10 | (lhs as u32) << 5 | (dst as u32),
                    );
                }
            }
            (Operand::Imm64(lhs), Operand::Imm64(rhs)) => {
                self.mov(dst, lhs.saturating_sub(rhs));
            }
            (op1, op2) => {
                self.sub(dst, op2, op1);
            }
        }
        self.restore_dst();
    }

    pub fn csel(
        &mut self,
        dst: impl Into<Operand>,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
        cond: ConditionCode,
    ) {
        let dst = self.resolve_dst(dst);
        let (lhs, rhs) = self.resolve(lhs, rhs);
        match (lhs, rhs) {
            (Operand::Reg(lhs), Operand::Reg(rhs)) => {
                self.emit_u32(
                    0x9a800000
                        | (rhs as u32) << 16
                        | (cond as u32) << 12
                        | (lhs as u32) << 5
                        | (dst as u32),
                );
            }
            (Operand::Reg(lhs), Operand::Imm64(imm64)) => {
                self.mov(Reg::LoadTemp, imm64);
                self.csel(dst, lhs, Reg::LoadTemp, cond);
            }
            (Operand::Imm64(lhs), Operand::Imm64(rhs)) => {
                self.mov(Reg::LoadTemp, lhs);
                self.mov(Reg::LoadTemp2, rhs);
                self.csel(dst, Reg::LoadTemp, Reg::LoadTemp2, cond);
            }
            (Operand::Imm64(imm64), Operand::Reg(rhs)) => {
                self.mov(Reg::LoadTemp, imm64);
                // Select is not commutative, so we can't just call with the operands switched
                self.csel(dst, Reg::LoadTemp, rhs, cond);
            }
            _ => unreachable!("should not be reachable because of `resolve`"),
        }
        self.restore_dst();
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
    /// [`Reg::LoadTemp3`] is used as the temporary register.
    ///
    /// # Panics
    /// Panics if the operand is an immediate.
    fn resolve_dst(&mut self, op: impl Into<Operand>) -> Reg {
        match op.into() {
            Operand::Reg(reg) => reg,
            Operand::Mem64(base, offset) => {
                self.dst_mem = Some((base, offset as u32));
                Reg::LoadTemp3
            }
            Operand::Imm64(_) => panic!("cannot use an immediate as a register"),
        }
    }

    /// Moves the [`Reg::LoadTemp3`] register back into memory, if it was used as a temporary in
    /// `resolve_dst`.
    ///
    /// In general, any `resolve_dst` call should have a corresponding `restore_dst` call.
    fn restore_dst(&mut self) {
        if let Some((reg, offset)) = self.dst_mem.take() {
            self.store(offset, reg, Reg::LoadTemp3);
        }
    }

    /// Returns the operands as registers, or loads them into registers if they are memory offsets.
    /// Immediates are unchanged.
    ///
    /// Specifically, [`Reg::LoadTemp`] and [`Reg::LoadTemp2`] are used as temporary registers.
    fn resolve(&mut self, lhs: impl Into<Operand>, rhs: impl Into<Operand>) -> (Operand, Operand) {
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

/// Split a u64 into an array of u64s of size A, where each element is N bits.
fn split_u64<const N: usize, const A: usize>(input: u64) -> [u64; A] {
    let mut result = [0; A];
    let mask: u64 = !0 >> (64 - N);
    let mut shift = 0;
    let mut i = 0;

    while i < A {
        let part = (input >> shift) & mask;
        result[i] = part;
        shift += N;
        i += 1;
    }

    result
}

impl ConditionCode {
    pub fn invert(self) -> Self {
        match self {
            Self::Eq => Self::Ne,
            Self::Ne => Self::Eq,
            Self::Hs => Self::Lo,
            Self::Lo => Self::Hs,
            Self::Mi => Self::Pl,
            Self::Pl => Self::Mi,
            Self::Vs => Self::Vc,
            Self::Vc => Self::Vs,
            Self::Hi => Self::Ls,
            Self::Ls => Self::Hi,
            Self::Ge => Self::Lt,
            Self::Lt => Self::Ge,
            Self::Gt => Self::Le,
            Self::Le => Self::Gt,
            Self::Al => Self::Nv,
            Self::Nv => Self::Al,
        }
    }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::jit::debug::asm_assert_eq;

    #[test]
    fn mov_imm64() {
        let mut asm = Assembler::new();
        asm.mov(Reg::GPR0, 0xbabecafef00dface);
        let code = asm.consume();
        asm_assert_eq!(&[0xd29f59c8, 0xf2be01a8, 0xf2d95fc8, 0xf2f757c8], &code);
    }

    #[test]
    fn mov_reg() {
        let mut asm = Assembler::new();
        asm.mov(Reg::GPR0, Reg::GPR0);
        let code = asm.consume();
        asm_assert_eq!(&[0xaa0803e8], &code);
    }

    #[test]
    fn load_local() {
        let mut asm = Assembler::new();
        asm.load_local(Reg::GPR0, 0);
        asm.load_local(Reg::GPR1, 1);
        asm.load_local(Reg::GPR2, 2);
        let code = asm.consume();
        asm_assert_eq!(&[0xf9400008, 0xf9400409, 0xf940080a], &code);
    }

    #[test]
    fn store_local() {
        let mut asm = Assembler::new();
        asm.store_local(0, Reg::GPR0);
        asm.store_local(1, Reg::GPR0);
        asm.store_local(2, Reg::GPR0);
        let code = asm.consume();
        asm_assert_eq!(&[0xf9000008, 0xf9000408, 0xf9000808], &code);
    }

    #[test]
    fn store_return() {
        let mut asm = Assembler::new();
        asm.store(0, Reg::OutBase, Reg::GPR0);
        asm.store(1, Reg::OutBase, Reg::GPR0);
        asm.store(2, Reg::OutBase, Reg::GPR0);
        let code = asm.consume();
        asm_assert_eq!(&[0xf9000028, 0xf9000428, 0xf9000828], &code);
    }

    #[test]
    fn add() {
        let mut asm = Assembler::new();
        // Immediate < 4095
        asm.add(Reg::GPR0, Reg::GPR0, 1);
        let code = asm.consume();
        asm_assert_eq!(&[0x91000508], &code);
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
        asm_assert_eq!(&[0xf94003f8, 0xf94007f9, 0x8b190308], &code);
    }

    #[test]
    fn tiny_mov() {
        let mut asm = Assembler::new();
        asm.mov(Reg::GPR0, 0x1);
        let code = asm.consume();
        // Make sure `asm.mov()` doesn't emit a superfluous `movk` instructions
        asm_assert_eq!(&[0xd2800028], &code);
    }

    #[test]
    fn big_add() {
        let mut asm = Assembler::new();
        // Immediate > 4095
        asm.add(Reg::GPR0, Reg::GPR0, u64::MAX);
        let code = asm.consume();
        // Should mov into a temporary register, then add
        asm_assert_eq!(
            &[0xd29ffff8, 0xf2bffff8, 0xf2dffff8, 0xf2fffff8, 0x8b180108],
            &code
        );
    }

    #[test]
    fn small_sub() {
        let mut asm = Assembler::new();
        // Immediate < 4095
        asm.sub(Reg::GPR0, Reg::GPR0, 1);
        let code = asm.consume();
        asm_assert_eq!(&[0xd1000508], &code);
    }

    #[test]
    fn big_sub() {
        let mut asm = Assembler::new();
        // Immediate > 4095
        asm.sub(Reg::GPR0, Reg::GPR0, u64::MAX);
        let code = asm.consume();
        // Should mov into a temporary register, then sub
        asm_assert_eq!(
            &[0xd29ffff8, 0xf2bffff8, 0xf2dffff8, 0xf2fffff8, 0xcb180108],
            &code
        );
    }

    #[test]
    fn eq() {
        let mut asm = Assembler::new();
        // register
        asm.eq(Reg::GPR0, Reg::GPR0, Reg::GPR1);
        // immediate
        asm.eq(Reg::GPR0, Reg::GPR1, 0x4);
        // immediate > 4095
        asm.eq(Reg::GPR0, Reg::GPR1, 4096);
        let code = asm.consume();
        asm_assert_eq!(
            &[0xeb09011f, 0x9a9f17e8, 0xf100113f, 0x9a9f17e8, 0xd2820018, 0xeb18013f, 0x9a9f17e8],
            &code
        );
    }

    #[test]
    fn ne() {
        let mut asm = Assembler::new();
        // register
        asm.ne(Reg::GPR0, Reg::GPR0, Reg::GPR1);
        // immediate
        asm.ne(Reg::GPR0, Reg::GPR1, 0x4);
        // immediate > 4095
        asm.ne(Reg::GPR0, Reg::GPR1, 4096);
        let code = asm.consume();
        asm_assert_eq!(
            &[0xeb09011f, 0x9a9f07e8, 0xf100113f, 0x9a9f07e8, 0xd2820018, 0xeb18013f, 0x9a9f07e8],
            &code
        );
    }

    #[test]
    fn branch_if() {
        let mut asm = Assembler::new();
        asm.branch_if(0x30, ConditionCode::Eq);
        let code = asm.consume();
        asm_assert_eq!(&[0x54000180], &code);
    }

    #[test]
    fn backwards_branch() {
        let mut asm = Assembler::new();
        asm.nop();
        asm.nop();
        asm.nop();
        asm.nop();
        asm.branch(0);
        let code = asm.consume();
        asm_assert_eq!(
            &[0xd503201f, 0xd503201f, 0xd503201f, 0xd503201f, 0x17fffffc],
            &code
        );
    }

    #[test]
    fn csel() {
        let mut asm = Assembler::new();
        asm.csel(Reg::GPR0, Reg::GPR1, Reg::GPR2, ConditionCode::Eq);
        let code = asm.consume();
        asm_assert_eq!(&[0x9a8a0128], &code);
    }
}
