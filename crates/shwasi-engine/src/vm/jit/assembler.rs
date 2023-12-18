// TODO: make LoadTemp4. Prevent LoadTemp conflicts.
// Also move them to temporary registers.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Reg {
    GPR0 = 8,
    GPR1 = 9,
    GPR2 = 10,
    /// A temporary register used for moving memory around.
    LoadTemp = 11,
    /// A temporary register used for moving memory around.
    LoadTemp2 = 12,
    /// A temporary register used for moving memory around.
    LoadTemp3 = 13,

    Fp = 29,
    Lr = 30,
    Sp = 31,

    /// The register that holds the base address of the locals array. This should be the first
    /// argument to the JIT-compiled memory.
    Arg0 = 0,
    /// The register that holds the base address of the output array. This should be the second
    /// argument to the JIT-compiled memory.
    Arg1 = 1,
    Arg2 = 2,
    Arg3 = 3,
    Arg4 = 4,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operand {
    Reg(Reg),
    Imm64(u64),
    Mem64(Reg, u64),
    Unreachable,
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
                if ((-(1 << 16) + 1)..0).contains(&(imm64 as i64)) {
                    // Mov and negate instruction (movn)
                    self.emit_u32(
                        0x92800000 | (((imm64 as i32).abs() - 1) as u32) << 5 | reg as u32,
                    );
                    return;
                }

                let [low, mid_low, mid_high, high] = split_u64::<16, 4>(imm64);
                // mov (immediate)
                self.emit_u32(0xd2800000 | ((low as u32) << 5) | reg as u32);
                // The following are movk's that shift incrementally. We don't want to compile them
                // if the immediate's bits are representable by just the least significant 16.
                if mid_low != 0 {
                    self.emit_u32(0xf2000000 | 0b101 << 21 | ((mid_low as u32) << 5) | reg as u32);
                }
                if mid_high != 0 {
                    self.emit_u32(0xf2000000 | 0b110 << 21 | (mid_high as u32) << 5 | reg as u32);
                }
                if high != 0 {
                    self.emit_u32(0xf2000000 | 0b111 << 21 | (high as u32) << 5 | reg as u32);
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
            (Operand::Mem64(base, offset), Operand::Reg(reg)) => {
                self.store(offset as u32, base, reg);
            }
            (Operand::Reg(reg), Operand::Mem64(base, offset)) => {
                self.load(reg, offset as u32, base);
            }
            (dst, src) => panic!("unsupported mov operands: {:?} and {:?}", dst, src),
        }
    }

    pub fn nop(&mut self) {
        self.emit_u32(0xd503201f);
    }

    pub fn ret(&mut self) {
        self.emit_u32(0xd65f03c0);
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

    pub fn branch_link_register(&mut self, reg: Reg) {
        self.emit_u32(0xd63f0000 | (reg as u32) << 5);
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

    pub fn add(
        &mut self,
        dst: impl Into<Operand>,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
    ) {
        self.add_sub_op(dst, lhs, rhs, 0x8b000000, 0x91000000);
    }

    pub fn sub(
        &mut self,
        dst: impl Into<Operand>,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
    ) {
        self.add_sub_op(dst, lhs, rhs, 0xcb000000, 0xd1000000);
    }

    pub fn stp(&mut self, r1: Reg, r2: Reg, base: Reg, offset: u32) {
        // TOOD: make sure offset is within i7 bounds
        self.emit_u32(
            0xa9000000 | (offset) << 15 | (r2 as u32) << 10 | (base as u32) << 5 | r1 as u32,
        );
    }

    pub fn ldp(&mut self, r1: Reg, r2: Reg, base: Reg, offset: u32) {
        // TOOD: make sure offset is within i7 bounds
        self.emit_u32(
            0xa9400000 | (offset) << 15 | (r2 as u32) << 10 | (base as u32) << 5 | r1 as u32,
        );
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

    pub fn and(
        &mut self,
        dst: impl Into<Operand>,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
    ) {
        self.logical_op(dst, lhs, rhs, 0x8a000000, 0x92400000);
    }

    pub fn eor(
        &mut self,
        dst: impl Into<Operand>,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
    ) {
        self.logical_op(dst, lhs, rhs, 0xca000000, 0xd2400000);
    }

    pub fn or(
        &mut self,
        dst: impl Into<Operand>,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
    ) {
        self.logical_op(dst, lhs, rhs, 0xaa000000, 0xb2400000);
    }

    pub fn mul(
        &mut self,
        dst: impl Into<Operand>,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
    ) {
        let dst = self.resolve_dst(dst);
        let (lhs, rhs) = self.resolve(lhs, rhs);

        match (lhs, rhs) {
            (Operand::Reg(lhs), Operand::Reg(rhs)) => {
                self.emit_u32(0x9b007c00 | (rhs as u32) << 16 | (lhs as u32) << 5 | (dst as u32));
            }
            (Operand::Reg(lhs), Operand::Imm64(imm64)) => {
                self.mov(Reg::LoadTemp, imm64);
                self.mul(dst, lhs, Reg::LoadTemp);
            }
            (Operand::Imm64(lhs), Operand::Imm64(rhs)) => {
                self.mov(Reg::LoadTemp, lhs);
                self.mul(dst, Reg::LoadTemp, rhs);
            }
            (op1, op2) => {
                self.mul(dst, op2, op1);
            }
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
            Operand::Unreachable => unreachable!("cannot store an unreachable value"),
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
            Operand::Unreachable => unreachable!("cannot store an unreachable value"),
        }
    }

    fn add_sub_op(
        &mut self,
        dst: impl Into<Operand>,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
        reg_base: u32,
        imm_base: u32,
    ) {
        let dst = self.resolve_dst(dst);
        let (lhs, rhs) = self.resolve(lhs, rhs);
        match (lhs, rhs) {
            (Operand::Reg(lhs), Operand::Reg(rhs)) => {
                self.emit_u32(reg_base | (rhs as u32) << 16 | (lhs as u32) << 5 | (dst as u32));
            }
            (Operand::Reg(lhs), Operand::Imm64(rhs)) => {
                // The add/sub immediate instruction only supports 12 bits
                if rhs > (2 << 12) - 1 {
                    self.mov(Reg::LoadTemp, rhs);
                    self.add_sub_op(dst, lhs, Reg::LoadTemp, reg_base, imm_base);
                } else {
                    self.emit_u32(imm_base | (rhs as u32) << 10 | (lhs as u32) << 5 | (dst as u32));
                }
            }
            (Operand::Imm64(lhs), Operand::Reg(rhs)) => {
                self.mov(Reg::LoadTemp, lhs);
                self.add_sub_op(dst, Reg::LoadTemp, rhs, reg_base, imm_base);
            }
            (Operand::Imm64(lhs), Operand::Imm64(rhs)) => {
                self.mov(Reg::LoadTemp, lhs);
                self.add_sub_op(dst, Reg::LoadTemp, rhs, reg_base, imm_base);
            }
            _ => unreachable!(),
        }
        self.restore_dst();
    }

    fn logical_op(
        &mut self,
        dst: impl Into<Operand>,
        lhs: impl Into<Operand>,
        rhs: impl Into<Operand>,
        reg_base: u32,
        imm_base: u32,
    ) {
        let dst = self.resolve_dst(dst);
        let (lhs, rhs) = self.resolve(lhs, rhs);
        match (lhs, rhs) {
            (Operand::Reg(lhs), Operand::Reg(rhs)) => {
                self.emit_u32(reg_base | (rhs as u32) << 16 | (lhs as u32) << 5 | (dst as u32));
            }
            (Operand::Reg(lhs), Operand::Imm64(imm64)) => {
                if imm64 > (2 << 13) - 1 {
                    self.mov(Reg::LoadTemp, imm64);
                    self.logical_op(dst, lhs, Reg::LoadTemp, reg_base, imm_base);
                } else if let Some((n, immr, imms)) = encode_logical_immediate(imm64, 64) {
                    self.emit_u32(
                        imm_base
                            | n << 22
                            | immr << 16
                            | imms << 10
                            | (lhs as u32) << 5
                            | (dst as u32),
                    );
                } else {
                    self.mov(Reg::LoadTemp, imm64);
                    self.logical_op(dst, lhs, Reg::LoadTemp, reg_base, imm_base);
                }
            }
            (Operand::Imm64(lhs), Operand::Imm64(rhs)) => {
                self.mov(Reg::LoadTemp, lhs);
                self.logical_op(dst, Reg::LoadTemp, rhs, reg_base, imm_base);
            }
            (op1, op2) => {
                self.logical_op(dst, op2, op1, reg_base, imm_base);
            }
        }

        self.restore_dst();
    }

    fn emit(&mut self, b: u8) {
        self.out.push(b);
    }

    fn emit_u32(&mut self, u: u32) {
        self.out.reserve(4);
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
            Operand::Unreachable => unreachable!("cannot store an unreachable value"),
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

/// Encode a logical immediate value.
///
/// Returns a tuple of (N, immr, imms) where N is the encoded value of the N field, immr is the
/// encoded value of the immr field, and imms is the encoded value of the imms field.
///
/// Taken and modified from LLVM's AArch64 backend, at
/// https://llvm.org/doxygen/AArch64AddressingModes_8h_source.html
fn encode_logical_immediate(imm: u64, reg_size: u32) -> Option<(u32, u32, u32)> {
    if imm == 0
        || imm == !0
        || (reg_size != 64 && (imm >> reg_size != 0 || imm == (!0 >> (64 - reg_size))))
    {
        return None;
    }

    // Determine the element size.
    let mut size = reg_size;

    while size > 2 {
        size /= 2;
        let mask = (1u64 << size) - 1;

        if (imm & mask) != ((imm >> size) & mask) {
            size *= 2;
            break;
        }
    }

    // Determine the rotation to make the element be: 0^m 1^n.
    let cto;
    let i;
    let mask = u64::MAX >> (64 - size);
    let mut imm = imm & mask;

    if is_shifted_mask_64(imm) {
        i = imm.trailing_zeros();
        assert!(i < 64, "undefined behavior");
        cto = (imm >> i).count_ones();
    } else {
        imm |= !mask;
        if !is_shifted_mask_64(!imm) {
            return None;
        }

        let clo = imm.leading_ones();
        i = 64 - clo;
        cto = clo + imm.count_ones() - (64 - size);
    }

    // Encode in immr the number of RORs it would take to get *from* 0^m 1^n
    // to our target value, where I is the number of RORs to go the opposite
    // direction.
    assert!(size > i, "I should be smaller than element size");
    let immr = (size - i) & (size - 1);

    // If size has a 1 in the n'th bit, create a value that has zeroes in
    // bits [0, n] and ones above that.
    let mut n_imms = !(size - 1) << 1;

    // Or the cto value into the low bits, which must be below the nth bit
    // bit mentioned above.
    n_imms |= cto - 1;

    // Extract the seventh bit and toggle it to create the N field.
    let n = (((n_imms >> 6) & 1) ^ 1) << 12;

    Some((n, immr, n_imms & 0x3f))
}

fn is_shifted_mask_64(val: u64) -> bool {
    val & (val + 1) == 0
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

impl Operand {
    pub fn is_unreachable(&self) -> bool {
        matches!(self, Self::Unreachable)
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
    fn store_return() {
        let mut asm = Assembler::new();
        asm.store(0, Reg::Arg1, Reg::GPR0);
        asm.store(1, Reg::Arg1, Reg::GPR0);
        asm.store(2, Reg::Arg1, Reg::GPR0);
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
        asm_assert_eq!(&[0xf94003eb, 0xf94007ec, 0x8b0c0168], &code);
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
        asm_assert_eq!(&[0x9280000b, 0x8b0b0108], &code);
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
        asm_assert_eq!(&[0x9280000b, 0xcb0b0108], &code);
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
            &[0xeb09011f, 0x9a9f17e8, 0xf100113f, 0x9a9f17e8, 0xd282000b, 0xeb0b013f, 0x9a9f17e8],
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
            &[0xeb09011f, 0x9a9f07e8, 0xf100113f, 0x9a9f07e8, 0xd282000b, 0xeb0b013f, 0x9a9f07e8],
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

    #[test]
    fn and() {
        let mut asm = Assembler::new();
        asm.and(Reg::GPR0, Reg::GPR1, 1);
        asm.and(Reg::GPR0, Reg::GPR1, Reg::GPR2);
        let code = asm.consume();
        asm_assert_eq!(&[0x92400128, 0x8a0a0128], &code);
    }

    #[test]
    fn or() {
        let mut asm = Assembler::new();
        asm.or(Reg::GPR0, Reg::GPR1, 0b11);
        asm.or(Reg::GPR0, Reg::GPR1, Reg::GPR2);
        let code = asm.consume();
        asm_assert_eq!(&[0xb2400528, 0xaa0a0128], &code);
    }

    #[test]
    fn eor() {
        let mut asm = Assembler::new();
        asm.eor(Reg::GPR0, Reg::GPR1, 0b11);
        asm.eor(Reg::GPR0, Reg::GPR1, Reg::GPR2);
        let code = asm.consume();
        asm_assert_eq!(&[0xd2400528, 0xca0a0128], &code);
    }

    #[test]
    fn mul() {
        let mut asm = Assembler::new();
        asm.mul(Reg::GPR0, Reg::GPR1, Reg::GPR2);
        let code = asm.consume();
        asm_assert_eq!(&[0x9b0a7d28], &code);
    }

    #[test]
    fn mov_negative() {
        let mut asm = Assembler::new();
        asm.mov(Reg::GPR0, -14i64 as u64);
        asm.mov(Reg::GPR0, -55i64 as u64);
        asm.mov(Reg::GPR0, -1024i64 as u64);
        let code = asm.consume();
        asm_assert_eq!(&[0x928001a8, 0x928006c8, 0x92807fe8], &code);
    }
}
