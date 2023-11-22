use std::fmt;

/// Wrapper around a vector of u32s that prints as hex.
#[derive(PartialEq)]
pub struct Hex<'a>(pub &'a [u8]);

impl fmt::Display for Hex<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for b in to_32(self.0) {
            write!(f, "{:x}", b)?;
        }
        Ok(())
    }
}

pub fn to_32(bytes: &[u8]) -> &[u32] {
    unsafe {
        std::slice::from_raw_parts(
            bytes.as_ptr().cast::<u32>(),
            bytes.len() / std::mem::size_of::<u32>(),
        )
    }
}

pub fn to_8(bytes: &[u32]) -> &[u8] {
    unsafe { std::slice::from_raw_parts(bytes.as_ptr().cast::<u8>(), std::mem::size_of_val(bytes)) }
}

/// Asserts that the two slices of u8s are equal, but prints them as assembly instructions.
macro_rules! asm_assert_eq {
    ($left:expr, $right:expr) => {
        use $crate::vm::jit::debug::*;

        let left = to_8($left);
        if left != $right {
            use ::capstone::prelude::*;
            use ::pretty_assertions::StrComparison;

            let capstone = Capstone::new()
                .arm64()
                .mode(capstone::arch::arm64::ArchMode::Arm)
                .build()
                .unwrap();
            let got_instrs = capstone.disasm_all($right, 0x1000).unwrap();
            let expect_instrs = capstone.disasm_all(left, 0x1000).unwrap();
            let got_pretty = got_instrs.iter().fold(String::new(), |acc, i| {
                format!(
                    "{acc} 0x{:x}  {}    {} {}\n",
                    i.address(),
                    Hex(i.bytes()),
                    i.mnemonic().unwrap(),
                    i.op_str().unwrap(),
                )
            });
            let expect_pretty = expect_instrs.iter().fold(String::new(), |acc, i| {
                format!(
                    "{acc} 0x{:x}  {}    {} {}\n",
                    i.address(),
                    Hex(i.bytes()),
                    i.mnemonic().unwrap(),
                    i.op_str().unwrap(),
                )
            });
            panic!(
                "assertion failed:\n{}",
                StrComparison::new(&got_pretty, &expect_pretty)
            );
        }
    };
}

pub(crate) use asm_assert_eq;
