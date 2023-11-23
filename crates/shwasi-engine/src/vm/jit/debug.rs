// We don't care if this is dead code, we use parts of this library ad-hoc.
#![allow(dead_code)]

use std::fmt;

use capstone::prelude::*;

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

pub fn asm_fmt(bytes: &[u8]) -> String {
    let capstone = Capstone::new()
        .arm64()
        .mode(capstone::arch::arm64::ArchMode::Arm)
        .build()
        .unwrap();
    let instrs = capstone.disasm_all(bytes, 0x1000).unwrap();
    instrs.iter().fold(String::new(), |acc, i| {
        format!(
            "{acc} 0x{:x}  {}    {} {}\n",
            i.address(),
            Hex(i.bytes()),
            i.mnemonic().unwrap(),
            i.op_str().unwrap(),
        )
    })
}

/// Asserts that the two slices of u8s are equal, but prints them as assembly instructions.
macro_rules! asm_assert_eq {
    ($left:expr, $right:expr) => {
        use $crate::vm::jit::debug::*;

        let left = to_8($left);
        if left != $right {
            use ::pretty_assertions::StrComparison;

            let got_pretty = asm_fmt($right);
            let expect_pretty = asm_fmt(left);
            panic!(
                "assertion failed:\n{}",
                StrComparison::new(&got_pretty, &expect_pretty)
            );
        }
    };
}

pub(crate) use asm_assert_eq;
