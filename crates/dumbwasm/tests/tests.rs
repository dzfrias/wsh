use dumbwasm::parse;
use std::fmt;

/// A struct who's Debug implementation prints the inner byte buffer in hexadecimal.
#[derive(PartialEq)]
struct PrettyBytes<'a> {
    buf: &'a [u8],
}

impl fmt::Debug for PrettyBytes<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // format over multiple lines if there are more than 5 items
        let new_lines = self.buf.len() > 5;
        if new_lines {
            writeln!(f, "[")?;
        } else {
            write!(f, "[")?;
        }
        for (i, byte) in self.buf.iter().enumerate() {
            if new_lines {
                writeln!(f, "  {byte:#x},")?;
                continue;
            }
            if i == self.buf.len() - 1 {
                write!(f, "{byte:#x}")?;
                continue;
            }
            write!(f, "{byte:#x}, ")?;
        }
        write!(f, "]")?;
        Ok(())
    }
}

impl<'a> From<&'a [u8]> for PrettyBytes<'a> {
    fn from(buf: &'a [u8]) -> PrettyBytes<'a> {
        Self { buf }
    }
}

macro_rules! bytes {
    ($($byte:expr),*) => {
        {
            let bytes = PrettyBytes { buf: &[$($byte),*] };
            bytes
        }
    };
}

macro_rules! dumbwasm_test {
    ($name:ident, $input:expr, [$($byte:expr),*]) => {
        #[test]
        fn $name() {
            let buf = parse($input).unwrap();
            ::pretty_assertions::assert_eq!(bytes!($($byte),*), PrettyBytes::from(buf.as_slice()))
        }
    };
}

macro_rules! dumbwasm_test_failing {
    ($name:ident, $input:expr) => {
        #[test]
        fn $name() {
            use assert_cmd::Command;
            use std::{fs::File, io::Write};
            use tempdir::TempDir;

            let dir = TempDir::new(stringify!($name)).expect("could not create temp dir");
            let mut f = File::create(dir.path().join("input.dumbwasm"))
                .expect("could not create temp file");
            #[allow(clippy::string_lit_as_bytes)]
            f.write_all($input.as_bytes())
                .expect("could not write to temp file");
            let mut cmd = Command::cargo_bin(env!("CARGO_PKG_NAME")).unwrap();
            let assert = cmd.current_dir(dir.path()).arg("input.dumbwasm").assert();
            let out = assert.get_output();
            assert!(!out.status.success(), "command exited with exit code 0!");
            ::insta::assert_snapshot!(String::from_utf8_lossy(&out.stderr));
            dir.close().expect("could not close temp dir");
        }
    };
}

dumbwasm_test!(magic, "magic", [0x00, 0x61, 0x73, 0x6d]);
dumbwasm_test!(version, "version", [0x01, 0x00, 0x00, 0x00]);

dumbwasm_test!(
    aliases,
    "i32 i64 f32 f64 funcref externref extern.func extern.table extern.mem form.func true false",
    [0x7f, 0x7e, 0x7d, 0x7c, 0x70, 0x6f, 0x00, 0x01, 0x02, 0x60, 0x01, 0x00]
);
dumbwasm_test!(
    numbers,
    "4294967295 10.1",
    [0xff, 0xff, 0xff, 0xff, 0xf, 0x9a, 0x99, 0x21, 0x41]
);

dumbwasm_test!(
    comments,
    ";; comment
    (annotation)",
    []
);

dumbwasm_test!(
    type_indicators,
    "10<u8> 18446744073709551615<u64> 20.3<f64>",
    [
        0x0a, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x1, 0xcd, 0xcc, 0xcc, 0xcc,
        0xcc, 0x4c, 0x34, 0x40
    ]
);
dumbwasm_test_failing!(bad_instruction, "i32.cons 10");

dumbwasm_test!(instructions, "i32.const 10 unreachable", [0x41, 0x0a, 0x00]);
dumbwasm_test!(
    instructions_with_prefix,
    "memory.init 0 0x00<u8>",
    [0xfc, 0x08, 0x00, 0x00]
);

dumbwasm_test_failing!(unknown_char, "i32 i32 ]");
dumbwasm_test_failing!(unclosed_array, "i32 [0, 1");
dumbwasm_test_failing!(unclosed_section, "function {  ");
dumbwasm_test_failing!(unclosed_instrs, "instrs {  ");
dumbwasm_test_failing!(out_of_place_type_indicator_float, "23<f32>");
dumbwasm_test_failing!(out_of_place_type_indicator_int, "10.1<u8>");

dumbwasm_test!(vectors, "[0, 1, 2, 3]", [0x04, 0x00, 0x01, 0x02, 0x03]);
dumbwasm_test!(
    type_section,
    "type { form.func [i32] [i32] }",
    [0x01, 0x05, 0x60, 0x01, 0x7f, 0x01, 0x7f]
);
dumbwasm_test!(
    instrs,
    "instrs { i32.const 10 i32.const 10 i32.add return }",
    [0x06, 0x41, 0x0a, 0x41, 0x0a, 0x6a, 0x0f]
);
dumbwasm_test!(
    strings,
    "\"Hello, world!\"",
    [0x0d, 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x2c, 0x20, 0x77, 0x6f, 0x72, 0x6c, 0x64, 0x21]
);

dumbwasm_test_failing!(suggests_general_keywords, "inst");

dumbwasm_test!(call_indirect, "call_indirect", [0x11]);
