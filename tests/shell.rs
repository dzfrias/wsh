macro_rules! shell_test {
    ($name:ident, $input:expr, $expect:expr) => {
        #[test]
        fn $name() {
            use ::assert_cmd::Command;
            use ::std::{
                fs::{self, File},
                io::Write,
            };

            const PATH: &str = concat!(stringify!($name), ".tmp");
            let mut f = File::create(PATH).unwrap();
            f.write_all($input.as_bytes()).unwrap();
            let mut cmd = Command::cargo_bin(env!("CARGO_PKG_NAME")).unwrap();
            cmd.arg(PATH)
                .assert()
                .success()
                .stdout(concat!($expect, "\n"));

            fs::remove_file(PATH).unwrap();
        }
    };
    (@fail $name:ident, $input:expr) => {
        #[test]
        fn $name() {
            use ::assert_cmd::Command;
            use ::std::{
                fs::{self, File},
                io::Write,
            };

            const PATH: &str = concat!(stringify!($name), ".tmp");
            let mut f = File::create(PATH).unwrap();
            f.write_all($input.as_bytes()).unwrap();
            let mut cmd = Command::cargo_bin(env!("CARGO_PKG_NAME")).unwrap();
            let cmd = cmd.arg(PATH).assert().failure();
            fs::remove_file(PATH).unwrap();
            let out = cmd.get_output();
            let stderr = String::from_utf8_lossy(&out.stderr);
            insta::assert_snapshot!(stringify!($name), stderr);
        }
    };
}

shell_test!(basic_command_execution, "echo hi", "hi");
shell_test!(piping, "echo \"hello world\" | wc -w | xargs", "2");
shell_test!(nested_commands, "echo `echo .(1 / 1)`", "1");
shell_test!(last_status, "false\necho .?\ntrue\necho .?", "1\n0");
shell_test!(last_status_in_piping, "false | echo .?", "0");
shell_test!(aliases, "alias foo = echo hi\nfoo", "hi");

shell_test!(@fail unclosed_paren, "echo .(1 + 1");
shell_test!(@fail unfinished_pipe, "echo hi |");
shell_test!(@fail unclosed_backtick, "echo `echo hi");
shell_test!(@fail unfinished_infix, "echo .(1 +)");
shell_test!(@fail unfinished_prefix, "echo .(!)");
