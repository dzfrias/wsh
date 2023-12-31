macro_rules! shell_test {
    ($name:ident, $input:expr, $expect:expr) => {
        #[test]
        fn $name() {
            use ::assert_cmd::Command;
            use ::std::{
                env,
                fs::{self, File},
                io::Write,
            };

            // Auxiliary environment variable for test usage.
            env::set_var("SHWASI_ENV", "test");
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
                env,
                fs::{self, File},
                io::Write,
            };

            env::set_var("SHWASI_ENV", "test");
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
shell_test!(assignments, ".x = 10 + 10\necho .x", "20");
shell_test!(
    alias_and_assignments,
    "alias foo = echo hi\n.x = `foo`\necho .x",
    "hi"
);
shell_test!(
    file_redirects,
    "echo hi > .(\"file\" + \".txt\")\n.x = `cat file.txt`\nrm file.txt\necho .x",
    "hi"
);
// Disabling this test on Windows because I'm not sure what to do. In the CI, the test fails as a
// result of:
//   echo: write error: Bad file descriptor
// I found a good issue (https://github.com/mvdan/sh/issues/289) that has a pretty long-standing
// investigation of this behavior. I do not have a Windows machine to thoroughly debug this on, so
// I'm going to leave the append redirect test disabled for now.
//
// In the issue, it was suggested that bash emulates /dev/fd on Windows, which is interesting.
// However, work at such a low-level is not feasible, as I don't have Windows. Working entirely
// through GitHub Actions would be impractical for this task.
#[cfg(not(windows))]
shell_test!(
    append_file_redirects,
    "echo hi >> .(\"t\" + \".txt\")\necho hi >> t.txt\n.x = `cat t.txt`\nrm t.txt\necho .x",
    "hi\nhi"
);
shell_test!(alias_with_args, "alias foo = echo\nfoo hi", "hi");
shell_test!(
    alias_with_args_and_piping,
    "alias foo = echo\nfoo hi | wc -c | xargs",
    "3"
);
// TODO: figure out how to test setting environment variables for processes spawned by the shell
shell_test!(get_environment_variables, "echo $SHWASI_ENV", "test");
// Could fail if the user has $FOO_NO_ASSIGN set in their environment.
shell_test!(environment_variables_default, "echo $FOO_NO_ASSIGN", "");
shell_test!(
    environment_variables_export,
    "export FOO = bar\necho $FOO",
    "bar"
);

shell_test!(@fail unclosed_paren, "echo .(1 + 1");
shell_test!(@fail unfinished_pipe, "echo hi |");
shell_test!(@fail unclosed_backtick, "echo `echo hi");
shell_test!(@fail unfinished_infix, "echo .(1 +)");
shell_test!(@fail unfinished_prefix, "echo .(!)");
shell_test!(@fail bad_redirect_position, "echo hi > file.txt | cat");
// Should fail as a result of `__should_not_be_defined` not being a valid command. Note that this
// CAN possibly fail if the user has a command named `__should_not_be_defined` in their PATH.
shell_test!(@fail recursive_alias, "alias __should_not_be_defined = __should_not_be_defined\n__should_not_be_defined");
shell_test!(@fail accurate_errors_with_bloated_floats, "echo .(1.0000000000000)\necho .(1");
