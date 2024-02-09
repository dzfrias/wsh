macro_rules! shell_test {
    ($name:ident, $input:expr, $expect:expr, $expect_stderr:expr) => {
        #[test]
        fn $name() {
            use assert_cmd::Command;
            use std::{env, fs::File, io::Write};
            use tempdir::TempDir;

            // Auxiliary environment variable for test usage.
            env::set_var("WSH_ENV", "test");
            env::set_var("WASM_PATH", env::current_dir().unwrap().join("tests/wasm"));
            let tmp_dir =
                TempDir::new(stringify!($name)).expect("error creating temporary directory");
            let file_path = tmp_dir.path().join(concat!(stringify!($name), ".tmp"));
            let mut f = File::create(&file_path).unwrap();
            f.write_all($input.as_bytes()).unwrap();
            let mut cmd = Command::cargo_bin(env!("CARGO_PKG_NAME")).unwrap();
            cmd.arg(&file_path)
                .current_dir(tmp_dir.path())
                .assert()
                .success()
                .stdout($expect)
                .stderr($expect_stderr);

            drop(f);
            tmp_dir.close().expect("error closing temporary directory");
        }
    };
    ($name:ident, $input:expr, $expect:expr) => {
        shell_test!($name, $input, format!("{}\n", $expect), "");
    };
    ($name:ident, $input:expr, @stderr $expect:expr) => {
        shell_test!($name, $input, "", format!("{}\n", $expect));
    };
    ($name:ident, $input:expr, @stdout $expect:expr, @stderr $expect_stderr:expr) => {
        shell_test!(
            $name,
            $input,
            format!("{}\n", $expect),
            format!("{}\n", $expect_stderr)
        );
    };
    (@fail $name:ident, $input:expr) => {
        #[test]
        fn $name() {
            use assert_cmd::Command;
            use std::{env, fs::File, io::Write};
            use tempdir::TempDir;

            env::set_var("WSH_ENV", "test");
            env::set_var("WASM_PATH", env::current_dir().unwrap().join("tests/wasm"));

            let tmp_dir =
                TempDir::new(stringify!($name)).expect("error creating temporary directory");
            let file_path = tmp_dir.path().join(concat!(stringify!($name), ".tmp"));
            let mut f = File::create(&file_path).unwrap();
            f.write_all($input.as_bytes()).unwrap();
            let mut cmd = Command::cargo_bin(env!("CARGO_PKG_NAME")).unwrap();
            let cmd = cmd.current_dir(tmp_dir.path()).arg(&file_path).assert();
            let out = cmd.get_output();
            let stderr = String::from_utf8_lossy(&out.stderr);
            insta::assert_snapshot!(stringify!($name), stderr);

            drop(f);
            tmp_dir.close().expect("error closing temporary directory");
        }
    };
}

shell_test!(basic_command_execution, "echo hi", "hi");
shell_test!(piping, "echo \"hello world\" | wc -w | xargs", "2");
shell_test!(nested_commands, "echo `echo .(1 / 1)`", "1");
shell_test!(last_status, "false\necho .?\ntrue\necho .?", "1\n0");
shell_test!(last_status_in_piping, "false | echo .?", "0");
shell_test!(
    tilde_expansion,
    "echo ~/.vimrc",
    format!("{}/.vimrc", dirs::home_dir().unwrap().display())
);
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
shell_test!(
    merge_file_redirects,
    "echo .(!1) %> merge_file.txt\n.x = `cat merge_file.txt`\nrm merge_file.txt\necho .x",
    "wsh: type error: `!` `number`"
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
#[cfg(not(windows))]
shell_test!(
    merge_append_file_redirects,
    "echo .(!1) %>> merge_t.txt\necho .(!1) %>> merge_t.txt\n.x = `cat merge_t.txt`\nrm merge_t.txt\necho .x",
    "wsh: type error: `!` `number`\nwsh: type error: `!` `number`"
);
shell_test!(alias_with_args, "alias foo = echo\nfoo hi", "hi");
shell_test!(
    alias_with_args_and_piping,
    "alias foo = echo\nfoo hi | wc -c | xargs",
    "3"
);
shell_test!(get_environment_variables, "echo $WSH_ENV", "test");
// Could fail if the user has $FOO_NO_ASSIGN set in their environment.
shell_test!(environment_variables_default, "echo $FOO_NO_ASSIGN", "");
shell_test!(
    environment_variables_export,
    "export FOO = bar\necho $FOO",
    "bar"
);
shell_test!(builtins_have_stdout, "cd __BAD_DIR %| wc -l | xargs", "1");
shell_test!(
    source,
    "echo \"echo foo\" > __t_tmp.wsi\nsource __t_tmp.wsi\nrm __t_tmp.wsi",
    "foo"
);
shell_test!(
    no_fail_fast_on_errs,
    "__UNDEFINED\necho .?",
    @stdout "127",
    @stderr "wsh: command not found: __UNDEFINED"
);
// Should fail as a result of `__should_not_be_defined` not being a valid command. Note that this
// CAN possibly fail if the user has a command named `__should_not_be_defined` in their PATH.
shell_test!(
    recursive_alias,
    "alias __should_not_be_defined = __should_not_be_defined\n__should_not_be_defined",
    @stderr "wsh: command not found: __should_not_be_defined"
);
shell_test!(
    shell_errors_are_piped_properly,
    "echo .(!1) %| wc -l | xargs",
    "1"
);
shell_test!(simple_functions, "def f do echo hi end\nf", "hi");
shell_test!(
    function_with_args,
    "def f : x y do echo .x .y end\nf hello world",
    "hello world"
);
shell_test!(
    function_with_named_args,
    "def f : x color|c=always force|f do echo .x .color .force end\nf hello -c never -f\nf goodbye\nf greetings -f",
    "hello never true\ngoodbye always false\ngreetings always true"
);
shell_test!(
    return_from_func,
    "def f do\nreturn\necho hi\nend\nf | wc -l | xargs",
    "0"
);
shell_test!(
    functions_do_not_leak_scope,
    "def f do .x = 1 end\nf\necho .x",
    @stderr "wsh: unbound: `x`"
);
shell_test!(global_vars, "def f do .hello := 1 end\nf\necho .hello", "1");

shell_test!(wasm, "load .($WASM_PATH + \"/fib.wasm\")\nfib 10", "55");
shell_test!(
    wasm_unload,
    "load .($WASM_PATH + \"/fib.wasm\")\nunload\nfib 10",
    @stdout "unloaded 1 modules",
    @stderr "wsh: command not found: fib"
);
shell_test!(
    wasm_piping,
    "load .($WASM_PATH + \"/fib.wasm\")\nfib 10 | wc -l | xargs",
    "1"
);
shell_test!(
    wasm_bad_args,
    "load .($WASM_PATH + \"/fib.wasm\")\nfib hello",
    @stderr "wsh: cannot pass string to wasm function `fib`"
);
shell_test!(
    source_wasi,
    "source .($WASM_PATH + \"/hello_wasi.wasm\")",
    "Hello, world!"
);
shell_test!(
    source_wasi_piping,
    "source .($WASM_PATH + \"/hello_wasi.wasm\") | wc -w | xargs",
    "2"
);
shell_test!(
    source_non_wasi_fails,
    "source .($WASM_PATH + \"/fib.wasm\")",
    @stderr "source: error running wasm file: function not found \"_start\""
);
shell_test!(
    unload_keeps_wasi,
    "unload\nsource .($WASM_PATH + \"/hello_wasi.wasm\")",
    "unloaded 0 modules\nHello, world!"
);
shell_test!(
    wasi_sandboxing,
    "source .($WASM_PATH + \"/new_file.wasm\")",
    @stderr "thread \'main\' panicked at src/main.rs:4:31:
called `Result::unwrap()` on an `Err` value: Custom { kind: Uncategorized, error: \"failed to find a pre-opened file descriptor through which \\\"hello.txt\\\" could be opened\" }
note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
source: error running wasm file: trap: unreachable encountered: unreachable encountered"
);
shell_test!(
    wasi_sandboxing_allow,
    "allow\nsource .($WASM_PATH + \"/new_file.wasm\")\ncat hello.txt\necho works!",
    "works!"
);
shell_test!(
    wasi_sandboxing_args,
    "source .($WASM_PATH + \"/args.wasm\") hello world",
    "hello\nworld"
);
shell_test!(
    wasi_stdin,
    "echo hello | source .($WASM_PATH + \"/stdin.wasm\")",
    "Got: hello\n"
);
shell_test!(
    wasi_stdin_merge,
    "echo .(!1) %| source .($WASM_PATH + \"/stdin.wasm\")",
    "Got: wsh: type error: `!` `number`\n"
);
shell_test!(
    wasi_env_vars,
    "source .($WASM_PATH + \"/env.wasm\")\n$HELLO=nice source .($WASM_PATH + \"/env.wasm\")",
    "NOT HERE\nnice"
);
shell_test!(
    wasi_env_vars_cannot_access_from_parent,
    "export HELLO = nice\nsource .($WASM_PATH + \"/env.wasm\")",
    "NOT HERE"
);
shell_test!(
    allow_env_vars_from_parent,
    "export HELLO = nice\nallow --env HELLO\nsource .($WASM_PATH + \"/env.wasm\")",
    "nice"
);
shell_test!(
    wasi_remove,
    "allow\necho \"stuff\" > hello.txt\nsource .($WASM_PATH + \"/rm.wasm\")\nls | wc -l | xargs",
    "1"
);
shell_test!(
    memfs,
    "allow --virtual\nsource .($WASM_PATH + \"/new_file.wasm\")\nmemfs\nls | wc -l | xargs",
    "Created entries:\n    + hello.txt\n1"
);
shell_test!(
    memfs_rm,
    "allow --virtual\necho \"stuff\" > hello.txt\nsource .($WASM_PATH + \"/rm.wasm\")\nmemfs",
    "Removed entries:\n    - hello.txt"
);
shell_test!(
    memfs_new_dir,
    "allow --virtual\nsource .($WASM_PATH + \"/new_dir.wasm\")\nmemfs",
    "Created entries:\n    + hello/"
);
shell_test!(
    memfs_rm_dir,
    "allow --virtual\nmkdir hello\nsource .($WASM_PATH + \"/rm_dir.wasm\")\nmemfs",
    "Removed entries:\n    - hello/"
);
shell_test!(
    memfs_completely_virtual_dirs,
    "allow --virtual\nsource .($WASM_PATH + \"/new_dir.wasm\")\nsource .($WASM_PATH + \"/rm_dir.wasm\")\nmemfs | wc -l | xargs",
    "0"
);
shell_test!(
    memfs_readdir,
    "allow --virtual\ntouch nice.txt\ntouch hi.txt\nsource .($WASM_PATH + \"/read_dir.wasm\")\nmemfs",
    "./hi.txt\n./memfs_readdir.tmp\n./nice.txt"
);
shell_test!(
    memfs_rename,
    "allow --virtual\ntouch hello.txt\nsource .($WASM_PATH + \"/rename.wasm\")\nmemfs",
    "Created entries:\n    + nice.txt\n\nRemoved entries:\n    - hello.txt"
);

shell_test!(@fail unclosed_paren, "echo .(1 + 1");
shell_test!(@fail unfinished_pipe, "echo hi |");
shell_test!(@fail unclosed_backtick, "echo `echo hi");
shell_test!(@fail unfinished_infix, "echo .(1 +)");
shell_test!(@fail unfinished_prefix, "echo .(!)");
shell_test!(@fail bad_redirect_position, "echo hi > file.txt | cat");
shell_test!(@fail accurate_errors_with_bloated_floats, "echo .(1.0000000000000)\necho .(1");
shell_test!(
    @fail source_errors_propagate,
    "echo \"echo .(\" > __bad_tmp.wsi\nsource __bad_tmp.wsi\nrm __bad_tmp.wsi"
);
