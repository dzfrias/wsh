# WebAssembly Inputs

This directory holds WebAssembly binaries that are used in shell tests. There
are two categories of modules: WASI and non-WASI. Descriptions of each input
type can be found in their respective sections.

## WASI Inputs

WASI modules should export a `_start` function (as is standard). In the shell,
these files are run using the `source` command. No default permissions are
provided to the modules.

| File              | Description                                  |
| ----------------- | -------------------------------------------- |
| `args.wasm`       | Writes each argument to stdout.              |
| `hello_wasi.wasm` | Writes "Hello, World!" to stdout.            |
| `new_file.wasm`   | Attempts to create a file named `hello.txt`. |
| `stdin.wasm`      | Writes `Got: {STDIN}` to stdout.             |

## Non-WASI Inputs

Non-WASI modules can have arbitrary exported functions. These should **not** use
`wasi_snapshot_preview1`.

| Name       | Export            | Description                               |
| ---------- | ----------------- | ----------------------------------------- |
| `fib.wasm` | `fib(i32) -> i32` | Calculates the fibonacci number provided. |
