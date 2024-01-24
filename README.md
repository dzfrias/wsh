<div align="center">
  <h1><code>wsh</code></h1>
  <p>
    <strong>
      A <a href="https://webassembly.org/">WebAssembly</a>-based shell.
    </strong>
  </p>
</div>

## Why WebAssembly?

`wsh` brings a [WebAssembly](https://webassembly.org/) sandbox straight to the
shell, providing an interactive interface for manipulating and restricting Wasm
executables. This includes:

- Sandboxed disk access
- An in-memory file system
- Dynamic Wasm module loading
- Wasm linking overrides
- All cross platform!

These capabilities open the door to a number of things, like:

- Run untrusted executables safely on a user machine
- Diff changes executables attempt to make
- Load _individual functions_ into the shell environment for testing
- Change program behavior post-compilation
- Share the binaries to any system!

`wsh` empowers a WebAssembly-oriented workflow, taking advantage of the
security, portability, and module interface that Wasm provides. Start building
with Wasm, and enjoy the benefits with `wsh` **today**!

## Project status

This project is still in infancy. There are many aspects of the shell that are
still under consideration. Some POSIX-style shell functionality does not exist
yet, and some syntax/API decisions still need to be made.

That being said, Wasm functionality aims to be as polished as possible. The
embedded Wasm runtime passes the full
[specifcation testsuite](https://github.com/WebAssembly/testsuite/), and work
was done to make the runtime performant.
