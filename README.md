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
executables. This allows a programmer to:

- Run executables in a contained envrionment
- Suspend an executable's disk side effects in-memory
- Load functions from executables into the shell environment for testing
- Share the binaries to any system!

`wsh` empowers a WebAssembly-oriented workflow, taking advantage of the
[security](https://github.com/WebAssembly/WASI/blob/db4e3a12dadbe3e7e41dddd04888db3bf1cf7a96/docs/DesignPrinciples.md#capability-based-security),
[portability](https://webassembly.github.io/spec/core/intro/introduction.html#design-goals),
and
[module interface](https://webassembly.github.io/spec/core/intro/overview.html#concepts)
that Wasm provides. At a high-level, `wsh`'s goal is to take the power of
WebAssembly and bring it to the everyday programmer. Start building with Wasm,
and enjoy the benefits with `wsh` **today**!

## Project status

This project is still in infancy. There are many aspects of the shell that are
still under consideration. Some POSIX-style shell functionality does not exist
yet, and some syntax/API decisions still need to be made. That being said, the
Wasm functionality aims to be as polished as possible. The
[embedded Wasm runtime](https://github.com/dzfrias/wsh/tree/main/crates/wsh-engine)
passes the full
[specifcation testsuite](https://github.com/WebAssembly/testsuite/), and work
was done to make the runtime performant.

## License

This project is licenced under the
[MIT License](https://github.com/dzfrias/wsh/blob/main/LICENSE).
