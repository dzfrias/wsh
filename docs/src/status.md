# Project Status

This project is not finished yet! Mainly, there are a few POSIX-shell features
that aren't supported in `wsh` at this point. There are also many things that
are still under consideration, so **do not** expect the language to be stable.
Some syntax hasn't been completely decided on yet, but most semantics _should_
stay the same.

That being said, this project aims to be as polished as possible, so the
[WebAssembly features](./using/webassembly/index.md) and the language
implementation should be sound.

## Planned Features

Here are some things that are planned, but haven't landed yet:

- Process substitution
- Windows `>>` (append [redirection](./using/running-commands.md#redirection))
  support
- Collection data structures (lists, maps)
- For loops
- Job control
- Signal handling
- Completion scripts

## Contributing

If you'd like to help out with implementing any of these features, feel free to
fork the [repository](https://github.com/dzfrias/wsh) and send in a PR!
