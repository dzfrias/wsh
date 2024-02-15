# Installing

Since this project is still in infancy (see [Project Status](../status.md)), the
best way to install `wsh` is through Rust's package manager,
[`cargo`](https://doc.rust-lang.org/cargo/). In the future, more package
managers, along with pre-built binaries, will be supported.

After installing the binary, you may start it up `wsh`:

```
$ wsh
Welcome to wsh, the WebAssembly shell!

~/code $
```

`wsh` is compatible with **all** major operating systems and architectures.

## Using `cargo`

`wsh` can be installed using `cargo`, though [crates.io](https://crates.io/).
This will build the project using Rust. The minimum supported Rust version
(MSRV) is **1.75.0** (you can check with `rustc --version`. If you do not have
`cargo`, follow the instructions at
[this link](https://www.rust-lang.org/tools/install).

```
$ cargo install wsh
...
```

## Building from source

First, you can clone the repository, with:

```
$ git clone https://github.com/dzfrias/wsh
$ cd wsh
```

Then, you can build the project with `cargo`. Make sure that you have version
**1.75.0** at least (you can check with `rustc --version`). There are _no_
external (non-Rust) dependencies needed. However, if you'd like to run the
testsuite, make sure to clone the submodules with `git submodule update --init`.

```
$ cargo run --release
...
```
