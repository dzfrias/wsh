log := "warn"
bt := "1"

export RUST_BACKTRACE := bt
export RUST_LOG := log

alias c := check
alias b := build
alias mkt := mktest

default: build check test

check:
  cargo clippy

test:
  cargo nextest run --all

build *ARGS:
  cargo build {{ARGS}}
  @echo built project!
  @just buildtest

watch:
  #!/bin/bash
  set -euo pipefail
  cd ./crates/shwasi-engine/tests/inputs
  echo "watching for test input changes!"
  fswatch dumbwasm -0 | xargs -0 -n1 -I{} dumbwasm {}

mktest NAME:
  #!/bin/bash
  set -euo pipefail
  touch ./crates/shwasi-engine/tests/inputs/dumbwasm/{{NAME}}.dumbwasm
  cd ./crates/shwasi-engine/tests/inputs
  touch {{NAME}}.wasm
  echo "pub const {{uppercase(NAME)}}: &[u8] = include_bytes!(\"./inputs/{{NAME}}.wasm\");" >> ../inputs.rs
  echo created test, {{NAME}}.dumbwasm!

buildtest:
  #!/bin/bash
  set -euo pipefail
  cd ./crates/shwasi-engine/tests/inputs
  ls dumbwasm | xargs -I{} dumbwasm ./dumbwasm/{}
  echo built test files!

fuzz *ARGS="parse":
  #!/bin/bash
  set -euo pipefail
  cd ./crates/shwasi-engine
  cargo fuzz run {{ARGS}}
