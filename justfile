log := "warn"
bt := "1"

export RUST_BACKTRACE := bt
export RUST_LOG := log

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

mktest name:
  #!/bin/bash
  set -euo pipefail
  touch ./crates/shwasi-engine/tests/inputs/dumbwasm/{{name}}.dumbwasm
  cd ./crates/shwasi-engine/tests/inputs
  touch {{name}}.wasm
  echo "pub const {{uppercase(name)}}: &[u8] = include_bytes!(\"./inputs/{{name}}.wasm\");" >> ../inputs.rs
  echo created test, {{name}}.dumbwasm!

buildtest:
  #!/bin/bash
  set -euo pipefail
  cd ./crates/shwasi-engine/tests/inputs
  ls dumbwasm | xargs -I{} dumbwasm ./dumbwasm/{}
  echo built test files!
