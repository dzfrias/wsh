log := "warn"
bt := "0"

export RUST_BACKTRACE := bt
export RUST_LOG := log

alias c := check
alias b := build
alias mkt := mktest
alias rev := review

default: build check test fmt

check:
  cargo clippy --workspace

test:
  cargo nextest run --workspace

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

mktest NAME KIND="good":
  #!/bin/bash
  set -euo pipefail
  touch ./crates/shwasi-engine/tests/inputs/dumbwasm/{{NAME}}.dumbwasm
  cd ./crates/shwasi-engine/tests/inputs
  touch {{NAME}}.wasm
  echo "pub const {{uppercase(NAME)}}: &[u8] = include_bytes!(\"./inputs/{{NAME}}.wasm\");" >> ../inputs.rs
  echo created test, {{NAME}}.dumbwasm!
  if [[ {{KIND}} == "good" ]]; then
    printf "\n\n#[test]\nfn {{NAME}}() {\n    let module = Parser::new({{uppercase(NAME)}}).read_module().unwrap();\n    assert_snapshot!(pretty_fmt(&module));\n}" >> ../tests.rs
  else
    printf "\n\n#[test]\nfn {{NAME}}() {\n    let result = Parser::new({{uppercase(NAME)}}).read_module();\n    let err = result.unwrap_err();\n    assert_display_snapshot!(err.root_cause(), @\"\");\n}" >> ../tests.rs
  fi
  $EDITOR ./dumbwasm/{{NAME}}.dumbwasm

crash NAME:
  #!/bin/bash
  set -euo pipefail
  mv ./crates/shwasi-engine/crash.wasm ./crates/shwasi-engine/tests/inputs/{{NAME}}.wasm
  cd ./crates/shwasi-engine/tests/inputs
  echo "pub const {{uppercase(NAME)}}: &[u8] = include_bytes!(\"./inputs/{{NAME}}.wasm\");" >> ../inputs.rs
  printf "\n\n#[test]\nfn {{NAME}}() {\n    let module = Parser::new({{uppercase(NAME)}}).read_module().unwrap();\n    assert_snapshot!(pretty_fmt(&module));\n}" >> ../tests.rs
  echo "created crash test, {{NAME}}.wasm!"

fmt *ARGS:
  cargo fmt {{ARGS}}

review:
  @cargo insta review

buildtest:
  cd ./crates/shwasi-engine/tests/inputs && ls dumbwasm | xargs -I{} dumbwasm ./dumbwasm/{}
  echo built test files!

fuzz *ARGS="parse":
  cd ./crates/shwasi-engine && cargo fuzz run {{ARGS}}
