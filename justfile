log := "warn"
bt := "0"

export RUST_BACKTRACE := bt
export RUST_LOG := log

alias c := check
alias b := build
alias rev := review

default: build check test fmt

check:
  cargo clippy --workspace

test:
  cargo nextest run --workspace

build *ARGS:
  cargo build {{ARGS}}
  @just engine buildtest
  @echo built project!

fmt *ARGS:
  cargo fmt {{ARGS}}

review:
  @cargo insta review

engine *ARGS:
  @just -f ./crates/shwasi-engine/justfile {{ARGS}}
