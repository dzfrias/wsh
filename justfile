log := "warn"
bt := "0"

export RUST_BACKTRACE := bt
export RUST_LOG := log

alias c := check
alias b := build
alias rev := review

default: fallback

check:
  cargo clippy --workspace

test:
  cargo nextest run --workspace

build *ARGS:
  cargo build {{ARGS}}
  @just parser build
  @echo built project!

fmt *ARGS:
  cargo fmt {{ARGS}}

review:
  @cargo insta review

bench *ARGS:
  cargo bench --workspace {{ARGS}}

parser *ARGS:
  @just -f ./crates/shwasi-parser/justfile {{ARGS}}

fallback:
  @just build
  @just test
  @just fmt
  @just check
