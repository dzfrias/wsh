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

test *ARGS:
  RUST_LOG={{log}} RUST_BACKTRACE={{bt}} cargo nextest run --workspace {{ARGS}}

build *ARGS:
  cargo build {{ARGS}}
  @just parser build
  @echo built project!

fmt *ARGS:
  cargo fmt {{ARGS}} --all

review:
  @cargo insta review

bench *ARGS:
  cargo bench --workspace {{ARGS}}

parser *ARGS:
  @just -f ./crates/shwasi-parser/justfile {{ARGS}}

xtask *ARGS:
  cargo xtask {{ARGS}}

@fallback:
  just build
  RUST_LOG={{log}} RUST_BACKTRACE={{bt}} just test
  just fmt
  just check
