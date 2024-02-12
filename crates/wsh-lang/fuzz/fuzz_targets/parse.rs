#![no_main]

use libfuzzer_sys::fuzz_target;
use wsh_lang::v2::{Parser, Source};

fuzz_target!(|data: &str| {
    let source = Source::new("<fuzz>", data.to_string());
    let _ = Parser::new(&source).parse();
});
