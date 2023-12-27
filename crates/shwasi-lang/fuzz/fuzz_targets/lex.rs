#![no_main]

use libfuzzer_sys::fuzz_target;
use shwasi_lang::Lexer;

fuzz_target!(|data: &str| {
    Lexer::new(data).lex();
});
