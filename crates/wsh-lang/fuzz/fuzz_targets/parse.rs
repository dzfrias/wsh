#![no_main]

use libfuzzer_sys::fuzz_target;
use wsh_lang::{Lexer, Parser};

fuzz_target!(|data: &str| {
    let buf = Lexer::new(data).lex();
    let _ = Parser::new(&buf).parse();
});
