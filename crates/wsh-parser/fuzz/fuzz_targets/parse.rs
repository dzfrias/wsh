#![no_main]

use libfuzzer_sys::fuzz_target;
use wsh_parser::Parser;

fuzz_target!(|data: &[u8]| {
    let parser = Parser::new(data);
    let _ = parser.read_module();
});
