#![no_main]

use std::fs;

use libfuzzer_sys::{
    arbitrary::{self, Arbitrary},
    fuzz_target,
};
use shwasi_parser::Parser;
use wasm_smith::{Config, ConfiguredModule};

#[derive(Debug, Arbitrary)]
struct ShwasiConfig;

impl Config for ShwasiConfig {
    fn generate_custom_sections(&self) -> bool {
        true
    }

    fn bulk_memory_enabled(&self) -> bool {
        true
    }
}

fuzz_target!(|data: ConfiguredModule<ShwasiConfig>| {
    let bytes = data.module.to_bytes();
    let parser = Parser::new(&bytes);
    if let Err(err) = parser.read_module() {
        eprintln!("failed on {bytes:?}\nwith module {:#?}", data.module);
        fs::write("./crash.wasm", &bytes).unwrap();
        panic!("parser should not fail on valid modules: {err:?}");
    }
});
