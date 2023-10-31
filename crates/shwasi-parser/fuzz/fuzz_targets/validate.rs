#![no_main]

use std::fs;

use libfuzzer_sys::{
    arbitrary::{self, Arbitrary},
    fuzz_target,
};
use shwasi_parser::{validate, Parser};
use wasm_smith::{Config, ConfiguredModule};

#[derive(Debug, Arbitrary)]
struct ShwasiConfig;

impl Config for ShwasiConfig {
    fn bulk_memory_enabled(&self) -> bool {
        true
    }
}

fuzz_target!(|data: ConfiguredModule<ShwasiConfig>| {
    let bytes = data.module.to_bytes();
    let module = Parser::new(&bytes)
        .read_module()
        .expect("should be valid module");
    if let Err(err) = validate(&module) {
        eprintln!("failed on {bytes:?}\nwith module {:#?}", data.module);
        fs::write("./crash.wasm", &bytes).unwrap();
        panic!("validator should not fail on valid modules: {err:?}");
    }
});
