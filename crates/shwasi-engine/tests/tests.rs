mod inputs;
mod pretty_fmt;

use insta::{assert_display_snapshot, assert_snapshot};
use shwasi_engine::Parser;

use crate::{inputs::*, pretty_fmt::pretty_fmt};

#[test]
fn simple() {
    let module = Parser::new(SIMPLE)
        .read_module()
        .expect("module should parse with no errors");
    assert_snapshot!(pretty_fmt(&module));
}

#[test]
fn fails_on_bad_section_order() {
    let result = Parser::new(SECTION_ORDER).read_module();
    let err = result.unwrap_err();
    assert_display_snapshot!(err.root_cause(), @"the function section is out of order");
}

#[test]
fn can_parse_instructions_with_placeholders() {
    let module = Parser::new(PLACEHOLDER_BYTE)
        .read_module()
        .expect("module should parse with no errors");
    assert_snapshot!(pretty_fmt(&module));
}

#[test]
fn bad_placeholder_byte() {
    let result = Parser::new(BAD_PLACEHOLDER_BYTE).read_module();
    let err = result.unwrap_err();
    assert_display_snapshot!(err.root_cause(), @"placeholder byte must be 0x00");
}

#[test]
fn bad_magic() {
    let result = Parser::new(BAD_MAGIC).read_module();
    let err = result.unwrap_err();
    assert_display_snapshot!(err.root_cause(), @"bad magic value: 2155905152");
}

#[test]
fn bad_version() {
    let result = Parser::new(BAD_VERSION).read_module();
    let err = result.unwrap_err();
    assert_display_snapshot!(err.root_cause(), @"bad version: 276856960");
}
