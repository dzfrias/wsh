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

#[test]
fn read_overflow() {
    let result = Parser::new(READ_OVERFLOW).read_module();
    let err = result.unwrap_err();
    assert_display_snapshot!(err.root_cause(), @"leb128 integer too long");
}

#[test]
fn signed_overflow() {
    let result = Parser::new(SIGNED_OVERFLOW).read_module();
    let err = result.unwrap_err();
    assert_display_snapshot!(err.root_cause(), @"leb128 integer too long");
}

#[test]
fn prefix_byte() {
    let module = Parser::new(PREFIX_BYTE).read_module().unwrap();
    assert_snapshot!(pretty_fmt(&module));
}

#[test]
fn globals() {
    let module = Parser::new(GLOBALS).read_module().unwrap();
    assert_snapshot!(pretty_fmt(&module));
}

#[test]
fn bad_init_expr() {
    let result = Parser::new(BAD_INIT_EXPR).read_module();
    let err = result.unwrap_err();
    assert_display_snapshot!(err.root_cause(), @"init expr can must have 2+ instructions");
}

#[test]
fn data() {
    let module = Parser::new(DATA).read_module().unwrap();
    assert_snapshot!(pretty_fmt(&module));
}

#[test]
fn init_expr_wrong_instr() {
    let result = Parser::new(INIT_EXPR_WRONG_INSTR).read_module();
    let err = result.unwrap_err();
    assert_display_snapshot!(err.root_cause(), @"init expr instruction is not const-valid");
}

#[test]
fn bad_data_count() {
    let result = Parser::new(BAD_DATA_COUNT).read_module();
    let err = result.unwrap_err();
    assert_display_snapshot!(err.root_cause(), @"data count section does not match amount of data segments");
}

#[test]
fn data_count() {
    let module = Parser::new(DATA_COUNT).read_module().unwrap();
    assert_snapshot!(pretty_fmt(&module));
}

#[test]
fn duplicate_sections() {
    let result = Parser::new(DUPLICATE_SECTIONS).read_module();
    let err = result.unwrap_err();
    assert_display_snapshot!(err.root_cause(), @"duplicate section: function");
}

#[test]
fn import_table() {
    let module = Parser::new(IMPORT_TABLE).read_module().unwrap();
    assert_snapshot!(pretty_fmt(&module));
}

#[test]
fn import_section_order() {
    let module = Parser::new(IMPORT_SECTION_ORDER).read_module().unwrap();
    assert_snapshot!(pretty_fmt(&module));
}

#[test]
fn complex_init() {
    let module = Parser::new(COMPLEX_INIT).read_module().unwrap();
    assert_snapshot!(pretty_fmt(&module));
}

#[test]
fn code_and_types() {
    let module = Parser::new(CODE_AND_TYPES).read_module().unwrap();
    assert_snapshot!(pretty_fmt(&module));
}