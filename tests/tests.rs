mod spectests;

macro_rules! spectests {
    ($(fn $name:ident($test:literal);)*) => {
        mod spec {
            use ::test_log::test;
            use super::spectests::run_spectest;

            $(
                #[test]
                fn $name() {
                    // Validate that the test file exists at compile time
                    include_bytes!(concat!("spectests/testsuite/", $test, ".wast"));
                    run_spectest($test).expect("error running spectest");
                }
             )*
        }
    };
}

spectests!(
    fn address("address");
    fn align("align");
    fn binary_leb128("binary-leb128");
    fn binary("binary");
    fn block("block");
    fn br("br");
    fn br_if("br_if");
    fn br_table("br_table");
    fn bulk("bulk");
    fn call("call");
    fn call_indirect("call_indirect");
    fn comments("comments");
    fn const_("const");
    fn conversions("conversions");
    fn custom("custom");
    fn data("data");
    fn elem("elem");
    fn endianness("endianness");
    fn exports("exports");
    // fn f32("f32");
    fn f32_bitwise("f32_bitwise");
    fn f32_cmp("f32_cmp");
    // fn f64("f64");
    fn f64_bitwise("f64_bitwise");
    fn f64_cmp("f64_cmp");
    fn fac("fac");
    fn float_exprs("float_exprs");
    fn float_literals("float_literals");
    fn float_memory("float_memory");
    fn float_misc("float_misc");
    fn forward("forward");
    fn func("func");
    fn func_ptrs("func_ptrs");
    fn global("global");
    fn i32("i32");
    fn i64("i64");
    fn if_("if");
    fn imports("imports");
    fn inline_module("inline-module");
    fn int_exprs("int_exprs");
    fn int_literals("int_literals");
    fn labels("labels");
    fn left_to_right("left-to-right");
    fn linking("linking");
    fn load("load");
    fn local_get("local_get");
    fn local_set("local_set");
    fn local_tee("local_tee");
    fn loop_("loop");
    fn memory("memory");
    fn memory_copy("memory_copy");
    fn memory_fill("memory_fill");
    fn memory_grow("memory_grow");
    fn memory_init("memory_init");
    fn memory_redundancy("memory_redundancy");
    fn memory_size("memory_size");
    fn memory_trap("memory_trap");
    fn nop("nop");
    fn ref_func("ref_func");
    fn ref_is_null("ref_is_null");
    fn ref_null("ref_null");
    fn return_("return");
    fn select("select");
    fn skip_stack_guard_page("skip-stack-guard-page");
    fn stack("stack");
    fn start("start");
    fn store("store");
    fn switch("switch");
    fn table_sub("table-sub");
    fn table("table");
    fn table_copy("table_copy");
    fn table_fill("table_fill");
    fn table_get("table_get");
    fn table_grow("table_grow");
    fn table_init("table_init");
    fn table_set("table_set");
    fn table_size("table_size");
    fn token("token");
    fn tokens("tokens");
    fn traps("traps");
    fn type_("type");
    fn unreachable("unreachable");
    fn unreached_invalid("unreached-invalid");
    fn unreached_valid("unreached-valid");
    fn unwind("unwind");
    fn utf8_custom_section_id("utf8-custom-section-id");
    fn utf8_import_field("utf8-import-field");
    fn utf8_import_module("utf8-import-module");
    fn utf8_invalid_encoding("utf8-invalid-encoding");
);
