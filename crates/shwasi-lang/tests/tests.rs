use shwasi_lang::{Shell, Value};

macro_rules! shwasi_test {
    ($name:ident, $code:expr, $expected:expr) => {
        #[test]
        fn $name() {
            let input = $code;
            let expected = Some($expected);
            let mut interpreter = Shell::new();
            let result = interpreter.run(input, "test").unwrap();
            assert_eq!(expected, result);
        }
    };
}

shwasi_test!(
    simple_arithmetic,
    ".(1 + 2 * 3 - 4 / 5)",
    Value::Number(6.2)
);

shwasi_test!(
    string_concat,
    ".(\"hello\" + \" \" + \"world\")",
    Value::String("hello world".into())
);

shwasi_test!(
    string_repeat,
    ".(\"hello\" * 3)",
    Value::String("hellohellohello".into())
);

shwasi_test!(
    string_concat_number,
    ".(\"hello\" + 3 + \"hello2\" + 3.3)",
    Value::String("hello3hello23.3".into())
);

shwasi_test!(
    coerce_string_to_number,
    ".(\"10\" + 3.3 + -\"1\")",
    Value::Number(12.3)
);

shwasi_test!(
    commutative_string_number,
    ".(\"3.3\" + \"hello\")",
    Value::String("3.3hello".into())
);

shwasi_test!(
    bool_string_compare,
    ".(true == \"true\")",
    Value::Bool(true)
);

shwasi_test!(
    bool_string_add,
    ".(true + \"false\")",
    Value::String("truefalse".into())
);

shwasi_test!(
    implicit_concat,
    ".(\"hello\"(\"nice\" + \"cool\"))",
    Value::String("hellonicecool".into())
);

shwasi_test!(if_stmt, "if true then .x = 1 end\n.x", Value::Number(1.0));

shwasi_test!(
    while_stmt,
    ".x = 0\nwhile x < 5 do .x = x + 1 end\n.x",
    Value::Number(5.0)
);
