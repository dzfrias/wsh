use shwasi_lang::{Interpreter, Lexer, Parser, Value};

macro_rules! shwasi_test {
    ($name:ident, $code:expr, $expected:expr) => {
        #[test]
        fn $name() {
            let input = $code;
            let expected = Some($expected);
            let buf = Lexer::new(input).lex();
            let ast = Parser::new(&buf).parse().unwrap();
            let mut interpreter = Interpreter::new();
            let result = interpreter.run(ast).unwrap();
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
