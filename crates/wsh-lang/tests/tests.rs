use wsh_lang::{Shell, Value};

macro_rules! wsh_test {
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

wsh_test!(
    simple_arithmetic,
    ".(1 + 2 * 3 - 4 / 5)",
    Value::Number(6.2)
);

wsh_test!(
    string_concat,
    ".(\"hello\" + \" \" + \"world\")",
    Value::String("hello world".into())
);

wsh_test!(
    string_repeat,
    ".(\"hello\" * 3)",
    Value::String("hellohellohello".into())
);

wsh_test!(
    string_concat_number,
    ".(\"hello\" + 3 + \"hello2\" + 3.3)",
    Value::String("hello3hello23.3".into())
);

wsh_test!(
    coerce_string_to_number,
    ".(\"10\" + 3.3 + -\"1\")",
    Value::Number(12.3)
);

wsh_test!(
    commutative_string_number,
    ".(\"3.3\" + \"hello\")",
    Value::String("3.3hello".into())
);

wsh_test!(
    bool_string_compare,
    ".(true == \"true\")",
    Value::Bool(true)
);

wsh_test!(
    bool_string_add,
    ".(true + \"false\")",
    Value::String("truefalse".into())
);

wsh_test!(
    implicit_concat,
    ".(\"hello\"(\"nice\" + \"cool\"))",
    Value::String("hellonicecool".into())
);

wsh_test!(if_stmt, "if true then .x = 1 end\n.x", Value::Number(1.0));

wsh_test!(
    if_else_stmt,
    "if false then .x = 1 else .x = 10 end\n.x",
    Value::Number(10.0)
);

wsh_test!(
    while_stmt,
    ".x = 0\nwhile x < 5 do .x = x + 1 end\n.x",
    Value::Number(5.0)
);

wsh_test!(
    break_from_while,
    ".x = 0
while true do
  if x == 10 then
    break
  end
  .x = x + 1
end
.x",
    Value::Number(10.0)
);

wsh_test!(
    continue_in_while,
    ".x = 0
.y = 1
while x < 10 do
  .x = x + 1
  if x == 5 then
    continue
  end
  .y = y * 2
end
.y",
    Value::Number(512.0)
);
