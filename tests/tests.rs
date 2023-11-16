mod spectests;

use self::spectests::run_spectest;
use test_log::test;

macro_rules! spectests {
    ($($name:tt),* $(,)?) => {
        paste::paste! {
            $(
                #[test]
                fn [<spec_ $name>]() {
                    run_spectest(stringify!($name)).expect("should have no errors running spectest");
                }
             )*
        }
    };
}

spectests!(
    address,
    align,
    br,
    br_if,
    br_table,
    binary,
    block,
    exports,
    linking,
    unreachable,
    if,
    start,
    loop,
);
