mod spectests;

use self::spectests::run_spectest;
use test_log::test;

macro_rules! spectest {
    ($name:ident) => {
        #[test]
        fn $name() {
            run_spectest(stringify!($name)).expect("should have no errors running spectest");
        }
    };
}

spectest!(address);
spectest!(exports);
spectest!(linking);
