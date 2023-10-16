use std::fmt::Write;

use shwasi_engine::Module;

macro_rules! pretty_fmt_all {
    ($out:expr, $module:expr, $field:ident) => {
        if !$module.$field.is_empty() {
            let _ = writeln!($out, concat!(stringify!($field), "\n---"));
            for (i, x) in $module.$field.iter().enumerate() {
                let _ = writeln!($out, "{i}: {x}");
            }
            let _ = writeln!($out);
        }
    };
}

pub fn pretty_fmt(module: &Module) -> String {
    let mut s = String::new();

    pretty_fmt_all!(s, module, types);
    pretty_fmt_all!(s, module, imports);
    pretty_fmt_all!(s, module, functions);
    pretty_fmt_all!(s, module, tables);
    pretty_fmt_all!(s, module, memories);
    pretty_fmt_all!(s, module, globals);
    pretty_fmt_all!(s, module, exports);

    if let Some(start) = module.start {
        let _ = writeln!(s, "start: {start}\n");
    }

    pretty_fmt_all!(s, module, elements);
    pretty_fmt_all!(s, module, datas);
    pretty_fmt_all!(s, module, codes);

    if let Some(data_count) = module.data_count {
        let _ = writeln!(s, "data count: {data_count}\n");
    }

    s
}
