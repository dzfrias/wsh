use std::{
    fs::{self, OpenOptions},
    io::Write,
    path::Path,
};

use anyhow::{Context, Result};

use crate::cli::Specgen;

pub fn specgen(args: &Specgen) -> Result<()> {
    fs::write(
        "mod.rs",
        "// This file was generated by xtask specgen\n\nuse shwasi_engine::module::Parser;\n\n",
    )
    .context("error resetting mod.rs")?;
    convert_to_wasm(&args.spectests, "wasm")
        .context("error converting spectests to WebAssembly")?;

    Ok(())
}

fn convert_to_wasm(path: impl AsRef<Path>, out_path: impl AsRef<Path>) -> Result<()> {
    for entry in path
        .as_ref()
        .read_dir()
        .context("error reading spectests path")?
    {
        let path = entry?.path();
        let name = path
            .file_stem()
            .expect("path should be valid by now")
            .to_str()
            .context("path should be valid utf-8")?;
        // Skip simd tests for now
        if name.starts_with("simd") {
            continue;
        }

        match path.extension().and_then(|ext| ext.to_str()) {
            Some("wasm") => {
                let wasm = fs::read(&path)?;
                let out = out_path.as_ref().join(name).with_extension("wasm");
                fs::write(&out, wasm).context("error writing to generated wasm file")?;
                write_test(out)?;
            }
            Some("wat") => {
                let input = fs::read_to_string(&path)?;
                let wasm = wat::parse_str(&input)?;
                let out = out_path.as_ref().join(name).with_extension("wasm");
                fs::write(&out, wasm).context("error writing to generated wasm file")?;
                write_test(out)?;
            }
            Some("wast") => {
                let input = fs::read_to_string(&path)?;
                let Ok(buf) = wast::parser::ParseBuffer::new(&input) else {
                    continue;
                };
                let wast: wast::Wast<'_> = match wast::parser::parse(&buf) {
                    Ok(wast) => wast,
                    Err(_) => continue,
                };
                for (i, directive) in wast.directives.into_iter().enumerate() {
                    match directive {
                        wast::WastDirective::Wat(mut module) => {
                            let wasm = module.encode()?;
                            let name = format!("{name}_{i}");
                            let out = out_path.as_ref().join(&name).with_extension("wasm");
                            fs::write(&out, wasm)
                                .context("error writing to generated wasm file")?;
                            write_test(out)?;
                        }
                        _ => continue,
                    }
                }
            }
            _ => continue,
        }
    }

    Ok(())
}

fn write_test(wasm: impl AsRef<Path>) -> Result<()> {
    let name = wasm
        .as_ref()
        .file_stem()
        .unwrap()
        .to_str()
        .unwrap()
        .replace('-', "_");
    let wasm_str = wasm.as_ref().to_str().unwrap();
    let test = format!(
        r#"#[test]
fn {name}() {{
    let wasm = include_bytes!("{wasm_str}");
    Parser::new(wasm).read_module().expect("module should parse with no errors");
}}

"#
    );
    let mut file = OpenOptions::new()
        .append(true)
        .open("mod.rs")
        .context("error opening mod.rs")?;
    file.write_all(test.as_bytes())
        .context("error appending to mod.rs")?;
    file.flush().context("error flushing mod.rs")?;

    Ok(())
}
