use std::{fs, path::PathBuf};

use anyhow::{Context, Result};
use clap::Parser;
use shwasi_engine::Instance;

use crate::{interpreter::builtins::IoStreams, Shell};

#[derive(Debug, Parser)]
struct Args {
    path: PathBuf,

    #[arg(long = "as")]
    export: Option<String>,
}

pub fn load(shell: &mut Shell, args: Vec<String>, _io_streams: &mut IoStreams) -> Result<()> {
    let args = Args::try_parse_from(args)?;
    let contents = fs::read(&args.path).context("error reading input file")?;

    // Loading WASM file
    const MAGIC: &[u8; 4] = b"\0asm";
    if contents.len() >= 4 && &contents[0..4] == MAGIC {
        let module = shwasi_parser::Parser::new(&contents)
            .read_module()
            .context("bad wasm module")?;
        let instance = Instance::instantiate(shell.env.store_mut(), module)
            .context("error instantiating wasm module")?;
        if let Some(name) = args.export {
            instance.export_as(shell.env.store_mut(), &name);
        }
        shell.env.register_module(instance);
        return Ok(());
    }

    let contents = String::from_utf8(contents).context("error reading shwasi file as UTF-8")?;
    shell
        .load(
            &contents,
            &args.path.file_stem().unwrap().to_string_lossy(),
            &args.export.unwrap(),
        )
        .unwrap();

    Ok(())
}
