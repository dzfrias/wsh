use std::{ffi::OsString, fs, path::PathBuf};

use clap::Parser;

use anyhow::{Context, Result};

use crate::{
    shell_v2::{env::WasiContext, pipeline::Stdio, Shell},
    v2::{Source, SourceError},
};

#[derive(Debug, Parser)]
struct Args {
    file: PathBuf,
    args: Vec<String>,
}

pub fn source<I, S>(shell: &mut Shell, stdio: Stdio, args: I) -> Result<()>
where
    I: IntoIterator<Item = S>,
    S: Into<OsString> + Clone,
{
    let args = Args::try_parse_from(args)?;
    let contents = fs::read(&args.file).context("error reading file")?;

    const MAGIC: &[u8; 4] = b"\0asm";
    // This will detect if the file is a WebAssembly file
    if contents.len() >= 4 && &contents[0..4] == MAGIC {
        shell.env.prepare_wasi(
            WasiContext::new(&stdio.stdout, &stdio.stderr, stdio.stdin.as_ref()).args(args.args),
        );
        let module = wsh_parser::Parser::new(&contents)
            .read_module()
            .context("error parsing wasm file")?;
        let instance = wsh_engine::Instance::instantiate(shell.env.store_mut(), module)
            .context("error instantiating wasm module")?;
        let start = instance
            .get_func::<(), ()>(shell.env.store(), "_start")
            .context("error running wasm file")?;
        start
            .call(shell.env.store_mut(), ())
            .context("error running wasm file")?;
        // We don't need one-off `source`s to be kept in the store, it just wastes memory
        instance.free(shell.env.store_mut());
        return Ok(());
    }

    // If not a Wasm file, it should be a wsh script
    let contents = String::from_utf8(contents).context("error reading file")?;
    let old_stdout = shell.set_stdout(stdio.stdout);
    let old_stderr = shell.set_stderr(stdio.stderr);
    let name = args.file.file_stem().unwrap().to_string_lossy();
    let source = Source::new(&name, contents);
    let result = shell.run(&source);
    shell.set_stdout(old_stdout);
    let stderr = shell.set_stderr(old_stderr);
    if let Err(err) = result {
        err.fmt_on(&source, stderr)
            .context("problem writing error to stderr")?;
    }

    Ok(())
}
