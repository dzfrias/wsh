use std::{ffi::OsString, fs, path::PathBuf};

use clap::Parser;

use anyhow::{bail, Context, Result};
use filedescriptor::AsRawFileDescriptor;

use crate::{
    shell_v2::{env::WasiContext, error::ErrorKind, pipeline::Stdio, Shell},
    v2::Source,
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
    let (old_stdout, old_stderr) = (shell.global_stdout, shell.global_stderr);
    (shell.global_stdout, shell.global_stderr) = (
        stdio.stdout.as_raw_file_descriptor(),
        stdio.stderr.as_raw_file_descriptor(),
    );
    let source = Source::new(args.file.to_string_lossy().to_string(), contents);
    if let Err(err) = shell.run(&source) {
        if let ErrorKind::ParseError(parse_error) = err.kind() {
            source.fmt_error(parse_error, stdio.stderr)?;
            bail!("parse error")
        } else {
            bail!(err);
        }
    }
    (shell.global_stdout, shell.global_stderr) = (old_stdout, old_stderr);
    Ok(())
}
