use std::{ffi::OsString, fs, path::PathBuf};

use anyhow::Context;
use clap::Parser;
use wsh_engine::Instance;

use crate::shell_v2::{pipeline::Stdio, Shell};

#[derive(Debug, Parser)]
struct Args {
    path: PathBuf,

    #[arg(long = "as")]
    export: Option<String>,
}

pub fn load<I, S>(
    shell: &mut Shell,
    _stdio: Stdio,
    args: I,
    _env: &[(String, String)],
) -> anyhow::Result<()>
where
    I: IntoIterator<Item = S>,
    S: Into<OsString> + Clone,
{
    let args = Args::try_parse_from(args)?;
    let contents = fs::read(args.path).context("error reading input file")?;
    let module = wsh_parser::Parser::new(&contents)
        .read_module()
        .context("bad wasm module")?;
    let instance = Instance::instantiate(shell.env.store_mut(), module)
        .context("error instantiating wasm module")?;
    if let Some(name) = args.export {
        instance.export_as(shell.env.store_mut(), &name);
    }
    shell.env.register_module(instance);
    Ok(())
}
