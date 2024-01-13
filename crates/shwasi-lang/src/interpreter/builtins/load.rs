use std::{fs, io};

use anyhow::{Context, Result};
use filedescriptor::{AsRawFileDescriptor, IntoRawFileDescriptor};
use shwasi_engine::Instance;

use crate::{
    interpreter::builtins::{Args, ArgsValidator, Positionals},
    Shell,
};

pub fn load(
    shell: &mut Shell,
    args: Args,
    _stdout: &mut (impl io::Write + AsRawFileDescriptor),
    _stdin: Option<impl io::Read + IntoRawFileDescriptor>,
) -> Result<()> {
    ArgsValidator::default()
        .positionals(Positionals::Exact(1))
        .single("as")
        .validate(&args)?;

    let contents = fs::read(&args.positional[0]).context("error reading input file")?;
    let module = shwasi_parser::Parser::new(&contents)
        .read_module()
        .context("bad wasm module")?;
    let instance = Instance::instantiate(shell.env.store_mut(), module)
        .context("error instantiating wasm module")?;
    if let Some(name) = args.get_argv1("as") {
        instance.export_as(shell.env.store_mut(), name);
    }
    shell.env.register_module(instance);

    Ok(())
}
