use std::{fs, io};

use anyhow::{Context, Result};
use filedescriptor::AsRawFileDescriptor;
use shwasi_engine::Instance;

use crate::Shell;

pub fn load<I, S>(
    shell: &mut Shell,
    args: I,
    _stdout: &mut (impl io::Write + AsRawFileDescriptor),
) -> Result<()>
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    let args = args.into_iter().collect::<Vec<_>>();
    let (file, _) = args.split_first().context("no file provided")?;

    let contents = fs::read(file.as_ref()).context("error reading input file")?;
    let module = shwasi_parser::Parser::new(&contents)
        .read_module()
        .context("bad wasm module")?;
    let instance = Instance::instantiate(shell.env.store_mut(), module)
        .context("error instantiating wasm module")?;
    shell.env.register_module(instance);

    Ok(())
}
