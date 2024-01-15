use std::io;

use anyhow::{ensure, Result};
use filedescriptor::{AsRawFileDescriptor, IntoRawFileDescriptor};

use crate::Shell;

pub fn unload(
    shell: &mut Shell,
    args: Vec<String>,
    stdout: &mut (impl io::Write + AsRawFileDescriptor),
    _stdin: Option<impl io::Read + IntoRawFileDescriptor>,
) -> Result<()> {
    ensure!(args.len() == 1, "expected no arguments");

    let unloaded = shell.env.unload_modules();
    writeln!(stdout, "unloaded {unloaded} modules").expect("error writing to stdout");

    Ok(())
}
