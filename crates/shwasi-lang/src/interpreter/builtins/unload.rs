use std::io;

use anyhow::{ensure, Result};
use filedescriptor::AsRawFileDescriptor;

use crate::Shell;

pub fn unload<I, S>(
    shell: &mut Shell,
    args: I,
    stdout: &mut (impl io::Write + AsRawFileDescriptor),
) -> Result<()>
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    ensure!(
        args.into_iter().next().is_none(),
        "unload: expected no arguments"
    );

    let unloaded = shell.env.unload_modules();
    writeln!(stdout, "unloaded {unloaded} modules").expect("error writing to stdout");

    Ok(())
}
