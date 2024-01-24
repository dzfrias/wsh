use std::io::Write;

use anyhow::{ensure, Result};

use crate::{interpreter::builtins::IoStreams, Shell};

pub fn unload(shell: &mut Shell, args: Vec<String>, io_streams: &mut IoStreams) -> Result<()> {
    ensure!(args.len() == 1, "expected no arguments");

    let unloaded = shell.env.unload_modules();
    writeln!(io_streams.stdout, "unloaded {unloaded} modules").expect("error writing to stdout");

    Ok(())
}
