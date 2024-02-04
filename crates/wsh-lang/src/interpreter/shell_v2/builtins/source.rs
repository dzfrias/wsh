use std::{ffi::OsString, fs, path::PathBuf};

use clap::Parser;

use anyhow::{Context, Result};
use filedescriptor::AsRawFileDescriptor;

use crate::{
    shell_v2::{pipeline::Stdio, Shell},
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
    let contents = fs::read_to_string(args.file).context("error reading file")?;
    let old = shell.global_stdout;
    shell.global_stdout = stdio.stdout.as_raw_file_descriptor();
    let source = Source::new("hi", contents);
    shell.run(&source)?;
    shell.global_stdout = old;
    Ok(())
}
