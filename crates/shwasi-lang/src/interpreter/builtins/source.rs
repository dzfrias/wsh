use std::{fs, io};

use anyhow::{Context, Result};
use filedescriptor::{AsRawFileDescriptor, FileDescriptor, FromRawFileDescriptor};

use crate::{interpreter::builtins::Args, Shell};

pub fn source(
    shell: &mut Shell,
    args: Args,
    stdout: &mut (impl io::Write + AsRawFileDescriptor),
    stderr: &mut (impl io::Write + AsRawFileDescriptor),
) -> Result<()> {
    // TOOD: support passing args to scripts
    let (file, _args) = args.positional.split_first().context("no file provided")?;
    let contents = fs::read(file).context("error reading file")?;

    const MAGIC: &[u8; 4] = b"\0asm";
    // Automatically detect if the file is a wasm file. If it is, we can run it directly. This
    // check involves reading the first 4 bytes of the file for the special wasm magic number.
    if contents.len() >= 4 && &contents[0..4] == MAGIC {
        let module = shwasi_parser::Parser::new(&contents)
            .read_module()
            .context("error parsing wasm file")?;
        let instance = shwasi_engine::Instance::instantiate(shell.env.store_mut(), module)
            .context("error instantiating wasm module")?;
        let start = instance
            .get_func::<(), ()>(shell.env.store(), "_start")
            .context("error running wasm file")?;
        start
            .call(shell.env.store_mut(), ())
            .context("error running wasm file")?;
        return Ok(());
    }

    let contents = String::from_utf8(contents).context("error reading file")?;
    let stdout_fd = stdout.as_raw_file_descriptor();
    let stderr_fd = stderr.as_raw_file_descriptor();
    // SAFETY: `fd` is a valid file descriptor on account of it being from an
    // `IntoRawFileDescriptor`. We later close it, so we don't leak it.
    unsafe { shell.stdout(stdout_fd) };
    unsafe { shell.stderr(stderr_fd) };
    let _ = shell.run(&contents, file.as_ref());
    // We need to be careful that the file descriptor is closed, so we don't leak it. This can
    // cause hangs in the shell if used `source` is used with a pipe. This is because the shell
    // will never close the `stdout` it's given, so we must manually close it here.
    let _ = unsafe { FileDescriptor::from_raw_file_descriptor(stdout_fd) };
    let _ = unsafe { FileDescriptor::from_raw_file_descriptor(stderr_fd) };

    Ok(())
}
