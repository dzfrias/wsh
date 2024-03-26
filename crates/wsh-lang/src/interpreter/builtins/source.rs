use std::{fs, io::Read, path::Path};

use anyhow::{Context, Result};
use clap::Parser;
use filedescriptor::{
    AsRawFileDescriptor, FileDescriptor, FromRawFileDescriptor, IntoRawFileDescriptor,
};

use crate::{interpreter::builtins::IoStreams, Shell};

#[derive(Debug, Parser)]
struct Args {
    file: String,
    args: Vec<String>,
    #[arg(short, long)]
    url: bool,
}

pub fn source(
    shell: &mut Shell,
    args: Vec<String>,
    io_streams: &mut IoStreams,
    env: &[(String, String)],
) -> Result<()> {
    let Args {
        file,
        mut args,
        url,
    } = Args::try_parse_from(args)?;
    let contents = if url {
        let mut buf = vec![];
        reqwest::blocking::get(&file)?.read_to_end(&mut buf)?;
        buf
    } else {
        // TOOD: support passing args to scripts
        fs::read(&file).context("error reading file")?
    };

    const MAGIC: &[u8; 4] = b"\0asm";
    // Automatically detect if the file is a wasm file. If it is, we can run it directly. This
    // check involves reading the first 4 bytes of the file for the special wasm magic number.
    if contents.len() >= 4 && &contents[0..4] == MAGIC {
        let name = Path::new(&file)
            .file_name()
            .expect("should have file name")
            .to_string_lossy()
            .into_owned();
        args.insert(0, name);
        // SAFETY: `stdin` is a valid raw fd because it came from a File
        unsafe {
            shell.env.prepare_wasi(
                args,
                io_streams
                    .stdin
                    .take()
                    .map(|s| s.into_raw_file_descriptor()),
                env,
            )?;
        }
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

    let contents = String::from_utf8(contents).context("error reading file")?;
    let stdout_fd = io_streams.stdout.as_raw_file_descriptor();
    let stderr_fd = io_streams.stderr.as_raw_file_descriptor();
    // SAFETY: `fd` is a valid file descriptor on account of it being from an
    // `IntoRawFileDescriptor`. We later close it, so we don't leak it.
    unsafe { shell.stdout(stdout_fd) };
    unsafe { shell.stderr(stderr_fd) };
    let _ = shell.run(&contents, &file);
    // We need to be careful that the file descriptor is closed, so we don't leak it. This can
    // cause hangs in the shell if used `source` is used with a pipe. This is because the shell
    // will never close the `stdout` it's given, so we must manually close it here.
    let _ = unsafe { FileDescriptor::from_raw_file_descriptor(stdout_fd) };
    let _ = unsafe { FileDescriptor::from_raw_file_descriptor(stderr_fd) };

    Ok(())
}
