use std::{fs, io};

use filedescriptor::{FileDescriptor, FromRawFileDescriptor, IntoRawFileDescriptor};

use crate::{Shell, ShellResult};

pub fn source<I, S>(
    shell: &mut Shell,
    args: I,
    mut stdout: impl io::Write + IntoRawFileDescriptor,
) -> ShellResult<i32>
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    let args = args.into_iter().collect::<Vec<_>>();
    // TOOD: support passing args to scripts
    let Some((file, _args)) = args.split_first() else {
        writeln!(stdout, "source: no file provided").expect("error writing to stdout");
        return Ok(1);
    };
    let contents = match fs::read(file.as_ref()) {
        Ok(contents) => contents,
        Err(err) => {
            writeln!(stdout, "source: error reading file: {err}").expect("error writing to stdout");
            return Ok(1);
        }
    };
    const MAGIC: &[u8; 4] = b"\0asm";
    // Automatically detect if the file is a wasm file. If it is, we can run it directly. This
    // check involves reading the first 4 bytes of the file, so it's not free.
    if contents.len() >= 4 && &contents[0..4] == MAGIC {
        // TODO: error handling
        let module = shwasi_parser::Parser::new(&contents).read_module().unwrap();
        let instance = shwasi_engine::Instance::instantiate(shell.env.store_mut(), module).unwrap();
        let start = instance
            .get_func::<(), ()>(shell.env.store(), "_start")
            .unwrap();
        start.call(shell.env.store_mut(), ()).unwrap();
        return Ok(0);
    }
    let Ok(contents) = String::from_utf8(contents) else {
        writeln!(stdout, "source: error reading file: invalid utf8")
            .expect("error writing to stdout");
        return Ok(1);
    };
    let fd = stdout.into_raw_file_descriptor();
    // SAFETY: `fd` is a valid file descriptor on account of it being from an
    // `IntoRawFileDescriptor`. We later close it, so we don't leak it.
    unsafe { shell.stdout(fd) };
    let _ = shell.run(&contents, file.as_ref());
    // We need to be careful that the file descriptor is closed, so we don't leak it. This can
    // cause hangs in the shell if used `source` is used with a pipe. This is because the shell
    // will never close the `stdout` it's given, so we must manually close it here.
    let _ = unsafe { FileDescriptor::from_raw_file_descriptor(fd) };

    Ok(0)
}
