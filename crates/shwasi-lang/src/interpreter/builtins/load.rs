use std::{ffi::OsStr, fs, io};

use filedescriptor::IntoRawFileDescriptor;
use shwasi_engine::Instance;

use crate::{Shell, ShellResult};

pub fn load<I, S>(
    shell: &mut Shell,
    args: I,
    mut stdout: impl io::Write + IntoRawFileDescriptor,
) -> ShellResult<i32>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let args = args.into_iter().collect::<Vec<_>>();
    let Some((file, _)) = args.split_first() else {
        writeln!(stdout, "load: no file provided").expect("error writing to stdout");
        return Ok(1);
    };

    let contents = match fs::read(file.as_ref()) {
        Ok(contents) => contents,
        Err(err) => {
            writeln!(stdout, "load: error reading input file: {err}")
                .expect("error writing to stdout");
            return Ok(1);
        }
    };
    let module = match shwasi_parser::Parser::new(&contents).read_module() {
        Ok(contents) => contents,
        Err(err) => {
            writeln!(stdout, "load: bad wasm module: {err}").expect("error writing to stdout");
            return Ok(1);
        }
    };
    let instance = match Instance::instantiate(shell.env.store_mut(), module) {
        Ok(instance) => instance,
        Err(err) => {
            writeln!(stdout, "load: error instantiating wasm module: {err}")
                .expect("error writing to stdout");
            return Ok(1);
        }
    };
    shell.env.register_module(instance);

    Ok(0)
}
