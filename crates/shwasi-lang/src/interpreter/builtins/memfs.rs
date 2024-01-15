use std::io;

use anyhow::{ensure, Result};
use filedescriptor::{AsRawFileDescriptor, IntoRawFileDescriptor};

use crate::{interpreter::memfs, Shell};

pub fn memfs(
    shell: &mut Shell,
    args: Vec<String>,
    stdout: &mut (impl io::Write + AsRawFileDescriptor),
    _stdin: Option<impl io::Read + IntoRawFileDescriptor>,
) -> Result<()> {
    ensure!(args.len() == 1, "expected no arguments");

    shell.env.mem_fs.for_each(|entry| match entry {
        memfs::Entry::File(file) => {
            let path = std::env::current_dir()
                .ok()
                .and_then(|cwd| pathdiff::diff_paths(file.borrow().path(), cwd))
                .unwrap_or_else(|| file.borrow().path().to_path_buf());
            if !path.exists() {
                writeln!(stdout, "+ {}", path.display()).expect("write to stdout failed!");
                return;
            }
            writeln!(stdout, "~ {}", path.display()).expect("write to stdout failed!");
        }
        memfs::Entry::Directory(_) => {}
    });
    shell.env.mem_fs.for_each_removal(|path| {
        let path = std::env::current_dir()
            .ok()
            .and_then(|cwd| pathdiff::diff_paths(path, cwd))
            .unwrap_or_else(|| path.to_path_buf());
        if !path.exists() {
            return;
        }

        writeln!(stdout, "- {}", path.display()).expect("write to stdout failed!");
    });

    Ok(())
}
