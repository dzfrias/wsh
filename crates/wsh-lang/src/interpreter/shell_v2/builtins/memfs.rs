use std::{
    borrow::Cow,
    ffi::OsString,
    io::Write,
    path::{Path, PathBuf},
};

use clap::Parser;

use crate::{
    interpreter::memfs,
    shell_v2::{builtins::force_writeln, pipeline::Stdio, Shell},
};

#[derive(Debug, Parser)]
struct Args;

pub fn memfs<I, S>(
    shell: &mut Shell,
    mut stdio: Stdio,
    args: I,
    _env: &[(String, String)],
) -> anyhow::Result<()>
where
    I: IntoIterator<Item = S>,
    S: Into<OsString> + Clone,
{
    Args::try_parse_from(args)?;

    let (additions, modifications, removals) = get_status(shell);
    if !additions.is_empty() {
        force_writeln!(stdio.stdout, "Created entries:");
        for addition in additions {
            display_path("+", shell, addition, &mut stdio);
        }
        if !modifications.is_empty() || !removals.is_empty() {
            force_writeln!(stdio.stdout);
        }
    }
    if !modifications.is_empty() {
        force_writeln!(stdio.stdout, "Modified entries:");
        for modification in modifications {
            display_path("~", shell, modification, &mut stdio);
        }
        if !removals.is_empty() {
            force_writeln!(stdio.stdout);
        }
    }
    if !removals.is_empty() {
        force_writeln!(stdio.stdout, "Removed entries:");
        for (removal, ty) in removals {
            let removal = relative_to_cwd(&removal);
            force_writeln!(
                stdio.stdout,
                "    - {}{}",
                removal.display(),
                matches!(ty, memfs::EntryType::Directory)
                    .then_some("/")
                    .unwrap_or("")
            );
        }
    }

    Ok(())
}

fn display_path(prefix: &str, shell: &mut Shell, path: PathBuf, stdio: &mut Stdio) {
    let ty = shell.env.mem_fs.entry(&path).unwrap().ty();
    let modification = relative_to_cwd(&path);
    force_writeln!(
        stdio.stdout,
        "    {prefix} {}{}",
        modification.display(),
        matches!(ty, memfs::EntryType::Directory)
            .then_some("/")
            .unwrap_or("")
    );
}

fn get_status(shell: &mut Shell) -> (Vec<PathBuf>, Vec<PathBuf>, Vec<(PathBuf, memfs::EntryType)>) {
    let mut additions = vec![];
    let mut modifications = vec![];
    let mut removals = vec![];
    shell.env.mem_fs.for_each(|entry| match entry {
        memfs::Entry::File(file) => {
            let borrow = file.borrow();
            let path = borrow.path();
            if !path.exists() {
                additions.push(path.to_path_buf());
                return;
            }
            if !file.did_write() {
                return;
            }
            modifications.push(path.to_path_buf());
        }
        memfs::Entry::Directory(dir) => {
            let borrow = dir.borrow();
            let path = borrow.path();
            if path.exists() || path == Path::new("") {
                return;
            }
            additions.push(path.to_path_buf());
        }
    });
    shell.env.mem_fs.for_each_removal(|path, ty| {
        if !path.exists() {
            return;
        }

        removals.push((path.to_path_buf(), ty));
    });

    (additions, modifications, removals)
}

fn relative_to_cwd(path: &Path) -> Cow<'_, Path> {
    std::env::current_dir()
        .ok()
        .and_then(|cwd| pathdiff::diff_paths(path, cwd.canonicalize().ok()?).map(Cow::Owned))
        .unwrap_or(Cow::Borrowed(path))
}
