use std::{
    borrow::Cow,
    fmt,
    io::Write,
    path::{Path, PathBuf},
};

use anyhow::{bail, Context, Result};
use clap::{Args as ClapArgs, Parser, Subcommand, ValueEnum};
use path_absolutize::Absolutize;

use crate::{
    interpreter::{
        builtins::IoStreams,
        memfs::{self, Entry, EntryType},
    },
    Shell,
};

pub fn memfs(shell: &mut Shell, args: Vec<String>, io_streams: &mut IoStreams) -> Result<()> {
    let args = Args::try_parse_from(args)?;
    let command = args.command.unwrap_or_default();

    match command {
        Command::Status => status(shell, io_streams),
        Command::Show(show) => diff(shell, io_streams, show),
    }
}

#[derive(Debug, Parser)]
struct Args {
    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Debug, Subcommand, Default)]
enum Command {
    Show(Show),
    #[default]
    Status,
}

#[derive(Debug, ClapArgs)]
struct Show {
    path: PathBuf,

    #[arg(short, long, value_name = "WHEN", default_value_t = Color::Auto)]
    color: Color,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum Color {
    Always,
    Auto,
    Never,
}

fn diff(shell: &mut Shell, io_streams: &mut IoStreams, args: Show) -> Result<()> {
    enum DiffType {
        Addition,
        Modification,
        Removal,
    }
    let (additions, modifications, removals) = get_status(shell);
    let path = std::env::current_dir()
        .map(|dir| Cow::Owned(args.path.absolutize_from(dir).unwrap().to_path_buf()))
        .unwrap_or(Cow::Borrowed(&args.path));
    let Some((path, diff_ty)) = additions
        .into_iter()
        .map(|add| (add, DiffType::Addition))
        .chain(
            modifications
                .into_iter()
                .map(|modification| (modification, DiffType::Modification)),
        )
        .chain(
            removals
                .into_iter()
                .map(|(removal, _)| (removal, DiffType::Removal)),
        )
        .find(|(p, _)| path.as_path() == p)
    else {
        bail!("no changes to {}", path.display());
    };

    match diff_ty {
        DiffType::Modification | DiffType::Addition => {
            let Entry::File(file) = shell.env.mem_fs.entry(&path).unwrap() else {
                bail!("{} is a directory", path.display());
            };
            let borrow = file.borrow();
            let contents = match std::str::from_utf8(borrow.data()) {
                Ok(s) => s,
                Err(err) => bail!("error reading {}, {err}", path.display()),
            };
            let real = if matches!(diff_ty, DiffType::Modification) {
                std::fs::read_to_string(&path).context("file contains invalid UTF-8")?
            } else {
                String::new()
            };
            let patch = diffy::create_patch(&real, contents);
            show_patch(&args, io_streams, patch);
        }
        DiffType::Removal => {
            let real = std::fs::read_to_string(&path).context("error reading disk file")?;
            let patch = diffy::create_patch(&real, "");
            show_patch(&args, io_streams, patch);
        }
    }

    Ok(())
}

fn show_patch(args: &Show, io_streams: &mut IoStreams, patch: diffy::Patch<'_, str>) {
    match args.color {
        Color::Always => {
            let f = diffy::PatchFormatter::new().with_color();
            write!(io_streams.stdout, "{}", f.fmt_patch(&patch)).expect("write to stdout failed!");
        }
        Color::Auto if io_streams.to_tty => {
            let f = diffy::PatchFormatter::new().with_color();
            write!(io_streams.stdout, "{}", f.fmt_patch(&patch)).expect("write to stdout failed!");
        }
        Color::Never | Color::Auto => {
            write!(io_streams.stdout, "{patch}").expect("write to stdout failed!");
        }
    }
}

fn get_status(shell: &mut Shell) -> (Vec<PathBuf>, Vec<PathBuf>, Vec<(PathBuf, EntryType)>) {
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

fn status(shell: &mut Shell, io_streams: &mut IoStreams) -> Result<()> {
    let (additions, modifications, removals) = get_status(shell);
    if !additions.is_empty() {
        writeln!(io_streams.stdout, "Created entries:").expect("write to stdout failed!");
        for addition in additions {
            let ty = shell.env.mem_fs.entry(&addition).unwrap().ty();
            let addition = relative_to_cwd(&addition);
            writeln!(
                io_streams.stdout,
                "    + {}{}",
                addition.display(),
                matches!(ty, EntryType::Directory)
                    .then_some("/")
                    .unwrap_or("")
            )
            .expect("write to stdout failed!");
        }
        if !modifications.is_empty() || !removals.is_empty() {
            writeln!(io_streams.stdout).expect("write to stdout failed!");
        }
    }
    if !modifications.is_empty() {
        writeln!(io_streams.stdout, "Modified entries:").expect("write to stdout failed!");
        for modification in modifications {
            let ty = shell.env.mem_fs.entry(&modification).unwrap().ty();
            let modification = relative_to_cwd(&modification);
            writeln!(
                io_streams.stdout,
                "    ~ {}{}",
                modification.display(),
                matches!(ty, EntryType::Directory)
                    .then_some("/")
                    .unwrap_or("")
            )
            .expect("write to stdout failed!");
        }
        if !removals.is_empty() {
            writeln!(io_streams.stdout).expect("write to stdout failed!");
        }
    }
    if !removals.is_empty() {
        writeln!(io_streams.stdout, "Removed entries:").expect("write to stdout failed!");
        for (removal, ty) in removals {
            let removal = relative_to_cwd(&removal);
            writeln!(
                io_streams.stdout,
                "    - {}{}",
                removal.display(),
                matches!(ty, EntryType::Directory)
                    .then_some("/")
                    .unwrap_or("")
            )
            .expect("write to stdout failed!");
        }
    }

    Ok(())
}

impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Color::Always => write!(f, "always"),
            Color::Auto => write!(f, "auto"),
            Color::Never => write!(f, "never"),
        }
    }
}

fn relative_to_cwd(path: &Path) -> Cow<'_, Path> {
    std::env::current_dir()
        .ok()
        .and_then(|cwd| pathdiff::diff_paths(path, cwd).map(Cow::Owned))
        .unwrap_or(Cow::Borrowed(path))
}
