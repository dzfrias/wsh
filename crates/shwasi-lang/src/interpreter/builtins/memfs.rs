use std::{
    borrow::Cow,
    fmt, io,
    path::{Path, PathBuf},
};

use anyhow::{bail, Context, Result};
use clap::{Args as ClapArgs, Parser, Subcommand, ValueEnum};
use filedescriptor::{AsRawFileDescriptor, IntoRawFileDescriptor};

use crate::{
    interpreter::memfs::{self, Entry},
    Shell,
};

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

pub fn memfs(
    shell: &mut Shell,
    args: Vec<String>,
    stdout: &mut (impl io::Write + AsRawFileDescriptor),
    _stdin: Option<impl io::Read + IntoRawFileDescriptor>,
) -> Result<()> {
    let args = Args::try_parse_from(args)?;
    let command = args.command.unwrap_or_default();

    match command {
        Command::Status => status(shell, stdout),
        Command::Show(show) => diff(shell, stdout, show),
    }
}

fn diff(
    shell: &mut Shell,
    stdout: &mut (impl io::Write + AsRawFileDescriptor),
    args: Show,
) -> Result<()> {
    enum DiffType {
        Addition,
        Modification,
        Removal,
    }
    let (additions, modifications, removals) = get_status(shell);
    let path = std::env::current_dir()
        .map(|dir| Cow::Owned(dir.join(&args.path)))
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
                .map(|removal| (removal, DiffType::Removal)),
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
            show_patch(&args, stdout, patch);
        }
        DiffType::Removal => {
            let real = std::fs::read_to_string(&path).context("error reading disk file")?;
            let patch = diffy::create_patch(&real, "");
            show_patch(&args, stdout, patch);
        }
    }

    Ok(())
}

fn show_patch(
    args: &Show,
    stdout: &mut (impl io::Write + AsRawFileDescriptor),
    patch: diffy::Patch<'_, str>,
) {
    match args.color {
        // TODO: check if stdout stream is main needs more information
        Color::Always | Color::Auto => {
            let f = diffy::PatchFormatter::new().with_color();
            write!(stdout, "{}", f.fmt_patch(&patch)).expect("write to stdout failed!");
        }
        Color::Never => {
            write!(stdout, "{patch}").expect("write to stdout failed!");
        }
    }
}

fn get_status(shell: &mut Shell) -> (Vec<PathBuf>, Vec<PathBuf>, Vec<PathBuf>) {
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
    shell.env.mem_fs.for_each_removal(|path| {
        if !path.exists() {
            return;
        }

        removals.push(path.to_path_buf());
    });

    (additions, modifications, removals)
}

fn status(shell: &mut Shell, stdout: &mut (impl io::Write + AsRawFileDescriptor)) -> Result<()> {
    let (additions, modifications, removals) = get_status(shell);
    if !additions.is_empty() {
        writeln!(stdout, "Created entries:").expect("write to stdout failed!");
        for addition in additions {
            let addition = relative_to_cwd(&addition);
            writeln!(stdout, "    + {}", addition.display()).expect("write to stdout failed!");
        }
        if !modifications.is_empty() {
            writeln!(stdout).expect("write to stdout failed!");
        }
    }
    if !modifications.is_empty() {
        writeln!(stdout, "Modified entries:").expect("write to stdout failed!");
        for modification in modifications {
            let modification = relative_to_cwd(&modification);
            writeln!(stdout, "    ~ {}", modification.display()).expect("write to stdout failed!");
        }
        if !removals.is_empty() {
            writeln!(stdout).expect("write to stdout failed!");
        }
    }
    if !removals.is_empty() {
        writeln!(stdout, "Removed entries:").expect("write to stdout failed!");
        for removal in removals {
            let removal = relative_to_cwd(&removal);
            writeln!(stdout, "    - {}", removal.display()).expect("write to stdout failed!");
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
