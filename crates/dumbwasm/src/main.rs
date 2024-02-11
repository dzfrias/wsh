mod cli;

use std::{
    fs::{self, File},
    io::Write,
};

use anyhow::{bail, Context, Result};
use clap::Parser;
use console::StyledObject;
use dumbwasm::{parse, WriteError};

use crate::cli::{Cli, Color};

fn main() -> Result<()> {
    let args = Cli::parse();
    let input = fs::read_to_string(&args.file).context("error reading input file to string")?;
    let stem = args
        .file
        .file_stem()
        .expect("should have file stem if it was read successfully");

    let out = match parse(&input) {
        Ok(buf) => buf,
        Err(err) => {
            print_err(&input, err, &args);
            bail!("parse error");
        }
    };
    let out_file_path = args
        .out
        .unwrap_or_else(|| format!("{}.wasm", stem.to_string_lossy()).into());
    let mut out_file = File::create(out_file_path).context("error creating out file")?;
    out_file
        .write_all(&out)
        .context("error writing buffer to out file")?;

    Ok(())
}

fn print_line(line: &str, num: usize, args: &Cli) {
    eprintln!("{} {line}", style(format!("{}| ", num), args).blue());
}

fn style<D>(input: D, args: &Cli) -> StyledObject<D> {
    match args.color {
        Color::Never => console::style(input).force_styling(false),
        Color::Auto => console::style(input),
        Color::Always => console::style(input).force_styling(true),
    }
}

fn print_err(src: &str, err: WriteError, args: &Cli) {
    eprintln!("{}: {}\n", style("error", args).red().bold(), err.kind);

    let mut current_pos = 0;
    let mut after_print = 0;
    for (i, line) in src.lines().enumerate() {
        if after_print == 0 && current_pos + line.len() < err.span.start {
            current_pos += line.len() + 1;

            if current_pos + line.len() >= err.span.start {
                print_line(line, i + 1, args);
            }
            continue;
        }

        if after_print > 0 && after_print <= 2 {
            print_line(line, i + 1, args);
            after_print += 1;
            continue;
        } else if after_print > 2 {
            break;
        }

        print_line(line, i + 1, args);
        let padding = (err.span.start + 2) - current_pos;
        let underlines = "^".repeat(err.span.len());
        eprintln!(
            " {x}{:1$}{underlines}",
            "",
            padding,
            x = style("•", args).red().bold(),
            underlines = style(underlines, args).red().bold(),
        );
        after_print = 1;
    }

    if let Some(help) = err.help {
        eprintln!("\n{}: {help}", style("help", args).cyan().bold());
    }
}
