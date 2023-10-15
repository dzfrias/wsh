mod cli;

use std::{
    fs::{self, File},
    io::Write,
    process,
};

use anyhow::{Context, Result};
use clap::Parser;
use dumbwasm::{parse, WriteError};

use crate::cli::Cli;

fn main() -> Result<()> {
    let args = Cli::parse();
    let input = fs::read_to_string(&args.input).context("error reading input file to string")?;
    let stem = args.input.file_stem().expect("should have file stem");

    let out = match parse(&input) {
        Ok(buf) => buf,
        Err(err) => {
            print_err(&input, err);
            process::exit(1);
        }
    };
    let mut out_file = File::create(&format!("{}.wasm", stem.to_string_lossy()))
        .context("error creating out file")?;
    out_file
        .write_all(&out)
        .context("error writing buffer to out file")?;

    Ok(())
}

fn print_line(line: &str, num: usize) {
    eprintln!("{} {line}", console::style(format!("{}| ", num)).blue());
}

fn print_err(src: &str, err: WriteError) {
    eprintln!("{}: {}\n", console::style("error").red().bold(), err.kind);

    let mut current_pos = 0;
    let mut after_print = 0;
    for (i, line) in src.lines().enumerate() {
        if after_print == 0 && current_pos + line.len() < err.span.start {
            current_pos += line.len() + 1;

            if current_pos + line.len() >= err.span.start {
                print_line(line, i + 1);
            }
            continue;
        }

        if after_print > 0 && after_print <= 2 {
            print_line(line, i + 1);
            after_print += 1;
            continue;
        } else if after_print > 2 {
            break;
        }

        print_line(line, i + 1);
        let padding = (err.span.start + 2) - current_pos;
        let underlines = std::iter::repeat('^')
            .take(err.span.len())
            .collect::<String>();
        eprintln!(
            " {x}{:1$}{underlines}",
            "",
            padding,
            x = console::style("•").red().bold(),
            underlines = console::style(underlines).red().bold(),
        );
        after_print = 1;
    }

    if let Some(help) = err.help {
        eprintln!("\n{}: {help}", console::style("help").cyan().bold())
    }
}
