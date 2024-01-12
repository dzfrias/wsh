mod cli;

use std::{fs, path::Path};

use anyhow::{Context, Result};
use clap::Parser as CliParser;
use shwasi_lang::Shell;
use tracing_subscriber::{filter::LevelFilter, fmt, prelude::*, EnvFilter};

use crate::cli::Cli;

#[cfg(feature = "dhat-heap")]
#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

fn main() -> Result<()> {
    // Heap profiler setup
    #[cfg(feature = "dhat-heap")]
    let _profiler = dhat::Profiler::new_heap();

    // Logging setup
    let fmt_layer = fmt::layer();
    let filter_layer = EnvFilter::builder()
        .with_default_directive(LevelFilter::WARN.into())
        .from_env()
        .context("error reading logging directives")?;
    tracing_subscriber::registry()
        .with(filter_layer)
        .with(fmt_layer)
        .init();

    let args = Cli::parse();
    if let Some(input) = args.input {
        let mut interpreter = Shell::new();
        run_file(&mut interpreter, input)?;
    } else {
        start_repl()?;
    }

    Ok(())
}

fn run_file(interpreter: &mut Shell, input: impl AsRef<Path>) -> Result<()> {
    let contents = fs::read_to_string(&input).context("error reading input file")?;
    // TODO: handle
    let _ = interpreter.run(
        &contents,
        &input.as_ref().file_name().unwrap().to_string_lossy(),
    );
    Ok(())
}

fn start_repl() -> Result<()> {
    const PROMPT: &str = "$ ";

    let home_dir = dirs::home_dir();
    let rc_file = home_dir.as_ref().map(|home| home.join(".wsirc"));
    let mut rl = rustyline::DefaultEditor::new()?;
    if let Some(home) = &home_dir {
        let _ = rl.load_history(&home.join(".wsi_history"));
    }
    let mut shell = Shell::new();
    if let Some(rc_file) = rc_file {
        run_file(&mut shell, rc_file)?;
    }

    let mut dir = std::env::current_dir().context("error getting current directory")?;
    'main: loop {
        if let Ok(new_dir) = std::env::current_dir() {
            dir = new_dir;
        } else {
            eprintln!("WARNING: current directory could not be read");
        }
        let current_dir = dir.file_stem().unwrap().to_string_lossy();
        let input = rl.readline(&format!("{current_dir} {PROMPT}"));

        'inp: {
            match input {
                Ok(input) => {
                    rl.add_history_entry(&input)?;
                    match shell.run(&input, "<prompt>") {
                        Ok(Some(result)) => println!("{result}"),
                        Ok(None) => break 'inp,
                        // Shell errors are handled by the shell itself, so we don't need to do
                        // anything here (in the REPL).
                        Err(_) => {}
                    }
                }
                Err(rustyline::error::ReadlineError::Eof) => {
                    break 'main;
                }
                Err(rustyline::error::ReadlineError::Interrupted) => {
                    continue 'main;
                }
                Err(e) => todo!("{e:?}"),
            }
        }

        println!();
    }

    if let Some(home) = &home_dir {
        let _ = rl.save_history(&home.join(".wsi_history"));
    }
    Ok(())
}
