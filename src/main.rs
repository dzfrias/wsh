mod cli;

use std::{fs, path::Path};

use anyhow::{Context, Result};
use clap::Parser as CliParser;
use reedline::{DefaultPrompt, FileBackedHistory, Reedline, Signal};
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
    let mut shell = Shell::new();
    let home_dir = dirs::home_dir();
    let mut line_editor = Reedline::create();
    if let Some(home) = &home_dir {
        let hist = FileBackedHistory::with_file(100, home.join(".wsi_history"))?;
        line_editor = line_editor.with_history(Box::new(hist));
    }
    let rc_file = home_dir.as_ref().map(|home| home.join(".wsirc"));
    if let Some(rc_file) = rc_file {
        run_file(&mut shell, rc_file)?;
    }

    loop {
        let prompt = DefaultPrompt::default();
        let sig = line_editor.read_line(&prompt);
        match sig {
            Ok(Signal::Success(input)) => {
                if let Ok(Some(result)) = shell.run(&input, "<prompt>") {
                    println!("{result}");
                }
            }
            Ok(Signal::CtrlD) => break,
            Ok(Signal::CtrlC) => continue,
            Err(e) => todo!("{e:?}"),
        }

        println!();
    }

    Ok(())
}
