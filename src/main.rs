mod cli;
mod executor;

use std::fs;

use anyhow::{Context, Result};
use clap::Parser as CliParser;
use shwasi_lang::{Interpreter, Lexer, Parser};
use tracing_subscriber::{filter::LevelFilter, fmt, prelude::*, EnvFilter};

use crate::{cli::Cli, executor::Executor};

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

    // Catch ctrlc. This will make sure that shwasi does not exit when ctrlc is pressed.
    std::thread::spawn(|| ctrlc::set_handler(|| {}));

    let args = Cli::parse();
    if let Some(input) = args.input {
        let input = fs::read_to_string(input).context("error reading input file")?;
        let tokens = Lexer::new(&input).lex();
        let ast = Parser::new(&tokens)
            .parse()
            .context("error parsing input")?;
        let executor = Executor::new();
        Interpreter::new(Box::new(executor)).run(ast)?;
    } else {
        start_repl()?;
    }

    Ok(())
}

fn start_repl() -> Result<()> {
    const PROMPT: &str = "$ ";

    let mut rl = rustyline::DefaultEditor::new()?;
    let executor = Executor::new();
    let mut interpreter = Interpreter::new(Box::new(executor));

    'main: loop {
        let input = rl.readline(PROMPT);

        'inp: {
            match input {
                Ok(input) => {
                    let tokens = Lexer::new(&input).lex();
                    let ast = match Parser::new(&tokens).parse() {
                        Ok(ast) => ast,
                        Err(err) => {
                            println!("{err}");
                            break 'inp;
                        }
                    };
                    match interpreter.run(ast) {
                        Ok(Some(result)) => println!("{result}"),
                        Ok(None) => break 'inp,
                        Err(err) => println!("{err}"),
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

    Ok(())
}
