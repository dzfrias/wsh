mod cli;

use std::{fs, path::Path};

use anyhow::{bail, Context, Result};
use ariadne::{Color, Label, Report, ReportKind, Source};
use clap::Parser as CliParser;
use shwasi_lang::{Interpreter, Lexer, ParseError, Parser};
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

    // Catch ctrlc. This will make sure that shwasi does not exit when ctrlc is pressed.
    std::thread::spawn(|| ctrlc::set_handler(|| {}));

    let args = Cli::parse();
    if let Some(input) = args.input {
        let mut interpreter = Interpreter::new();
        run_file(&mut interpreter, input)?;
    } else {
        start_repl()?;
    }

    Ok(())
}

fn run_file(interpreter: &mut Interpreter, input: impl AsRef<Path>) -> Result<()> {
    let file_input = fs::read_to_string(&input).context("error reading input file")?;
    let tokens = Lexer::new(&file_input).lex();
    let ast = match Parser::new(&tokens).parse() {
        Ok(ast) => ast,
        Err(err) => {
            print_parse_error(
                err,
                &file_input,
                Some(&input.as_ref().as_os_str().to_string_lossy()),
            );
            bail!("error parsing input");
        }
    };
    interpreter.run(ast)?;
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
    let mut interpreter = Interpreter::new();
    if let Some(rc_file) = rc_file {
        run_file(&mut interpreter, rc_file)?;
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
                    let tokens = Lexer::new(&input).lex();
                    let ast = match Parser::new(&tokens).parse() {
                        Ok(ast) => ast,
                        Err(err) => {
                            print_parse_error(err, &input, None);
                            break 'inp;
                        }
                    };
                    match interpreter.run(ast) {
                        Ok(Some(result)) => println!("{result}"),
                        Ok(None) => break 'inp,
                        Err(err) => println!("shwasi: {err}"),
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

fn print_parse_error(mut err: ParseError, input: &str, name: Option<&str>) {
    let a = Color::Blue;
    let b = Color::Green;
    let name = name.unwrap_or("prompt");
    err.range.start = err.range.start.min(input.len() - 1);
    err.range.end = err.range.end.min(input.len());

    Report::build(ReportKind::Error, name, err.range.start)
        .with_message(err.kind.to_string())
        .with_label(
            Label::new((name, err.range))
                .with_message("error happened here")
                .with_color(a),
        )
        .with_labels(err.labels.into_iter().map(|label| {
            Label::new((name, label.range))
                .with_message(label.message)
                .with_color(b)
        }))
        .finish()
        .eprint((name, Source::from(input)))
        .unwrap();
}
