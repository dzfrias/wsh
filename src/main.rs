mod cli;

use std::fs;

use anyhow::{Context, Result};
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
        let input = fs::read_to_string(input).context("error reading input file")?;
        let tokens = Lexer::new(&input).lex();
        let ast = Parser::new(&tokens)
            .parse()
            .context("error parsing input")?;
        Interpreter::new().run(ast)?;
    } else {
        start_repl()?;
    }

    Ok(())
}

fn start_repl() -> Result<()> {
    const PROMPT: &str = "$ ";

    let mut rl = rustyline::DefaultEditor::new()?;
    let mut interpreter = Interpreter::new();

    'main: loop {
        let input = rl.readline(PROMPT);

        'inp: {
            match input {
                Ok(input) => {
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

fn print_parse_error(mut err: ParseError, input: &str, name: Option<&str>) {
    let a = Color::Blue;
    let b = Color::Green;
    let name = name.unwrap_or("prompt");
    err.offset = err.offset.min(input.len() - 1);

    Report::build(ReportKind::Error, name, err.offset)
        .with_message(err.kind.to_string())
        .with_label(
            Label::new((name, err.offset..err.offset + 1))
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
