mod cli;
mod file_suggest;

use std::{fs, io, path::PathBuf};

use anyhow::{Context, Result};
use clap::Parser as CliParser;
use reedline::{
    ColumnarMenu, DefaultPrompt, Emacs, FileBackedHistory, KeyCode, KeyModifiers, Reedline,
    ReedlineEvent, ReedlineMenu, Signal,
};
use tracing_subscriber::{filter::LevelFilter, fmt, prelude::*, EnvFilter};
use wsh_cmp::completer::Completer;
use wsh_lang::{
    shell_v2::{error::ErrorKind, Shell},
    v2::Source,
};

use crate::cli::Cli;
use file_suggest::FileSuggest;

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
        let contents = fs::read_to_string(&input).context("error reading input file")?;
        let mut shell = Shell::new();
        let source = Source::new(
            input.file_stem().unwrap().to_string_lossy().to_string(),
            contents,
        );
        if let Err(err) = shell.run(&source) {
            if let ErrorKind::ParseError(parse_error) = err.kind() {
                source.fmt_error(parse_error, io::stderr())?;
            } else {
                eprintln!("wsh: {err}");
            }
        }
        return Ok(());
    }
    start_repl()?;

    Ok(())
}

fn start_repl() -> Result<()> {
    println!("Welcome to wsh, the WebAssembly shell!\n");
    let mut shell = Shell::new();
    let home_dir = dirs::home_dir();
    let mut line_editor = line_editor(home_dir.as_ref().map(|home| home.join(".wsi_history")))?;
    if let Some(rc_file) = home_dir.as_ref().map(|home| home.join(".wsirc")) {
        if rc_file.try_exists().context("error finding .wsirc")? {
            let contents = fs::read_to_string(&rc_file).context("error reading rc file")?;
            shell.run(&Source::new(
                rc_file.to_string_lossy().to_string(),
                contents,
            ))?;
        }
    }

    loop {
        let prompt = DefaultPrompt::new(
            reedline::DefaultPromptSegment::WorkingDirectory,
            reedline::DefaultPromptSegment::Empty,
        );
        let sig = line_editor.read_line(&prompt);
        match sig {
            Ok(Signal::Success(input)) => {
                let source = Source::new("<prompt>", input);
                if let Err(err) = shell.run(&source) {
                    if let ErrorKind::ParseError(parse_error) = err.kind() {
                        source.fmt_error(parse_error, io::stderr())?;
                    } else {
                        eprintln!("wsh: {err}");
                    }
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

fn line_editor(history_file: Option<PathBuf>) -> Result<Reedline> {
    let completion_menu = Box::new(ColumnarMenu::default().with_name("completion_menu"));
    let mut keybindings = reedline::default_emacs_keybindings();
    keybindings.add_binding(
        KeyModifiers::NONE,
        KeyCode::Tab,
        ReedlineEvent::UntilFound(vec![
            ReedlineEvent::Menu("completion_menu".to_string()),
            ReedlineEvent::MenuNext,
        ]),
    );
    let edit_mode = Box::new(Emacs::new(keybindings));

    let completer = Completer::new(Box::new(FileSuggest));
    let mut line_editor = Reedline::create()
        .with_completer(Box::new(completer))
        .with_menu(ReedlineMenu::EngineCompleter(completion_menu))
        .with_edit_mode(edit_mode);
    if let Some(file) = history_file {
        let hist = FileBackedHistory::with_file(100, file)?;
        line_editor = line_editor.with_history(Box::new(hist));
    }

    Ok(line_editor)
}
