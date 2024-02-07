mod cli;
mod file_suggest;

use std::{
    fs, io,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result};
use clap::Parser as CliParser;
use reedline::{
    ColumnarMenu, DefaultPrompt, Emacs, FileBackedHistory, KeyCode, KeyModifiers, Reedline,
    ReedlineEvent, ReedlineMenu, Signal,
};
use tracing_subscriber::{filter::LevelFilter, fmt, prelude::*, EnvFilter};
use wsh_cmp::completer::Completer;
use wsh_lang::{
    shell_v2::Shell,
    v2::{Source, SourceError},
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
        let mut shell = Shell::new();
        run_file(input, &mut shell)?;
        return Ok(());
    }
    run_repl()?;

    Ok(())
}

fn run_file(path: impl AsRef<Path>, shell: &mut Shell) -> Result<()> {
    let path = path.as_ref();
    let contents = fs::read_to_string(path).context("error reading input file")?;
    let name = path.file_stem().unwrap().to_string_lossy();
    let source = Source::new(&name, contents);
    if let Err(err) = shell.run(&source) {
        err.fmt_on(&source, io::stderr())
            .context("problem writing error to stderr")?;
    }
    Ok(())
}

fn run_repl() -> Result<()> {
    println!("Welcome to wsh, the WebAssembly shell!\n");
    let mut shell = Shell::new();
    let home_dir = dirs::home_dir();
    let mut line_editor = line_editor(home_dir.as_ref().map(|home| home.join(".wsi_history")))?;
    if let Some(rc_file) = home_dir.as_ref().map(|home| home.join(".wsirc")) {
        if rc_file.try_exists().context("error finding .wsirc")? {
            run_file(rc_file, &mut shell)?;
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
                    err.fmt_on(&source, io::stderr())
                        .context("problem writing error to stderr")?;
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
