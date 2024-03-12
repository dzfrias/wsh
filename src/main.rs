mod cli;
mod file_suggest;

use std::{
    borrow::Cow,
    cell::Cell,
    fs,
    io::{self, IsTerminal},
    path::{Path, PathBuf},
};

use anyhow::{Context, Result};
use clap::Parser as CliParser;
use reedline::{
    ColumnarMenu, Emacs, FileBackedHistory, KeyCode, KeyModifiers, Prompt, Reedline, ReedlineEvent,
    ReedlineMenu, Signal,
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
        let mut shell = Shell::new().context("error initializing shell")?;
        run_file(input, &mut shell)?;
        return Ok(());
    }
    // Setting this signal handler will make sure that any time we CTRL-C a subprocess, we run
    // in the shell, it'll only quit that subprocess (not this one)
    ctrlc::set_handler(|| {}).context("problem setting ctrl-c signal handler")?;
    run_repl()?;

    Ok(())
}

fn run_file(path: impl AsRef<Path>, shell: &mut Shell) -> Result<()> {
    let path = path.as_ref();
    let contents = fs::read_to_string(path).context("error reading input file")?;
    let name = path.file_stem().unwrap().to_string_lossy();
    let source = Source::new(&name, contents);
    if let Err(err) = shell.run(&source) {
        err.fmt_on(&source, io::stderr(), io::stderr().is_terminal())
            .context("problem writing error to stderr")?;
    }
    Ok(())
}

fn run_repl() -> Result<()> {
    println!("Welcome to wsh, the WebAssembly shell!\n");
    let mut shell = Shell::new().context("error initializing shell")?;
    let home_dir = dirs::home_dir();
    let mut line_editor = line_editor(home_dir.as_ref().map(|home| home.join(".wsi_history")))?;
    if let Some(rc_file) = home_dir.as_ref().map(|home| home.join(".wsirc")) {
        if rc_file.try_exists().context("error finding .wsirc")? {
            run_file(rc_file, &mut shell)?;
        }
    }

    let is_terminal = io::stderr().is_terminal();
    let prompt = WshPrompt {
        cwd: current_dir_string()
            .context("invalid current directory!")?
            .into(),
    };
    loop {
        let sig = line_editor.read_line(&prompt);
        match sig {
            Ok(Signal::Success(input)) => {
                let source = Source::new("<prompt>", input);
                if let Err(err) = shell.run(&source) {
                    err.fmt_on(&source, io::stderr(), is_terminal)
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

fn current_dir_string() -> Option<String> {
    std::env::current_dir()
        .ok()
        .map(|dir| dir.file_stem().unwrap().to_string_lossy().to_string())
}

struct WshPrompt {
    cwd: Cell<String>,
}

impl Prompt for WshPrompt {
    fn render_prompt_left(&self) -> Cow<str> {
        if let Some(cwd) = current_dir_string() {
            self.cwd.set(cwd)
        }
        let cwd = self.cwd.take();
        let prompt = format!("{cwd} ");
        self.cwd.set(cwd);
        Cow::Owned(prompt)
    }

    fn render_prompt_right(&self) -> Cow<str> {
        Cow::Borrowed("")
    }

    fn render_prompt_indicator(&self, _prompt_mode: reedline::PromptEditMode) -> Cow<str> {
        Cow::Borrowed("$ ")
    }

    fn render_prompt_multiline_indicator(&self) -> Cow<str> {
        Cow::Borrowed("> ")
    }

    fn render_prompt_history_search_indicator(
        &self,
        history_search: reedline::PromptHistorySearch,
    ) -> Cow<str> {
        let prefix = match history_search.status {
            reedline::PromptHistorySearchStatus::Passing => "",
            reedline::PromptHistorySearchStatus::Failing => "failing ",
        };

        Cow::Owned(format!(
            "({}reverse-search: {}) ",
            prefix, history_search.term
        ))
    }

    fn get_prompt_color(&self) -> reedline::Color {
        reedline::Color::White
    }

    fn get_indicator_color(&self) -> reedline::Color {
        reedline::Color::White
    }
}
