use std::{env, io, path::PathBuf, process::Command};

use shwasi_lang::{RuntimeError, RuntimeResult, Value};

#[derive(Debug, Default)]
pub struct Executor {}

impl Executor {
    pub fn new() -> Self {
        Self {}
    }
}

impl shwasi_lang::Executor for Executor {
    fn run(&mut self, name: &str, args: &[String]) -> RuntimeResult<Value> {
        match name {
            "cd" => cd(args),
            _ => Command::new(name)
                .args(args)
                .spawn()
                .map_err(RuntimeError::CommandFailed)?
                .wait()
                .map_err(RuntimeError::CommandFailed)
                .map(|_| Value::Null),
        }
    }
}

fn cd(args: &[String]) -> RuntimeResult<Value> {
    let path = args
        .get(0)
        .map(PathBuf::from)
        .map_or_else(dirs::home_dir, Some)
        .ok_or_else(|| {
            RuntimeError::CommandFailed(io::Error::new(
                io::ErrorKind::Other,
                "home directory not found",
            ))
        })?;

    env::set_current_dir(path).map_err(RuntimeError::CommandFailed)?;

    Ok(Value::Null)
}
