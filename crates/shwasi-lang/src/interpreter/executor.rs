use crate::interpreter::{error::RuntimeResult, value::Value};

/// Executor is an interface for executing commands.
///
/// This trait is made to be object safe.
pub trait Executor {
    /// Execute a command.
    fn run(&mut self, name: &str, args: &[String]) -> RuntimeResult<Value>;
}
