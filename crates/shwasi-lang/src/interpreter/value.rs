use std::fmt;

use smol_str::SmolStr;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(SmolStr),
    Null,
}

impl Value {
    pub fn is_null(&self) -> bool {
        matches!(self, Value::Null)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Null => write!(f, "null"),
        }
    }
}
