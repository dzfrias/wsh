use std::fmt;

use smol_str::SmolStr;

use crate::Ident;

#[derive(Debug, Clone)]
pub struct Ast {
    pub defs: Vec<Def>,
    pub statements: Vec<Stmt>,
}

impl Ast {
    pub fn new(statements: Vec<Stmt>, defs: Vec<Def>) -> Self {
        Self { statements, defs }
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Pipeline(Pipeline),
    Expr(Expr),
    AliasAssign(AliasAssign),
    Assign(Assign),
    Export(Export),
    If(If),
    While(While),
    Break,
    Continue,
    Return,
}

#[derive(Debug, Clone)]
pub struct Def {
    pub name: Ident,
    pub body: Vec<Stmt>,
    pub args: DefArgs,
}

#[derive(Debug, Clone)]
pub struct DefArgs(pub Vec<DefArg>);

#[derive(Debug, Clone)]
pub enum DefArg {
    Positional {
        name: Ident,
    },
    Named {
        name: Ident,
        alias: Option<char>,
        default: SmolStr,
    },
    Boolean {
        name: Ident,
        alias: char,
    },
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Expr,
    pub body: Vec<Stmt>,
    pub else_: Option<Vec<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct While {
    pub condition: Expr,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub name: Ident,
    pub expr: Expr,
    pub global: bool,
}

#[derive(Debug, Clone)]
pub struct Export {
    pub name: Ident,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct AliasAssign {
    pub name: SmolStr,
    pub pipeline: Pipeline,
}

#[derive(Debug, Clone)]
pub struct Pipeline {
    pub env: Vec<EnvSet>,
    pub commands: Vec<Command>,
    pub write: Option<Box<PipelineEnd>>,
}

#[derive(Debug, Clone)]
pub struct EnvSet {
    pub name: Ident,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct PipelineEnd {
    pub kind: PipelineEndKind,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub enum PipelineEndKind {
    Append,
    Write,
}

#[derive(Debug, Clone, Default)]
pub struct Command {
    pub name: SmolStr,
    pub args: Vec<Expr>,
    pub merge_stderr: bool,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Ident(Ident),
    Env(Ident),
    String(SmolStr),
    Number(f64),
    Pipeline(Pipeline),
    Bool(bool),
    LastStatus,
    Tilde,

    Infix(InfixExpr),
    Prefix(PrefixExpr),
}

#[derive(Debug, Clone)]
pub struct InfixExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: InfixOp,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Ne,
}

#[derive(Debug, Clone)]
pub struct PrefixExpr {
    pub expr: Box<Expr>,
    pub op: PrefixOp,
}

#[derive(Debug, Clone, Copy)]
pub enum PrefixOp {
    Neg,
    Sign,
    Bang,
}

impl IntoIterator for Ast {
    type Item = Stmt;
    type IntoIter = AstIter;

    fn into_iter(self) -> Self::IntoIter {
        AstIter { ast: self, pos: 0 }
    }
}

#[derive(Debug)]
pub struct AstIter {
    ast: Ast,
    pos: usize,
}

impl Iterator for AstIter {
    type Item = Stmt;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= self.ast.statements.len() {
            return None;
        }

        let stmt = std::mem::replace(
            &mut self.ast.statements[self.pos],
            Stmt::Expr(Expr::Number(0.0)),
        );
        self.pos += 1;
        Some(stmt)
    }
}

impl fmt::Display for InfixOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InfixOp::Add => write!(f, "+"),
            InfixOp::Sub => write!(f, "-"),
            InfixOp::Mul => write!(f, "*"),
            InfixOp::Div => write!(f, "/"),
            InfixOp::Lt => write!(f, "<"),
            InfixOp::Gt => write!(f, ">"),
            InfixOp::Le => write!(f, "<="),
            InfixOp::Ge => write!(f, ">="),
            InfixOp::Eq => write!(f, "=="),
            InfixOp::Ne => write!(f, "!="),
        }
    }
}

impl fmt::Display for PrefixOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrefixOp::Neg => write!(f, "-"),
            PrefixOp::Sign => write!(f, "+"),
            PrefixOp::Bang => write!(f, "!"),
        }
    }
}
