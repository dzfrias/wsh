use smol_str::SmolStr;

use crate::parser::{Symbol, SymbolTable};

#[derive(Debug, Clone)]
pub struct Ast {
    statements: Vec<Stmt>,
    #[allow(dead_code)]
    symbols: SymbolTable,
}

impl Ast {
    pub fn new(statements: Vec<Stmt>, symbols: SymbolTable) -> Self {
        Self {
            statements,
            symbols,
        }
    }
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

#[derive(Debug, Clone)]
pub enum Stmt {
    Command(Command),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub struct Command {
    pub name: SmolStr,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Ident(Symbol),
    String(SmolStr),
    Number(f64),

    Infix(InfixExpr),
    Prefix(PrefixExpr),
}

#[derive(Debug, Clone)]
pub struct InfixExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: InfixOp,
}

#[derive(Debug, Clone, Copy)]
pub enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
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
