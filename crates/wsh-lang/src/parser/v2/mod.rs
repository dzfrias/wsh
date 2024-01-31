pub mod ast;
pub mod error;

use std::{borrow::Cow, io};

use tracing::debug;

use self::error::*;
use crate::{
    parser::lexer::v2::{LexMode, Lexer, Token, TokenKind},
    v2::ast::{
        Ast, DataArray, DataHandle, EnvSet, NodeHandle, NodeInfo, NodeInfoKind, PipelineEnd,
        PipelineEndKind,
    },
};

/// The original text of wsh's input, along with a source name.
#[derive(Debug)]
pub struct Source {
    contents: String,
    name: Cow<'static, str>,
}

impl Source {
    /// Create a new source with the given name and text.
    pub fn new(name: impl Into<Cow<'static, str>>, contents: String) -> Self {
        Self {
            name: name.into(),
            contents,
        }
    }

    /// Get the underlying contents of the source.
    pub fn contents(&self) -> &str {
        &self.contents
    }

    /// Get the name of the source.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Format an error on this source. This will write the error message to the `writer`.
    pub fn fmt_error(&self, error: Error, writer: impl io::Write) -> io::Result<()> {
        use ariadne::{Color, Label, Report, ReportKind, Source};

        Report::build(ReportKind::Error, self.name(), error.offset())
            .with_message(error.msg())
            .with_labels(error.labels().iter().map(|label| {
                Label::new((self.name(), label.range.clone()))
                    .with_message(&label.msg)
                    .with_color(Color::Blue)
            }))
            .finish()
            .write((self.name(), Source::from(self.contents())), writer)
    }
}

/// The parser for the wsh language.
#[derive(Debug)]
pub struct Parser<'src> {
    lexer: Lexer<'src>,

    mode: LexMode,
    current: Token<'src>,
    scope: Vec<Scope>,
}

impl<'src> Parser<'src> {
    /// Create a new parser for the given source.
    pub fn new(source: &'src Source) -> Self {
        let mut parser = Self {
            lexer: Lexer::new(&source.contents),

            mode: LexMode::Normal,
            current: Token::default(),
            scope: vec![],
        };
        // Past dummy token
        parser.next_token();

        parser
    }

    /// Parse the given source code into an AST.
    pub fn parse(mut self) -> Result<Ast> {
        debug!("began parsing");

        let mut ast = Ast::new();

        let mut stmts = vec![];
        while self.current.kind() != TokenKind::Eof {
            stmts.push(self.parse_stmt(&mut ast)?);
            if self.peek().kind() != TokenKind::Eof {
                self.expect_next(TokenKind::Semicolon, "expected line break")
                    .attach(Label::new(
                        self.current.start()..self.current.end(),
                        "expected here",
                    ))?;
            }
            self.next_token();
        }
        let arr = DataArray::from_iter(&mut ast, stmts);
        // SAFETY: Root's data is a pointer to AST statements, which is provided by `arr`
        unsafe {
            ast.add(NodeInfo {
                kind: NodeInfoKind::Root,
                offset: 0,
                p1: arr.raw(),
                p2: 0,
            })
        };

        debug!("finished parsing into AST");
        Ok(ast)
    }

    fn parse_stmt(&mut self, ast: &mut Ast) -> Result<NodeHandle> {
        debug!("began parsing statement");
        let s = match self.current.kind() {
            TokenKind::Ident(_) if self.peek_strict().kind() == TokenKind::Assign => {
                self.parse_assignment(ast)?
            }
            TokenKind::While => self.parse_while(ast)?,
            TokenKind::If => self.parse_if(ast)?,
            TokenKind::Break => self.parse_break(ast)?,
            TokenKind::Continue => self.parse_continue(ast)?,
            _ => self.parse_pipeline(ast)?,
        };
        debug!("successfully parsed statement");
        Ok(s)
    }

    fn parse_pipeline(&mut self, ast: &mut Ast) -> Result<NodeHandle> {
        debug!("began parsing pipeline");

        let mut env = vec![];
        while matches!(self.current.kind(), TokenKind::EnvVar(_))
            && self.peek_strict().kind() == TokenKind::Assign
        {
            let TokenKind::EnvVar(name) = self.current.kind() else {
                unreachable!()
            };
            debug!("got env set with name: {name}");
            let name = ast.alloc_ident(name);
            self.mode = LexMode::Strict;
            self.next_token();
            self.mode = LexMode::Normal;
            self.next_token();
            let value = self.parse_expr(ast, Precedence::Lowest)?;
            env.push(EnvSet { name, value });
            debug!("parsed env set");
            self.next_token();
        }

        if let TokenKind::String(s) = self.current.kind() {
            // Special commands that get dedicated nodes in the AST
            match s {
                "export" => return self.parse_export(ast),
                _ => {}
            }
        }

        let offset = self.current.start() as u32;
        let mut cmds = vec![self.parse_cmd(ast)?];
        while matches!(self.peek().kind(), TokenKind::Pipe | TokenKind::PercentPipe) {
            self.next_token();
            self.next_token();
            cmds.push(self.parse_cmd(ast)?);
        }
        let (kind, p2) = match self.peek().kind() {
            TokenKind::Append
            | TokenKind::Write
            | TokenKind::PercentWrite
            | TokenKind::PercentAppend => {
                self.next_token();
                let kind = match self.current.kind() {
                    TokenKind::Write | TokenKind::PercentWrite => PipelineEndKind::Write,
                    TokenKind::Append | TokenKind::PercentAppend => PipelineEndKind::Append,
                    _ => unreachable!(),
                };
                self.next_token();
                let file = self.parse_expr(ast, Precedence::Lowest)?;
                let end = PipelineEnd { kind, file };
                let env_handle = DataArray::from_iter(ast, env);
                // Pointer to a tuple of data containing the end kind and the environment array
                let data = ast.serialize((end, env_handle));
                (NodeInfoKind::PipelineWithEnd, data.raw())
            }
            _ => {
                let env_handle = DataArray::from_iter(ast, env);
                (NodeInfoKind::Pipeline, env_handle.raw())
            }
        };
        let cmds_len = cmds.len();
        let data = DataArray::from_iter(ast, cmds);
        // SAFETY: Both variants of pipeliens (with end and without) are provided with their
        // necessary handles.
        let node = unsafe {
            ast.add(NodeInfo {
                kind,
                offset,
                p1: data.raw(),
                p2,
            })
        };

        debug!("finished parsing pipline with {cmds_len} commands");
        Ok(node)
    }

    fn parse_assignment(&mut self, ast: &mut Ast) -> Result<NodeHandle> {
        debug!("began parsing assignment");
        let TokenKind::Ident(name) = self.current.kind() else {
            panic!("BUG: should not call on non-ident!");
        };
        let offset = self.current.start() as u32;
        let name = ast.alloc_string(name);
        self.mode = LexMode::Strict;
        self.next_token();
        debug_assert!(matches!(self.current.kind(), TokenKind::Assign));
        self.next_token();
        let value = self.parse_expr(ast, Precedence::Lowest)?;
        self.mode = LexMode::Normal;
        // SAFETY: Assignment takes an ident handle and an expression handle, which are provided.
        let node = unsafe {
            ast.add(NodeInfo {
                kind: NodeInfoKind::Assignment,
                offset,
                p1: name.raw(),
                p2: value.raw(),
            })
        };
        debug!("successfully parsed assignment");
        Ok(node)
    }

    fn parse_export(&mut self, ast: &mut Ast) -> Result<NodeHandle> {
        debug!("began parsing export");
        let start = self.current.start() as u32;
        self.mode = LexMode::Strict;
        self.next_token();
        let TokenKind::Ident(name) = self.current.kind() else {
            return Err(Error::new(
                self.current.start(),
                "expected ident for environment variable name",
            ))
            .attach(Label::new(
                self.current.start()..self.current.end(),
                "this should be a regular identifier",
            ));
        };
        let name = ast.alloc_string(name);
        self.expect_next(TokenKind::Assign, "expected assignment token")
            .attach(Label::new(
                self.current.start()..self.current.end(),
                "you might mean: `=`",
            ))?;
        self.next_token();
        let value = self.parse_expr(ast, Precedence::Lowest)?;
        self.mode = LexMode::Normal;
        // SAFETY: Export takes an ident handle and a node handle, which have been provided.
        let node = unsafe {
            ast.add(NodeInfo {
                kind: NodeInfoKind::Export,
                offset: start,
                p1: name.raw(),
                p2: value.raw(),
            })
        };
        debug!("successfully parsed export");
        Ok(node)
    }

    fn parse_if(&mut self, ast: &mut Ast) -> Result<NodeHandle> {
        debug_assert!(matches!(self.current.kind(), TokenKind::If));
        debug!("began parsing if statement");
        let start = self.current.start();
        self.mode = LexMode::Strict;
        self.next_token();
        let cond_start = self.current.start();
        let condition = self.parse_expr(ast, Precedence::Lowest)?;
        let cond_end = self.current.end();
        self.expect_next(TokenKind::Then, "expected `then` after if condition")
            .attach(Label::new(cond_start..cond_end, "after this expression"))?;
        self.next_token();
        let body = self.parse_block(ast, true)?;
        let (kind, p2) = if self.current.kind() == TokenKind::Else {
            debug!("got `else` in if statement");
            self.next_token();
            let else_body = self.parse_block(ast, false)?;
            let data = ast.serialize((body, else_body));
            (NodeInfoKind::IfElse, data.raw())
        } else {
            (NodeInfoKind::If, body.raw())
        };
        // SAFETY: the two variants of If (with else and without) have their proper handles
        // provided.
        let node = unsafe {
            ast.add(NodeInfo {
                kind,
                offset: start as u32,
                p1: condition.raw(),
                p2,
            })
        };
        debug!("sucessfully parsed if statement");
        Ok(node)
    }

    fn parse_while(&mut self, ast: &mut Ast) -> Result<NodeHandle> {
        debug_assert!(matches!(self.current.kind(), TokenKind::While));
        debug!("began parsing while statement");
        let start = self.current.start();
        self.mode = LexMode::Strict;
        self.next_token();
        let cond_start = self.current.start();
        let condition = self.parse_expr(ast, Precedence::Lowest)?;
        let cond_end = self.current.end();
        self.expect_next(TokenKind::Do, "expected `do` after while condition")
            .attach(Label::new(cond_start..cond_end, "after this expression"))?;
        self.next_token();
        self.enter_scope(Scope::Loop);
        let body = self.parse_block(ast, false)?;
        self.pop_scope();
        // SAFETY: While takes a node handle and a pointer to an array of body statements, which
        // are provided.
        let node = unsafe {
            ast.add(NodeInfo {
                kind: NodeInfoKind::While,
                offset: start as u32,
                p1: condition.raw(),
                p2: body.raw(),
            })
        };

        debug!("successfully parsed while statement");
        Ok(node)
    }

    fn parse_block(
        &mut self,
        ast: &mut Ast,
        allow_else: bool,
    ) -> Result<DataHandle<DataArray<NodeHandle>>> {
        debug!("began parsing body");
        let mut body = vec![];
        while !matches!(
            self.current.kind(),
            TokenKind::End | TokenKind::Eof | TokenKind::Else
        ) {
            let stmt = self.parse_stmt(ast)?;
            if !matches!(
                self.peek().kind(),
                TokenKind::End | TokenKind::Eof | TokenKind::Else
            ) {
                self.expect_next(TokenKind::Semicolon, "expected line break")
                    .attach(Label::new(
                        self.current.start()..self.current.end(),
                        "expected here",
                    ))?;
            }
            body.push(stmt);
            self.next_token();
        }
        if self.current.kind() != TokenKind::End
            && !(allow_else && self.current.kind() == TokenKind::Else)
        {
            return Err(Error::new(
                self.current.start(),
                "expected `end` after block",
            ))
            .attach(Label::new(
                self.current.start()..self.current.end(),
                "expected right here",
            ));
        }
        debug!("successfully parsed body with {} statements", body.len());
        Ok(DataArray::from_iter(ast, body))
    }

    fn parse_cmd(&mut self, ast: &mut Ast) -> Result<NodeHandle> {
        debug!("began parsing command");
        let mut exprs = vec![self.parse_expr(ast, Precedence::Lowest)?];
        while !matches!(
            self.peek().kind(),
            TokenKind::Semicolon
                | TokenKind::Eof
                | TokenKind::Pipe
                | TokenKind::Write
                | TokenKind::Append
                | TokenKind::PercentPipe
                | TokenKind::PercentWrite
                | TokenKind::PercentAppend
                | TokenKind::End
                | TokenKind::Else
        ) {
            if self.peek().kind() == TokenKind::Backtick && self.in_scope(Scope::Capture) {
                break;
            }
            self.next_token();
            let expr = self.parse_expr(ast, Precedence::Lowest)?;
            exprs.push(expr);
        }
        // Merge stderr with stdout of a `%` redirect or pipeline operator was used
        let merge_stderr = matches!(
            self.peek().kind(),
            TokenKind::PercentAppend | TokenKind::PercentPipe | TokenKind::PercentWrite
        );
        debug!(
            "successfully parsed command, got {} exprs and merge_stderr={merge_stderr}",
            exprs.len()
        );
        let data = DataArray::from_iter(ast, exprs);
        // SAFETY: Command takes a handle to an array of expressions, and a boolean denoting if
        // it's stderr should be merged to stdout, which have been provided.
        let node = unsafe {
            ast.add(NodeInfo {
                kind: NodeInfoKind::Command,
                offset: self.current.start() as u32,
                p1: data.raw(),
                p2: merge_stderr as u32,
            })
        };

        Ok(node)
    }

    fn parse_expr(&mut self, ast: &mut Ast, precedence: Precedence) -> Result<NodeHandle> {
        // This function contains the heart of the recursive descent parser! This uses the Pratt
        // parsing algorithm.
        debug!("began parsing expr");
        let start = self.current.start();
        let mut expr = match self.current.kind() {
            TokenKind::String(_) => self.parse_string_like(ast, NodeInfoKind::String),
            TokenKind::EnvVar(_) => self.parse_string_like(ast, NodeInfoKind::EnvVar),
            TokenKind::Ident(_) => self.parse_string_like(ast, NodeInfoKind::Ident),
            TokenKind::BoolTrue | TokenKind::BoolFalse => self.parse_bool(ast),
            TokenKind::Number(_) => self.parse_number(ast),
            TokenKind::LParen => self.parse_grouped_expr(ast)?,
            TokenKind::QuestionMark => self.parse_last_status(ast),
            TokenKind::Tilde => self.parse_home_dir(ast),
            TokenKind::Bang | TokenKind::Minus | TokenKind::Plus => self.parse_prefix(ast)?,
            TokenKind::Backtick => self.parse_capture(ast)?,
            s => todo!("expr for {s:?}"),
        };

        loop {
            let peek = self.peek();
            if !(peek.kind() != TokenKind::Semicolon && precedence < peek.kind().into()) {
                break;
            }
            expr = match peek.kind() {
                TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::Lt
                | TokenKind::Gt
                | TokenKind::Le
                | TokenKind::Ge
                | TokenKind::Eq
                | TokenKind::Ne => {
                    self.next_token();
                    self.parse_infix(ast, expr)?
                }
                _ => break,
            }
        }

        // Implicit concatenation if the following tokens are found. The most important part here
        // is `peek_token_raw`, which will **not** skip TokenKind::Space tokens!
        if matches!(
            self.lexer.peek_token_raw(LexMode::Normal).kind(),
            TokenKind::String(_)
                | TokenKind::EnvVar(_)
                | TokenKind::LParen
                | TokenKind::Ident(_)
                | TokenKind::QuestionMark
        ) && self.mode.is_normal()
        {
            self.next_token();
            let next = self.parse_expr(ast, Precedence::Lowest)?;
            // SAFETY: Add takes expression handles for both data segments, which are provided
            expr = unsafe {
                ast.add(NodeInfo {
                    kind: NodeInfoKind::Add,
                    offset: start as u32,
                    p1: expr.raw(),
                    p2: next.raw(),
                })
            };
        }

        debug!("sucessfully parsed expr");
        Ok(expr)
    }

    fn parse_infix(&mut self, ast: &mut Ast, lhs: NodeHandle) -> Result<NodeHandle> {
        debug!("began parsing infix expr");
        let binop = match self.current.kind() {
            TokenKind::Plus => NodeInfoKind::Add,
            TokenKind::Minus => NodeInfoKind::Sub,
            TokenKind::Star => NodeInfoKind::Mul,
            TokenKind::Slash => NodeInfoKind::Div,
            TokenKind::Lt => NodeInfoKind::Lt,
            TokenKind::Gt => NodeInfoKind::Gt,
            TokenKind::Le => NodeInfoKind::Le,
            TokenKind::Ge => NodeInfoKind::Ge,
            TokenKind::Eq => NodeInfoKind::Eq,
            TokenKind::Ne => NodeInfoKind::Ne,
            _ => panic!("BUG: should not call when not on binop token!"),
        };
        let prec = Precedence::from(self.current.kind());
        self.next_token();
        let rhs = self.parse_expr(ast, prec)?;
        // SAFETY: binary operations take two expression handles, which have been provided
        let infix = unsafe {
            ast.add(NodeInfo {
                kind: binop,
                offset: self.current.start() as u32,
                p1: lhs.raw(),
                p2: rhs.raw(),
            })
        };

        debug!("got infix with op: {binop:?}");
        Ok(infix)
    }

    fn parse_prefix(&mut self, ast: &mut Ast) -> Result<NodeHandle> {
        debug!("began parsing infix expr");
        let unop = match self.current.kind() {
            TokenKind::Minus => NodeInfoKind::Neg,
            TokenKind::Bang => NodeInfoKind::Bang,
            TokenKind::Plus => NodeInfoKind::Sign,
            _ => panic!("BUG: should not call when not on unop token!"),
        };
        self.next_token();
        let expr = self.parse_expr(ast, Precedence::Prefix)?;
        // SAFETY: unary operations take one expression, which has been provided
        let prefix = unsafe {
            ast.add(NodeInfo {
                kind: unop,
                offset: self.current.start() as u32,
                p1: expr.raw(),
                p2: 0,
            })
        };
        debug!("got unop with op: {unop:?}");
        Ok(prefix)
    }

    fn parse_number(&mut self, ast: &mut Ast) -> NodeHandle {
        let TokenKind::Number(n) = self.current.kind() else {
            panic!("should not call on non-number token!");
        };
        let f = n.parse::<f64>().unwrap();
        let raw = f.to_bits();
        let p1 = (raw >> 32) as u32;
        let p2 = (raw & 0xFFFFFFFF) as u32;
        debug!("found number: {f}");
        // SAFETY: Numbers have their 64-bit float split across `p1` and `p2`, which has been done
        unsafe {
            ast.add(NodeInfo {
                kind: NodeInfoKind::Number,
                offset: self.current.start() as u32,
                p1,
                p2,
            })
        }
    }

    fn parse_capture(&mut self, ast: &mut Ast) -> Result<NodeHandle> {
        debug_assert!(matches!(self.current.kind(), TokenKind::Backtick));
        debug!("began parsing capture");
        let old = self.mode;
        self.mode = LexMode::Normal;
        let start = self.current.start();
        self.next_token();
        self.enter_scope(Scope::Capture);
        let pipeline = self.parse_pipeline(ast)?;
        self.mode = old;
        self.pop_scope();
        // SAFETY: captures take a handle to the pipeline they point to, which has been provided
        let node = unsafe {
            ast.add(NodeInfo {
                kind: NodeInfoKind::Capture,
                offset: start as u32,
                p1: pipeline.raw(),
                p2: 0,
            })
        };
        self.expect_next(TokenKind::Backtick, "expected closing backtick")
            .attach(Label::new(start..=start, "for this backtick"))?;
        debug!("successfully parsed capture");
        Ok(node)
    }

    fn parse_break(&mut self, ast: &mut Ast) -> Result<NodeHandle> {
        debug_assert!(matches!(self.current.kind(), TokenKind::Break));
        if !self.in_scope(Scope::Loop) {
            return Err(Error::new(
                self.current.start(),
                "cannot break outside of loop",
            ))
            .attach(Label::new(
                self.current.start()..self.current.end(),
                "invalid when there are no loop scopes",
            ));
        }
        debug!("got break statement");
        // SAFETY: Break takes nothing
        let node = unsafe {
            ast.add(NodeInfo {
                kind: NodeInfoKind::Break,
                offset: self.current.start() as u32,
                p1: 0,
                p2: 0,
            })
        };
        Ok(node)
    }

    fn parse_continue(&mut self, ast: &mut Ast) -> Result<NodeHandle> {
        debug_assert!(matches!(self.current.kind(), TokenKind::Continue));
        if !self.in_scope(Scope::Loop) {
            return Err(Error::new(
                self.current.start(),
                "cannot continue outside of loop",
            ))
            .attach(Label::new(
                self.current.start()..self.current.end(),
                "invalid when there are no loop scopes",
            ));
        }
        debug!("got continue statement");
        // SAFETY: Continue takes nothing
        let node = unsafe {
            ast.add(NodeInfo {
                kind: NodeInfoKind::Continue,
                offset: self.current.start() as u32,
                p1: 0,
                p2: 0,
            })
        };
        Ok(node)
    }

    fn parse_string_like(&mut self, ast: &mut Ast, kind: NodeInfoKind) -> NodeHandle {
        let (TokenKind::Ident(s) | TokenKind::EnvVar(s) | TokenKind::String(s)) =
            self.current.kind()
        else {
            panic!("BUG: should only be called on a string-like!");
        };
        debug!("parsed string-like ({kind:?}): `{s}`");
        let i = ast.alloc_string(s);
        // SAFETY: string-likes take a handle to their underlying value, which has been provided
        unsafe {
            ast.add(NodeInfo {
                kind,
                offset: self.current.start() as u32,
                p1: i.raw(),
                p2: 0,
            })
        }
    }

    fn parse_bool(&mut self, ast: &mut Ast) -> NodeHandle {
        let p1 = match self.current.kind() {
            TokenKind::BoolTrue => 1,
            TokenKind::BoolFalse => 0,
            _ => panic!("BUG: should only be called on an ident"),
        };
        debug!("parsed bool: `{p1}`");
        // SAFETY: Booleans take a binary value denoting if it's `true` or `false`
        unsafe {
            ast.add(NodeInfo {
                kind: NodeInfoKind::Boolean,
                offset: self.current.start() as u32,
                p1,
                p2: 0,
            })
        }
    }

    fn parse_last_status(&mut self, ast: &mut Ast) -> NodeHandle {
        debug_assert!(matches!(self.current.kind(), TokenKind::QuestionMark));
        debug!("parsed last status");
        // SAFETY: LastStatus takes nothing
        unsafe {
            ast.add(NodeInfo {
                kind: NodeInfoKind::LastStatus,
                offset: self.current.start() as u32,
                p1: 0,
                p2: 0,
            })
        }
    }

    fn parse_home_dir(&mut self, ast: &mut Ast) -> NodeHandle {
        debug_assert!(matches!(self.current.kind(), TokenKind::Tilde));
        debug!("parsed last status");
        // SAEFTY: HomeDir takes nothing
        unsafe {
            ast.add(NodeInfo {
                kind: NodeInfoKind::HomeDir,
                offset: self.current.start() as u32,
                p1: 0,
                p2: 0,
            })
        }
    }

    fn parse_grouped_expr(&mut self, ast: &mut Ast) -> Result<NodeHandle> {
        debug_assert!(matches!(self.current.kind(), TokenKind::LParen));
        debug!("began parsing grouped expr");
        let lparen_pos = self.current.start();
        let old = self.mode;
        self.mode = LexMode::Strict;
        self.next_token();
        let expr = self.parse_expr(ast, Precedence::Lowest)?;
        self.expect_next(
            TokenKind::RParen,
            "expected rparen to close grouped expression",
        )
        .attach(Label::new(
            lparen_pos..self.current.end(),
            "this grouped expression",
        ))?;
        self.mode = old;
        debug!("successfully parsed grouped expr");
        Ok(expr)
    }

    fn expect_next(
        &mut self,
        want: TokenKind<'src>,
        msg: impl Into<Cow<'static, str>>,
    ) -> Result<()> {
        self.next_token();
        if std::mem::discriminant(&self.current.kind()) != std::mem::discriminant(&want) {
            return Err(Error::new(self.current.start(), msg));
        }

        Ok(())
    }

    fn next_token(&mut self) {
        self.current = self.lexer.next_token(self.mode);
        while self.current.kind() == TokenKind::Space {
            self.current = self.lexer.next_token(self.mode);
        }
    }

    fn peek(&mut self) -> Token<'src> {
        self.lexer.peek_token(self.mode)
    }

    fn peek_strict(&mut self) -> Token<'src> {
        self.lexer.peek_token(LexMode::Strict)
    }

    fn enter_scope(&mut self, scope: Scope) {
        self.scope.push(scope);
    }

    fn in_scope(&self, scope: Scope) -> bool {
        self.scope.iter().rev().any(|s| *s == scope)
    }

    fn pop_scope(&mut self) -> Scope {
        self.scope.pop().expect("should always pop a scope!")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Scope {
    Loop,
    Capture,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest,
    Eq,
    Cmp,
    Sum,
    Product,
    Prefix,
}

impl From<TokenKind<'_>> for Precedence {
    fn from(tok: TokenKind) -> Self {
        match tok {
            TokenKind::Plus | TokenKind::Minus => Self::Sum,
            TokenKind::Star | TokenKind::Slash => Self::Product,
            TokenKind::Lt | TokenKind::Gt | TokenKind::Ge | TokenKind::Le => Self::Cmp,
            TokenKind::Eq | TokenKind::Ne => Self::Eq,
            _ => Self::Lowest,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;

    macro_rules! parser_test {
        ($name:ident, $input:expr) => {
            #[test]
            fn $name() {
                let source = Source::new("<test>", $input.to_owned());
                let parser = Parser::new(&source);
                let ast = parser.parse().expect("should parse with no errors!");
                ::insta::assert_display_snapshot!(ast);
            }
        };
        (@fail $name:ident, $input:expr) => {
            #[test]
            fn $name() {
                let source = Source::new("<test>", $input.to_owned());
                let parser = Parser::new(&source);
                let err = parser.parse().expect_err("parser should fail");
                let mut buf = vec![];
                source.fmt_error(err, &mut buf).unwrap();
                ::insta::assert_display_snapshot!(String::from_utf8(buf).unwrap());
            }
        };
    }

    parser_test!(basic_commands, "echo hello world\n");
    parser_test!(basic_arithmetic, "echo .(2 * 1 + 1) hello");
    parser_test!(idents, "echo .hello .(hi + 10)");
    parser_test!(prefix_exprs, "echo .(-10 + !29 * 10)");
    parser_test!(pipelines, "echo hello | wc -l | xargs\n");
    parser_test!(pipelines_multiline, "echo hello\n  | wc -l\n  | xargs\n");
    parser_test!(pipeline_with_end, "echo hi | wc -l > hello.txt\n");
    parser_test!(pipelines_with_merge_stderr, "echo hi %| cat %> hello.txt\n");
    parser_test!(booleans, "echo .(true + false) true");
    parser_test!(bool_infix, "echo .(1 + 1 <= 2)");
    parser_test!(last_status, "echo .? .(? + 10)");
    parser_test!(env_vars, "echo $HELLO .($HOME + \"hello\")");
    parser_test!(assignments, ".x = 1 + 1");
    parser_test!(implicit_concat, "echo .hi/$HELLO/world");
    parser_test!(home_dir, "echo ~/code");
    parser_test!(export, "export HELLO = 1 + 1\necho hi");
    parser_test!(if_, "if x == 3 then echo hi\necho hi end");
    parser_test!(if_else, "if x == 3 then echo hi else echo hi end");
    parser_test!(while_, "while x == 3 do echo hi end");
    parser_test!(captures, "echo `echo hi | wc -l`");
    parser_test!(env_set, "$RUST_LOG=trace $RUST_BACKTRACE=1 cargo test");
    parser_test!(break_and_continue, "while x == 3 do break\ncontinue end");
    parser_test!(backticks_go_to_normal, "echo .(`echo hi`) hello");

    parser_test!(@fail export_no_assign, "export HELLO == 1 + 1");
    parser_test!(@fail export_not_ident, "export $HELLO = 1 + 1");
    parser_test!(@fail unclosed_group, "echo .(2 + 10");
    parser_test!(@fail no_end_for_if, "if x == 3 then echo hi");
    parser_test!(@fail no_then_for_if, "if x == 3 echo hi");
    parser_test!(@fail break_outside_loop, "break");
    parser_test!(@fail continue_outside_loop, "continue");
}
