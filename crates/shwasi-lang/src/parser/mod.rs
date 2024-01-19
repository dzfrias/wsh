pub mod ast;
mod error;
mod lexer;

pub use self::error::*;
use crate::{
    ast::{
        AliasAssign, Assign, Def, DefArg, DefArgs, EnvSet, Export, If, Pipeline, PipelineEnd,
        PipelineEndKind, While,
    },
    parser::ast::{Ast, Command, Expr, InfixExpr, InfixOp, PrefixExpr, PrefixOp, Stmt},
};
pub use lexer::*;
use smol_str::SmolStr;

#[derive(Debug)]
pub struct Parser<'src> {
    buf: &'src TokenBuffer,
    tok_idx: usize,
    current_token: Token,
    in_subcmd: bool,
    scope_stack: Vec<Scope>,
}

impl<'src> Parser<'src> {
    pub fn new(buf: &'src TokenBuffer) -> Self {
        Self {
            buf,
            tok_idx: 0,
            current_token: buf.get(0).unwrap_or(Token::Eof).clone(),
            in_subcmd: false,
            scope_stack: vec![],
        }
    }

    pub fn parse(mut self) -> ParseResult<Ast> {
        let mut stmts = vec![];
        let mut defs = vec![];

        while self.current_token != Token::Eof {
            if self.current_token == Token::Def {
                let def = self.parse_def()?;
                defs.push(def);
                self.next_token();
                continue;
            }
            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
            self.next_token();
        }

        Ok(Ast::new(stmts, defs))
    }

    fn parse_block(&mut self) -> ParseResult<Vec<Stmt>> {
        let mut stmts = vec![];
        loop {
            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
            if matches!(self.current_token, Token::End | Token::Else) {
                break;
            }
            if self.current_token == Token::Newline
                && matches!(self.peek(), Token::End | Token::Else)
            {
                self.next_token();
                break;
            }
            self.next_token();
        }

        Ok(stmts)
    }

    fn parse_stmt(&mut self) -> ParseResult<Stmt> {
        Ok(match self.current_token {
            Token::String(_) | Token::QuotedString(_) | Token::Dollar => {
                Stmt::Pipeline(self.parse_pipeline()?)
            }
            Token::Alias => Stmt::AliasAssign(self.parse_alias_assign()?),
            Token::Ident(_) if matches!(self.peek(), Token::Assign | Token::ColonAssign) => {
                Stmt::Assign(self.parse_assign()?)
            }
            Token::Export => Stmt::Export(self.parse_export()?),
            Token::If => Stmt::If(self.parse_if()?),
            Token::While => Stmt::While(self.parse_while()?),
            Token::Break => {
                if !self.in_loop() {
                    return Err(self.expected("cannot break outside of loop"));
                }
                self.expect_next(Token::Newline, "expected newline to follow break")?;
                Stmt::Break
            }
            Token::Continue => {
                if !self.in_loop() {
                    return Err(self.expected("cannot continue outside of loop"));
                }
                self.expect_next(Token::Newline, "expected newline to follow continue")?;
                Stmt::Continue
            }
            Token::Return => {
                if !self.in_func() {
                    return Err(self.expected("cannot return outside of function"));
                }
                self.expect_next(Token::Newline, "expected newline to follow return")?;
                Stmt::Return
            }
            _ => Stmt::Expr(self.parse_expr(Precedence::Lowest)?),
        })
    }

    fn parse_pipeline(&mut self) -> ParseResult<Pipeline> {
        let mut env = vec![];
        // Environment variables being set
        while self.current_token == Token::Dollar {
            self.next_token();
            let name = self
                .buf
                .get_ident(self.tok_idx)
                .or_else(|| Some(Ident::new(&self.buf.get_string(self.tok_idx)?)))
                .ok_or_else(|| self.expected("expected a valid environment variable name"))?;
            self.expect_next(Token::Assign, "expected assign after env name")?;
            self.next_token();
            let expr = self.parse_expr(Precedence::Lowest)?;
            self.next_token();
            env.push(EnvSet { name, expr });
        }

        let mut cmds = vec![];
        let mut write = None;
        cmds.push(self.parse_cmd()?);

        while matches!(self.current_token, Token::Pipe | Token::PercentPipe) {
            self.next_token();
            cmds.push(self.parse_cmd()?);
        }
        if matches!(
            self.current_token,
            Token::Write | Token::Append | Token::PercentAppend | Token::PercentWrite
        ) {
            let kind = match self.current_token {
                Token::Write | Token::PercentWrite => PipelineEndKind::Write,
                Token::Append | Token::PercentAppend => PipelineEndKind::Append,
                _ => unreachable!(),
            };
            if matches!(
                self.current_token,
                Token::PercentAppend | Token::PercentWrite
            ) {
                cmds.last_mut().unwrap().merge_stderr = true;
            }
            self.next_token();
            let pipeline_end = PipelineEnd {
                kind,
                expr: self.parse_expr(Precedence::Lowest)?,
            };
            write = Some(Box::new(pipeline_end));
            if self.peek() != Token::Eof {
                self.expect_next(
                    Token::Newline,
                    "expected newline after file write redirection",
                )?;
            } else {
                self.next_token();
            }
        }

        Ok(Pipeline {
            commands: cmds,
            env,
            write,
        })
    }

    fn parse_if(&mut self) -> ParseResult<If> {
        debug_assert_eq!(self.current_token, Token::If);
        self.next_token();
        let condition = self.parse_expr(Precedence::Lowest)?;
        self.expect_next(Token::Then, "expected `then` after if condition")?;
        self.opt(Token::Newline);
        self.next_token();
        let body = self.parse_block()?;
        let current = self.current_token.clone();
        self.opt(Token::Newline);
        match current {
            Token::End => Ok(If {
                condition,
                body,
                else_: None,
            }),
            Token::Else => {
                self.next_token();
                let else_ = self.parse_block()?;
                if self.current_token != Token::End {
                    return Err(self.expected("expected `end` to terminate else block"));
                }
                self.opt(Token::Newline);
                Ok(If {
                    condition,
                    body,
                    else_: Some(else_),
                })
            }
            _ => panic!("should not terminate with anything other than `end` or `else`!"),
        }
    }

    fn parse_while(&mut self) -> ParseResult<While> {
        debug_assert_eq!(self.current_token, Token::While);
        self.next_token();
        let condition = self.parse_expr(Precedence::Lowest)?;
        self.expect_next(Token::Do, "expected `do` after while condition")?;
        self.opt(Token::Newline);
        self.next_token();
        self.scope_stack.push(Scope::Loop);
        let body = self.parse_block()?;
        assert_eq!(Scope::Loop, self.scope_stack.pop().unwrap());
        if self.current_token != Token::End {
            return Err(self.expected("expected `end` to terminate while block"));
        }
        self.opt(Token::Newline);
        Ok(While { condition, body })
    }

    fn parse_def(&mut self) -> ParseResult<Def> {
        debug_assert_eq!(self.current_token, Token::Def);
        self.next_token();
        let name = self
            .buf
            .get_ident(self.tok_idx)
            .ok_or_else(|| self.expected("expected function name"))?;
        let args = if self.peek() == Token::Colon {
            self.next_token();
            let mut args = vec![];
            self.next_token();
            args.push(self.parse_arg()?);
            while self.peek() != Token::Do {
                self.next_token();
                let def_arg = self.parse_arg()?;
                args.push(def_arg);
            }
            DefArgs(args)
        } else {
            DefArgs(vec![])
        };
        self.expect_next(
            Token::Do,
            "expected `do` token to follow function signature",
        )?;
        self.opt(Token::Newline);
        self.next_token();
        self.scope_stack.push(Scope::Function);
        let body = self.parse_block()?;
        assert_eq!(Scope::Function, self.scope_stack.pop().unwrap());
        if self.current_token != Token::End {
            return Err(self.expected("expected `end` to terminate function block"));
        }
        self.opt(Token::Newline);
        Ok(Def { name, body, args })
    }

    fn parse_arg(&mut self) -> ParseResult<DefArg> {
        let name = self
            .buf
            .get_ident(self.tok_idx)
            .ok_or_else(|| self.expected("expected argument name"))?;
        let alias = if self.peek() == Token::Pipe {
            self.next_token();
            self.next_token();
            let alias = self
                .buf
                .get_ident(self.tok_idx)
                .ok_or_else(|| self.expected("expected argument name"))?;
            let mut chars = alias.chars();
            let alias = chars.next().unwrap();
            if chars.next().is_some() {
                return Err(self.expected("expected arg alias to be one character long"));
            }
            Some(alias)
        } else {
            None
        };
        let default = if self.peek() == Token::Assign {
            self.next_token();
            self.next_token();
            let default = self
                .buf
                .get_ident(self.tok_idx)
                .map(|ident| SmolStr::new::<&str>(&ident))
                .or_else(|| self.buf.get_string(self.tok_idx))
                .ok_or_else(|| self.expected("expected argument name"))?;
            Some(default)
        } else {
            None
        };
        Ok(match (alias, default) {
            (None, None) => DefArg::Positional { name },
            (None, Some(default)) => DefArg::Named {
                name,
                alias: None,
                default,
            },
            (Some(alias), None) => DefArg::Boolean { name, alias },
            (Some(alias), Some(default)) => DefArg::Named {
                name,
                alias: Some(alias),
                default,
            },
        })
    }

    fn parse_alias_assign(&mut self) -> ParseResult<AliasAssign> {
        debug_assert_eq!(self.current_token, Token::Alias);
        self.next_token();
        let name = self
            .buf
            .get_string(self.tok_idx)
            .ok_or_else(|| self.expected("expected a valid command name"))?;
        self.expect_next(Token::Assign, "expected assign after alias name")?;
        self.next_token();
        let pipeline = self.parse_pipeline()?;
        Ok(AliasAssign { name, pipeline })
    }

    fn parse_export(&mut self) -> ParseResult<Export> {
        debug_assert_eq!(self.current_token, Token::Export);
        // Optional dollar sign
        if self.peek() == Token::Dollar {
            self.next_token();
        }
        self.next_token();
        let name = self
            .buf
            .get_ident(self.tok_idx)
            .or_else(|| Some(Ident::new(&self.buf.get_string(self.tok_idx)?)))
            .ok_or_else(|| self.expected("expected a valid environment variable name"))?;
        self.expect_next(Token::Assign, "expected assign after export name")?;
        self.next_token();
        let expr = self.parse_expr(Precedence::Lowest)?;
        self.next_token();
        Ok(Export { name, expr })
    }

    fn parse_assign(&mut self) -> ParseResult<Assign> {
        debug_assert!(matches!(self.current_token, Token::Ident(_)));

        let name = self.buf.get_ident(self.tok_idx).unwrap();
        // Peek should either be Token::Assign or Token::ColonAssign
        let global = matches!(self.peek(), Token::ColonAssign);
        self.next_token();
        self.next_token();
        let expr = self.parse_expr(Precedence::Lowest)?;
        self.next_token();
        Ok(Assign { name, expr, global })
    }

    fn parse_backtick(&mut self) -> ParseResult<Pipeline> {
        let start = self.offset();
        self.next_token();
        self.in_subcmd = true;
        let pipeline = self.parse_pipeline()?;
        self.expect_next(Token::Backtick, "expected backtick to close command")
            .attach(Label::new(start..start + 1, "backtick found here"))?;
        self.in_subcmd = false;
        Ok(pipeline)
    }

    fn parse_cmd(&mut self) -> ParseResult<Command> {
        let name = self
            .buf
            .get_string(self.tok_idx)
            .ok_or(self.error(ParseErrorKind::UnfinishedPipeline))?;
        let mut args = vec![];
        while !matches!(
            self.peek(),
            Token::Newline
                | Token::Eof
                | Token::Pipe
                | Token::Write
                | Token::Append
                | Token::PercentPipe
                | Token::PercentWrite
                | Token::PercentAppend
                | Token::End
                | Token::Else
        ) {
            if self.peek() == Token::Backtick && self.in_subcmd {
                return Ok(Command {
                    name,
                    args,
                    merge_stderr: false,
                });
            }
            self.next_token();
            let arg = self.parse_expr(Precedence::Lowest)?;
            args.push(arg);
        }
        let merge_stderr = self.peek() == Token::PercentPipe;
        self.next_token();
        let cmd = Command {
            name,
            args,
            merge_stderr,
        };

        Ok(cmd)
    }

    fn parse_expr(&mut self, precedence: Precedence) -> ParseResult<Expr> {
        let mut expr = match &self.current_token {
            Token::String(s) | Token::QuotedString(s) => Expr::String(s.clone()),
            Token::Number(i) => Expr::Number(*i),
            Token::BoolFalse => Expr::Bool(false),
            Token::BoolTrue => Expr::Bool(true),
            Token::Ident(ident) => Expr::Ident(ident.clone()),
            Token::LParen => self.parse_grouped_expr()?,
            Token::Bang | Token::Minus | Token::Plus => Expr::Prefix(self.parse_prefix()?),
            Token::Tilde => Expr::Tilde,
            Token::Backtick => Expr::Pipeline(self.parse_backtick()?),
            Token::QuestionMark => Expr::LastStatus,
            Token::Dollar => Expr::Env(self.parse_env_expr()?),
            _ => return Err(self.expected("expected valid expression")),
        };

        while self.peek() != Token::Newline && precedence < self.peek().into() {
            expr = match self.peek() {
                Token::Plus
                | Token::Minus
                | Token::Star
                | Token::Slash
                | Token::Lt
                | Token::Gt
                | Token::Le
                | Token::Ge
                | Token::Eq
                | Token::Ne => {
                    self.next_token();
                    Expr::Infix(self.parse_infix(expr)?)
                }
                _ => break,
            };
        }

        // Implicit concatenation of expressions
        if self.buf.get(self.tok_idx + 1).is_some_and(|t| {
            matches!(
                t,
                Token::String(_)
                    | Token::QuotedString(_)
                    | Token::Dollar
                    | Token::LParen
                    | Token::Ident(_)
                    | Token::QuestionMark
            )
        }) {
            self.next_token();
            let next = self.parse_expr(Precedence::Lowest)?;
            expr = Expr::Infix(InfixExpr {
                op: InfixOp::Add,
                lhs: Box::new(expr),
                rhs: Box::new(next),
            });
        }

        Ok(expr)
    }

    fn parse_infix(&mut self, lhs: Expr) -> ParseResult<InfixExpr> {
        let tok_idx = self.tok_idx;
        let precedence = self.current_token.clone().into();
        self.next_token();
        let rhs = self.parse_expr(precedence)?;
        let infix = InfixExpr {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            op: match self.buf.get(tok_idx).unwrap() {
                Token::Plus => InfixOp::Add,
                Token::Minus => InfixOp::Sub,
                Token::Star => InfixOp::Mul,
                Token::Slash => InfixOp::Div,
                Token::Lt => InfixOp::Lt,
                Token::Gt => InfixOp::Gt,
                Token::Le => InfixOp::Le,
                Token::Ge => InfixOp::Ge,
                Token::Eq => InfixOp::Eq,
                Token::Ne => InfixOp::Ne,
                _ => unreachable!(),
            },
        };

        Ok(infix)
    }

    fn parse_grouped_expr(&mut self) -> ParseResult<Expr> {
        let start = self.offset();
        if self.peek() == Token::RParen {
            self.next_token();
            return Ok(Expr::String("".into()));
        }
        self.next_token();
        let expr = self.parse_expr(Precedence::Lowest)?;
        self.expect_next(Token::RParen, "expected rparen to close expression")
            .attach(Label::new(start..start + 1, "lparen found here"))?;

        Ok(expr)
    }

    fn parse_prefix(&mut self) -> ParseResult<PrefixExpr> {
        let tok_idx = self.tok_idx;
        self.next_token();
        let expr = self.parse_expr(Precedence::Prefix)?;
        let prefix = PrefixExpr {
            expr: Box::new(expr),
            op: match self.buf.get(tok_idx).unwrap() {
                Token::Bang => PrefixOp::Bang,
                Token::Minus => PrefixOp::Neg,
                Token::Plus => PrefixOp::Sign,
                _ => unreachable!(),
            },
        };

        Ok(prefix)
    }

    fn parse_env_expr(&mut self) -> ParseResult<Ident> {
        self.next_token();
        self.buf
            .get_string(self.tok_idx)
            .map(|s| Ident::new(s.as_ref()))
            .or_else(|| self.buf.get_ident(self.tok_idx))
            .ok_or_else(|| self.expected("expected a valid environment variable name"))
    }

    fn in_loop(&self) -> bool {
        self.scope_stack
            .iter()
            .rfind(|scope| **scope == Scope::Loop)
            .is_some()
    }

    fn in_func(&self) -> bool {
        self.scope_stack
            .iter()
            .rfind(|scope| **scope == Scope::Function)
            .is_some()
    }

    fn next_token(&mut self) {
        self.tok_idx += 1;
        self.current_token = self
            .buf
            .get(self.tok_idx)
            .unwrap_or_else(|| {
                self.tok_idx -= 1;
                Token::Eof
            })
            .clone();
        if self.current_token == Token::Space {
            self.next_token();
        }
    }

    fn peek(&self) -> Token {
        self.buf
            .iter()
            .skip(self.tok_idx + 1)
            .find(|t| !matches!(t, Token::Space))
            .unwrap_or(Token::Eof)
            .clone()
    }

    fn expect_next(&mut self, tok: Token, msg: &'static str) -> ParseResult<()> {
        self.next_token();
        if self.current_token != tok {
            return Err(self.expected(msg));
        }

        Ok(())
    }

    fn expected(&self, msg: &'static str) -> ParseError {
        let tok = self.current_token.clone();
        let len = tok.size();
        self.error_with_len(
            ParseErrorKind::UnexpectedToken {
                token: tok,
                expected: msg,
            },
            len,
        )
    }

    fn error(&self, kind: ParseErrorKind) -> ParseError {
        ParseError::new(self.offset()..self.offset() + 1, kind)
    }

    fn error_with_len(&self, kind: ParseErrorKind, len: usize) -> ParseError {
        let mut e = self.error(kind);
        e.range = e.range.start..e.range.start + len;
        e
    }

    fn offset(&self) -> usize {
        self.buf
            .offset(self.tok_idx)
            .expect("token index should be within bounds")
    }

    fn opt(&mut self, tok: Token) -> bool {
        if self.peek() == tok {
            self.next_token();
            true
        } else {
            false
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Scope {
    Loop,
    Function,
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

impl From<Token> for Precedence {
    fn from(tok: Token) -> Self {
        match tok {
            Token::Plus | Token::Minus => Self::Sum,
            Token::Star | Token::Slash => Self::Product,
            Token::Lt | Token::Gt | Token::Ge | Token::Le => Self::Cmp,
            Token::Eq | Token::Ne => Self::Eq,
            _ => Self::Lowest,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::lexer::Lexer;

    use super::*;

    #[test]
    fn simple_commands() {
        let input = "echo hello world\nls -l";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn infix_expressions() {
        let input = "echo .(1 + 2 * 3 / (4 - 5))";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn prefix_expressions() {
        let input = "echo .(-1 + 2 * !3)";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn lparen_no_rparen() {
        let input = "echo .(1";
        let buf = Lexer::new(input).lex();
        let result = Parser::new(&buf).parse().unwrap_err();
        assert_eq!(
            ParseError::new_with_labels(
                8..8,
                ParseErrorKind::UnexpectedToken {
                    token: Token::Eof,
                    expected: "expected rparen to close expression",
                },
                vec![Label::new(6..7, "lparen found here")],
            ),
            result
        );
    }

    #[test]
    fn symbols() {
        let input = "echo .x .y";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn pipelines() {
        let input = "echo hi | cat | wc -w";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn empty_pipelines() {
        let input = "echo hi | cat |";
        let buf = Lexer::new(input).lex();
        let err = Parser::new(&buf).parse().unwrap_err();
        assert_eq!(
            ParseError::new(15..16, ParseErrorKind::UnfinishedPipeline),
            err
        );
    }

    #[test]
    fn backtick_piplines() {
        let input = "echo `cat file.txt | wc -l`";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn backtick_in_expressions() {
        let input = "echo .(`cat file.txt` + \"nice\")";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn last_status() {
        let input = "echo .? .(? + 10)";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn alias() {
        let input = "alias foo = echo hi | cat";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn alias_with_invalid_name() {
        let input = "alias = = echo hi | cat";
        let buf = Lexer::new(input).lex();
        let err = Parser::new(&buf).parse().unwrap_err();
        assert_eq!(
            ParseError::new(
                6..7,
                ParseErrorKind::UnexpectedToken {
                    token: Token::Assign,
                    expected: "expected a valid command name"
                }
            ),
            err
        );
    }

    #[test]
    fn assignments() {
        let input = ".x = 11 + 10";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn comparisons() {
        let input = ".(1 < 2) .(1 > 2) .(1 <= 2) .(1 >= 2) .(1 == 2) .(1 != 2)";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn pipeline_with_file_write() {
        let input =
            "echo hi > file.txt\necho \"hello world\" | wc -w | xargs > .(\"test\" + \".txt\")";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn redirects_must_be_at_end() {
        let input = "echo hi > file.txt | echo hi";
        let buf = Lexer::new(input).lex();
        let err = Parser::new(&buf).parse().unwrap_err();
        assert_eq!(
            ParseError::new(
                19..20,
                ParseErrorKind::UnexpectedToken {
                    token: Token::Pipe,
                    expected: "expected newline after file write redirection"
                }
            ),
            err
        );
    }

    #[test]
    fn redirect_append() {
        let input = "echo hi >> file.txt";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn environment_setting_in_pipelines() {
        let input = "$FOO=.(10 + 10) $BAR=20 echo hi";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn environment_variable_get() {
        let input = "echo $FOO";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn export() {
        let input = "export FOO = .(10 + 10)\nexport $FOO = hello";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn merge_stderr_pipes() {
        let input = "echo hi %| cat";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn merge_stderr_redirect() {
        let input = "echo hi %> file.txt";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn merge_stderr_redirect_append() {
        let input = "echo hi %>> file.txt";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn implicit_concat() {
        let input = "echo $ENV_VAR/nice .(hello)/nice";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn if_stmt() {
        let input = "if x == 10 then echo hi end\nif x == 10 then\necho hi\necho hello\nend\n";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn if_else() {
        let input = "if x == 10 then echo hi else echo nice end";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn while_stmt() {
        let input = "while x == 10 do echo hi end";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn break_and_continue() {
        let input = "while x == 10 do break\ncontinue\nend";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn no_break_outside_of_loop() {
        let input = "break";
        let buf = Lexer::new(input).lex();
        let err = Parser::new(&buf).parse().unwrap_err();
        assert_eq!(
            ParseError::new(
                0..5,
                ParseErrorKind::UnexpectedToken {
                    token: Token::Break,
                    expected: "cannot break outside of loop"
                }
            ),
            err
        );
    }

    #[test]
    fn function_simple() {
        let input = "def f do\necho hello world\nend\n";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn global() {
        let input = ".x := hi";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn no_return_outside_of_function() {
        let input = "return";
        let buf = Lexer::new(input).lex();
        let err = Parser::new(&buf).parse().unwrap_err();
        assert_eq!(
            ParseError::new(
                0..6,
                ParseErrorKind::UnexpectedToken {
                    token: Token::Return,
                    expected: "cannot return outside of function"
                }
            ),
            err
        );
    }

    #[test]
    fn function_args() {
        let input = "def f : x y do echo hello world end";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn named_function_args() {
        let input = "def f : x color|c=always fast|f do echo hello world end";
        let buf = Lexer::new(input).lex();
        let ast = Parser::new(&buf).parse().unwrap();
        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn arg_aliases_error_with_more_than_one_character() {
        let input = "def f : color|color2=always do echo hello world end";
        let buf = Lexer::new(input).lex();
        let err = Parser::new(&buf).parse().unwrap_err();
        assert_eq!(
            ParseError::new(
                14..20,
                ParseErrorKind::UnexpectedToken {
                    token: Token::Ident(Ident::new("color2")),
                    expected: "expected arg alias to be one character long"
                }
            ),
            err
        );
    }

    #[test]
    fn function_args_needs_one_if_indicated() {
        let input = "def f : do echo hello world end";
        let buf = Lexer::new(input).lex();
        let err = Parser::new(&buf).parse().unwrap_err();
        assert_eq!(
            ParseError::new(
                8..10,
                ParseErrorKind::UnexpectedToken {
                    token: Token::Do,
                    expected: "expected argument name"
                }
            ),
            err
        );
    }
}
