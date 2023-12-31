pub mod ast;
mod error;
mod lexer;

pub use self::error::*;
use crate::{
    ast::{AliasAssign, Assign, EnvSet, Export, Pipeline, PipelineEnd, PipelineEndKind},
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
}

impl<'src> Parser<'src> {
    pub fn new(buf: &'src TokenBuffer) -> Self {
        Self {
            buf,
            tok_idx: 0,
            current_token: buf.get(0).unwrap_or(Token::Eof).clone(),
            in_subcmd: false,
        }
    }

    pub fn parse(mut self) -> ParseResult<Ast> {
        let mut stmts = vec![];

        while self.current_token != Token::Eof {
            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
            self.next_token();
        }

        Ok(Ast::new(stmts))
    }

    fn parse_stmt(&mut self) -> ParseResult<Stmt> {
        Ok(match self.current_token {
            Token::String(_) | Token::QuotedString(_) | Token::Dollar => {
                Stmt::Pipeline(self.parse_pipeline()?)
            }
            Token::Alias => Stmt::AliasAssign(self.parse_alias_assign()?),
            Token::Ident(_) if self.peek() == Token::Assign => Stmt::Assign(self.parse_assign()?),
            Token::Export => Stmt::Export(self.parse_export()?),
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
                .get_string(self.tok_idx)
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

        while self.current_token == Token::Pipe {
            self.next_token();
            cmds.push(self.parse_cmd()?);
        }
        if matches!(self.current_token, Token::Write | Token::Append) {
            let kind = match self.current_token {
                Token::Write => PipelineEndKind::Write,
                Token::Append => PipelineEndKind::Append,
                _ => unreachable!(),
            };
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
            .get_string(self.tok_idx)
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
        self.expect_next(Token::Assign, "BUG: should be unreachable")
            .unwrap();
        self.next_token();
        let expr = self.parse_expr(Precedence::Lowest)?;
        self.next_token();
        Ok(Assign { name, expr })
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
            Token::Newline | Token::Eof | Token::Pipe | Token::Write | Token::Append
        ) {
            if self.peek() == Token::Backtick && self.in_subcmd {
                return Ok(Command { name, args });
            }
            self.next_token();
            let arg = self.parse_expr(Precedence::Lowest)?;
            args.push(arg);
        }
        self.next_token();
        let cmd = Command { name, args };

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

    fn parse_env_expr(&mut self) -> ParseResult<SmolStr> {
        self.next_token();
        self.buf
            .get_string(self.tok_idx)
            .ok_or_else(|| self.expected("expected a valid environment variable name"))
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
    }

    fn peek(&self) -> Token {
        self.buf.get(self.tok_idx + 1).unwrap_or(Token::Eof).clone()
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
}
