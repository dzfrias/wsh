pub mod ast;
mod error;
mod lexer;
mod symbol;

pub use self::error::*;
pub use self::symbol::*;
use crate::{
    ast::Pipeline,
    parser::ast::{Ast, Command, Expr, InfixExpr, InfixOp, PrefixExpr, PrefixOp, Stmt},
};
pub use lexer::*;

#[derive(Debug)]
pub struct Parser<'src> {
    buf: &'src TokenBuffer,
    tok_idx: usize,
    current_token: Token,
    symbols: SymbolTable,
}

impl<'src> Parser<'src> {
    pub fn new(buf: &'src TokenBuffer) -> Self {
        Self {
            buf,
            tok_idx: 0,
            current_token: buf.get(0).unwrap_or(Token::Eof).clone(),
            symbols: SymbolTable::new(),
        }
    }

    pub fn parse(mut self) -> ParseResult<Ast> {
        let mut stmts = vec![];

        while self.current_token != Token::Eof {
            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
            self.next_token();
        }

        Ok(Ast::new(stmts, self.symbols))
    }

    fn parse_stmt(&mut self) -> ParseResult<Stmt> {
        Ok(match self.current_token {
            Token::String(_) => Stmt::Pipeline(self.parse_pipeline()?),
            _ => Stmt::Expr(self.parse_expr(Precedence::Lowest)?),
        })
    }

    fn parse_pipeline(&mut self) -> ParseResult<Pipeline> {
        let mut cmds = vec![];
        cmds.push(self.parse_cmd()?);

        while self.current_token == Token::Pipe {
            self.next_token();
            cmds.push(self.parse_cmd()?);
        }

        Ok(Pipeline(cmds))
    }

    fn parse_cmd(&mut self) -> ParseResult<Command> {
        let name = self
            .buf
            .get_string(self.tok_idx)
            .ok_or(self.error(ParseErrorKind::UnfinishedPipeline))?;
        let mut args = vec![];
        while !matches!(self.peek(), Token::Newline | Token::Eof | Token::Pipe) {
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
            Token::String(s) => Expr::String(s.clone()),
            Token::Number(i) => Expr::Number(*i),
            Token::Ident(ident) => Expr::Ident(self.symbols.intern(ident.clone())),
            Token::LParen => self.parse_grouped_expr()?,
            Token::Bang | Token::Minus | Token::Plus => Expr::Prefix(self.parse_prefix()?),
            _ => return Err(self.expected("expected valid expression")),
        };

        while self.peek() != Token::Newline && precedence < self.peek().into() {
            expr = match self.peek() {
                Token::Plus | Token::Minus | Token::Star | Token::Slash => {
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
        self.error(ParseErrorKind::UnexpectedToken {
            token: self.current_token.clone(),
            expected: msg,
        })
    }

    fn error(&self, kind: ParseErrorKind) -> ParseError {
        ParseError::new(self.offset(), kind)
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
    Sum,
    Product,
    Prefix,
}

impl From<Token> for Precedence {
    fn from(tok: Token) -> Self {
        match tok {
            Token::Plus | Token::Minus => Self::Sum,
            Token::Star | Token::Slash => Self::Product,
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
            ParseError::new(
                8,
                ParseErrorKind::UnexpectedToken {
                    token: Token::Eof,
                    expected: "expected rparen to close expression",
                }
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
        assert_eq!(ParseError::new(15, ParseErrorKind::UnfinishedPipeline), err);
    }
}
