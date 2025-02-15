use super::{Lexer, ParserError, ParserErrorKind, Token, TokenType, UnexpectedToken};
use crate::ast;
use std::rc::Rc;
use crate::ast::{Expr, UnaryOp};

pub struct Parser<'a> {
    lex: &'a mut Lexer<'a>,
    current_token: Token,
}

impl<'a> Parser<'a> {
    pub fn parse(input: &'a str, filename: &'a str) -> Result<ast::Program, ParserError> {
        let mut lex = Lexer::new(input, filename)?;
        let mut parser = Parser::new(&mut lex)?;

        parser.parse_program()
    }

    pub fn new(input: &'a mut Lexer<'a>) -> Result<Self, ParserError> {
        let lex = input;
        let current_token = lex.next_token()?;

        Ok(Parser {
            lex, current_token,
        })
    }

    pub fn parse_program(&mut self) -> Result<ast::Program, ParserError> {
        let f = self.parse_function()?;
        self.eat(TokenType::Eof)?;

        Ok(ast::Program { f })
    }

    fn eat(&mut self, expected: TokenType) -> Result<(), ParserError> {
        if self.current_token.kind != expected {
             Err(ParserError::new(self.current_token.line_num,
                                    ParserErrorKind::UnexpectedToken(
                                        UnexpectedToken {
                                            expected,
                                            found: self.current_token.kind.clone(),
                                        }
                                    ),
                                    self.lex.filename
            ))
        } else if self.current_token.kind == TokenType::Eof {
            Ok(())
        } else {
            self.advance_token();
            Ok(())
        }
    }

    fn parse_function(&mut self) -> Result<ast::Function, ParserError> {
        self.eat(TokenType::IntKeyword)?;

        let name = if let TokenType::Identifier(i) = self.current_token.kind.clone() {
            self.advance_token();
            Rc::clone(&i)
        } else {
            return Err(ParserError::new(
                self.current_token.line_num,
                ParserErrorKind::IllegalToken(self.current_token.kind.clone()),
                self.lex.filename
            ));
        };

        self.eat(TokenType::LParen)?;
        self.eat(TokenType::VoidKeyword)?;
        self.eat(TokenType::RParen)?;
        self.eat(TokenType::LSquirly)?;

        let stmt = self.parse_statement()?;

        self.eat(TokenType::RSquirly)?;

        Ok(ast::Function {
            name, stmt
        })
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, ParserError> {
        self.eat(TokenType::RetKeyword)?;
        let expr = self.parse_expr()?;
        self.eat(TokenType::Semicolon)?;

        Ok(ast::Statement::Return(expr))
    }

    fn parse_expr(&mut self) -> Result<ast::Expr, ParserError> {
        match self.current_token.kind {
            TokenType::Constant(i) => {
                let ret = Expr::Constant(i);
                self.advance_token();
                Ok(ret)
            },
            TokenType::LParen => {
                self.eat(TokenType::LParen)?;
                let expr = self.parse_expr()?;
                self.eat(TokenType::RParen)?;
                Ok(expr)
            },
            TokenType::Complement => {
                self.eat(TokenType::Complement)?;
                Ok(Expr::Unary(UnaryOp::Complement, Box::new(self.parse_expr()?)))
            }
            TokenType::Minus => {
                self.eat(TokenType::Minus)?;
                Ok(Expr::Unary(UnaryOp::Negate, Box::new(self.parse_expr()?)))
            }
            _ => return Err(ParserError::new(
                self.current_token.line_num,
                ParserErrorKind::IllegalToken(self.current_token.kind.clone()),
                self.lex.filename
            ))
        }
    }

    fn advance_token(&mut self) {
        assert_ne!(self.current_token.kind, TokenType::Eof);
        self.current_token = self.lex.next_token().unwrap();
    }
}