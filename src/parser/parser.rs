use std::iter::Peekable;
use super::{Lexer, ParserError, ParserErrorKind, Token, TokenType, UnexpectedToken};
use crate::ast;
use std::rc::Rc;
use crate::ast::{BinaryOp, Expr, UnaryOp};

pub struct Parser<'a> {
    lex: Peekable<&'a mut Lexer<'a>>,
    filename: &'a str,
    current_token: Token,
}

impl<'a> Parser<'a> {
    pub fn parse(input: &'a str, filename: &'a str) -> Result<ast::Program, ParserError> {
        let mut lex = Lexer::new(input, filename)?;
        let mut parser = Parser::new(&mut lex)?;

        parser.parse_program()
    }

    pub fn new(input: &'a mut Lexer<'a>) -> Result<Self, ParserError> {
        let filename = input.filename;
        let mut lex = input.peekable();

        let current_token = lex.next()
            .ok_or_else(|| ParserError::new(1, ParserErrorKind::UnexpectedEOF, filename))??;

        Ok(Parser {
            lex, current_token, filename
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
                                    self.filename
            ))
        } else if self.current_token.kind == TokenType::Eof {
            Ok(())
        } else {
            self.advance_token()?;
            Ok(())
        }
    }

    fn parse_function(&mut self) -> Result<ast::Function, ParserError> {
        self.eat(TokenType::IntKeyword)?;

        let name = if let TokenType::Identifier(i) = self.current_token.kind.clone() {
            self.advance_token()?;
            Rc::clone(&i)
        } else {
            return Err(ParserError::new(
                self.current_token.line_num,
                ParserErrorKind::IllegalToken(self.current_token.kind.clone()),
                self.filename
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

    fn factor(&mut self) -> Result<ast::Expr, ParserError> {
        Ok(match self.current_token.kind {
            TokenType::Constant(c) => {
                self.advance_token()?;
                Expr::Constant(c)
            },
            TokenType::Minus => {
                self.advance_token()?;
                let expr = self.parse_expr()?;
                Expr::Unary(UnaryOp::Negate, Box::new(expr))
            },
            TokenType::Complement => {
                self.advance_token()?;
                let expr = self.parse_expr()?;
                Expr::Unary(UnaryOp::Complement, Box::new(expr))
            }
            TokenType::LParen => {
                self.eat(TokenType::LParen)?;
                let expr = self.parse_expr()?;
                self.eat(TokenType::RParen)?;

                expr
            }
            _ => {
                return Err(ParserError::new(self.current_token.line_num, ParserErrorKind::BadForm, self.filename))
            }
        })
    }

    fn term(&mut self) -> Result<ast::Expr, ParserError> {
        let mut ret = self.factor()?;
        while self.current_token.kind == TokenType::Times
            || self.current_token.kind == TokenType::Divide
            || self.current_token.kind == TokenType::Mod {
            match self.current_token.kind {
                TokenType::Times => {
                    self.eat(TokenType::Times)?;
                    let rhs = self.factor()?;
                    ret = Expr::Binary(BinaryOp::Multiply, Box::new(ret), Box::new(rhs));
                },
                TokenType::Divide => {
                    self.eat(TokenType::Divide)?;

                    let rhs = self.factor()?;
                    ret = Expr::Binary(BinaryOp::Divide, Box::new(ret), Box::new(rhs));
                }
                TokenType::Mod => {
                    self.eat(TokenType::Mod)?;
                    let rhs = self.factor()?;
                    ret = Expr::Binary(BinaryOp::Remainder, Box::new(ret), Box::new(rhs));
                }
                _ => panic!()
            }
        }

        Ok(ret)
    }

    fn parse_expr(&mut self) -> Result<ast::Expr, ParserError> {
        let mut ret = self.term()?;
        while self.current_token.kind == TokenType::Plus
            || self.current_token.kind == TokenType::Minus
            || self.current_token.kind == TokenType::BitwiseAnd
            || self.current_token.kind == TokenType::BitwiseOr {
            match self.current_token.kind {
                TokenType::Plus => {
                    self.eat(TokenType::Plus)?;
                    let rhs = self.term()?;
                    ret = Expr::Binary(BinaryOp::Add, Box::new(ret), Box::new(rhs));
                },
                TokenType::Minus => {
                    self.eat(TokenType::Minus)?;
                    let rhs = self.term()?;
                    ret = Expr::Binary(BinaryOp::Subtract, Box::new(ret), Box::new(rhs));
                },
                TokenType::BitwiseAnd => {
                    self.eat(TokenType::BitwiseAnd)?;
                    let rhs = self.term()?;
                    ret = Expr::Binary(BinaryOp::BitwiseAnd, Box::new(ret), Box::new(rhs));
                },
                TokenType::BitwiseOr => {
                    self.eat(TokenType::BitwiseOr)?;
                    let rhs = self.term()?;
                    ret = Expr::Binary(BinaryOp::BitwiseOr, Box::new(ret), Box::new(rhs));
                }
                _ => panic!()
            }
        }

        Ok(ret)
    }

    fn advance_token(&mut self) -> Result<(), ParserError> {
        assert_ne!(self.current_token.kind, TokenType::Eof);
        self.current_token = self.lex
            .next().unwrap_or(Ok(Token::new(self.current_token.line_num, TokenType::Eof)))?;
        Ok(())
    }
}