use std::iter::Peekable;
use std::rc::Rc;
use super::{Lexer, ParserError, ParserErrorKind, Token, TokenType, UnexpectedToken};
use crate::ast;
use crate::ast::{BinaryOp, BlockItem, Declaration, Expr, ForInitializer, FunctionDeclaration, IfStatement, Incrementation, Statement, UnaryOp, VariableDeclaration};

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
        let mut func_defs = Vec::new();
        while self.current_token.kind != TokenType::Eof {
            func_defs.push(self.parse_function()?);
        }

        Ok(ast::Program {
            functions: func_defs,
        })
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

    fn eat_ident(&mut self) -> Result<ast::Identifier, ParserError> {
        if let TokenType::Identifier(i) = self.current_token.kind.clone() {
            self.advance_token()?;
            Ok(i)
        } else {
            Err(ParserError::new(
                self.current_token.line_num,
                ParserErrorKind::IllegalToken(self.current_token.kind.clone()),
                self.filename
            ))
        }
    }

    fn parse_function(&mut self) -> Result<ast::FunctionDeclaration, ParserError> {
        self.eat(TokenType::IntKeyword)?;

        let name = self.eat_ident()?;

        self.eat(TokenType::LParen)?;
        let mut params = Vec::new();
        if self.current_token.kind == TokenType::VoidKeyword {
            self.eat(TokenType::VoidKeyword)?;
        } else if self.current_token.kind != TokenType::RParen {
            loop {
                self.eat(TokenType::IntKeyword)?;
                params.push(self.eat_ident()?);
                if self.current_token.kind != TokenType::RParen {
                    self.eat(TokenType::Comma)?
                } else {
                    break;
                }
            }
        }
        self.eat(TokenType::RParen)?;

        let body = if self.current_token.kind == TokenType::LSquirly {
            Some(self.parse_block()?)
        } else {
            self.eat(TokenType::Semicolon)?;
            None
        };

        Ok(ast::FunctionDeclaration {
            name, params, body,
        })
    }

    fn parse_block(&mut self) -> Result<ast::Block, ParserError> {
        self.eat(TokenType::LSquirly)?;

        let mut block = Vec::new();
        while self.current_token.kind != TokenType::RSquirly {
            block.push(self.parse_block_item()?)
        }
        self.eat(TokenType::RSquirly)?;

        Ok(block)
    }

    fn parse_block_item(&mut self) -> Result<ast::BlockItem, ParserError> {
        if self.current_token.kind == TokenType::IntKeyword {
            self.parse_declaration().map(BlockItem::D)
        } else {
            self.parse_statement().map(BlockItem::S)
        }
    }

    fn parse_declaration(&mut self) -> Result<ast::Declaration, ParserError> {
        self.eat(TokenType::IntKeyword)?;
        let ident = self.eat_ident()?;

        if self.current_token.kind == TokenType::Assignment {
            self.eat(TokenType::Assignment)?;
            let expr = self.parse_expr()?;
            self.eat(TokenType::Semicolon)?;
            Ok(Declaration::Var(VariableDeclaration{
                name: ident,
                initializer: Some(expr),
            }))

        } else if self.current_token.kind == TokenType::LParen {
            self.eat(TokenType::LParen)?;
            let mut params = Vec::new();
            if self.current_token.kind != TokenType::RParen {
                loop {
                    self.eat(TokenType::IntKeyword)?;
                    params.push(self.eat_ident()?);
                    if self.current_token.kind != TokenType::RParen {
                        self.eat(TokenType::Comma)?;
                    } else {
                        break;
                    }
                }
            }

            self.eat(TokenType::RParen)?;

            let body = if self.current_token.kind == TokenType::LSquirly {
                Some(self.parse_block()?)
            } else {
                self.eat(TokenType::Semicolon)?;
                None
            };

            Ok(ast::Declaration::Fun(FunctionDeclaration {
                name: ident,
                params,
                body
            }))
        } else {
            self.eat(TokenType::Semicolon)?;
            Ok(ast::Declaration::Var(VariableDeclaration {
                name: ident,
                initializer: None,
            }))
        }
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, ParserError> {
        let current_token = self.current_token.kind.clone();
        let peek = self.peek_token();

        match current_token {
            TokenType::RetKeyword => {
                self.eat(TokenType::RetKeyword)?;
                let expr = self.parse_expr()?;
                self.eat(TokenType::Semicolon)?;
                Ok(ast::Statement::Return(expr))
            },
            TokenType::Semicolon => { self.eat(TokenType::Semicolon)?; Ok(ast::Statement::Empty) },
            TokenType::Identifier(x) if peek == Some(TokenType::Colon) => {
                self.advance_token()?;
                self.eat(TokenType::Colon)?;
                let rhs = self.parse_statement()?;
                Ok(ast::Statement::LabeledStatement(Rc::clone(&x), Box::new(rhs)))
            },
            TokenType::Goto => {
                self.eat(TokenType::Goto)?;
                let label = if let TokenType::Identifier(lbl) = &self.current_token.kind {
                    Rc::clone(&lbl)
                } else {
                    return Err(ParserError::new(self.current_token.line_num, ParserErrorKind::UnexpectedToken(UnexpectedToken {
                        expected: TokenType::Identifier(Rc::new("label".into())),
                        found: self.current_token.kind.clone(),
                    }), self.filename));
                };
                self.advance_token()?;
                self.eat(TokenType::Semicolon)?;

                Ok(ast::Statement::JmpStatement(label))
            }
            TokenType::If => {
                self.eat(TokenType::If)?;
                self.eat(TokenType::LParen)?;
                let condition = self.parse_expr()?;
                self.eat(TokenType::RParen)?;

                let mut then = Vec::new();
                if self.current_token.kind == TokenType::LSquirly {
                    then = self.parse_block()?;
                } else {
                    then.push(self.parse_block_item()?);
                }

                let otherwise = if self.current_token.kind == TokenType::Else {
                    self.eat(TokenType::Else)?;
                    let mut else_block = Vec::new();
                    if self.current_token.kind == TokenType::LSquirly {
                        else_block = self.parse_block()?;
                    } else {
                        else_block.push(self.parse_block_item()?);
                    }
                    Some(else_block)
                } else {
                    None
                };

                let stmt = Statement::If(IfStatement {
                    condition,
                    then,
                    otherwise,
                });
                Ok(stmt)
            },
            TokenType::LSquirly => {
                Ok(ast::Statement::Block(self.parse_block()?))
            },
            TokenType::While => {
                self.eat(TokenType::While)?;
                self.eat(TokenType::LParen)?;
                let condition = self.parse_expr()?;
                self.eat(TokenType::RParen)?;


                let body = if self.current_token.kind == TokenType::Semicolon {
                    self.eat(TokenType::Semicolon)?;
                    Statement::Empty
                } else {
                    self.parse_statement()?
                };

                Ok(ast::Statement::While(condition, Box::new(body), None))
            },
            TokenType::Break => {
                self.eat(TokenType::Break)?;
                Ok(ast::Statement::Break(None))
            },
            TokenType::Continue => {
                self.eat(TokenType::Continue)?;
                Ok(ast::Statement::Continue(None))
            },
            TokenType::Do => {
                self.eat(TokenType::Do)?;

                let body = self.parse_statement()?;
                self.eat(TokenType::While)?;

                self.eat(TokenType::LParen)?;
                let condition = self.parse_expr()?;
                self.eat(TokenType::RParen)?;
                self.eat(TokenType::Semicolon)?;

                Ok(ast::Statement::DoWhile(condition, Box::new(body), None))
            },
            TokenType::For => {
                self.eat(TokenType::For)?;
                self.eat(TokenType::LParen)?;
                let expr1 = if self.current_token.kind == TokenType::Semicolon {
                    self.eat(TokenType::Semicolon)?;
                    None
                } else if self.current_token.kind == TokenType::IntKeyword {
                    if let ast::Declaration::Var(v) = self.parse_declaration()? {
                        Some(ForInitializer::Decl(v))
                    } else {
                        return Err(ParserError::new(
                            self.current_token.line_num,
                            ParserErrorKind::BadForm,
                            self.filename,
                        ));
                    }
                } else {
                    let expr = self.parse_expr()?;
                    self.eat(TokenType::Semicolon)?;
                    Some(ForInitializer::Expr(expr))
                };


                let expr2 = if self.current_token.kind == TokenType::Semicolon {
                    None
                } else {
                    Some(self.parse_expr()?)
                };
                self.eat(TokenType::Semicolon)?;

                let expr3 = if self.current_token.kind == TokenType::Semicolon {
                    None
                } else {
                    Some(self.parse_expr()?)
                };

                self.eat(TokenType::RParen)?;
                let body = self.parse_statement()?;

                Ok(ast::Statement::ForLoop(expr1, expr2, expr3, Box::new(body), None))
            },
            _ => {
                let expr = self.parse_expr()?;
                self.eat(TokenType::Semicolon)?;
                Ok(ast::Statement::Expression(expr))
            },
        }
    }

    fn factor(&mut self) -> Result<ast::Expr, ParserError> {
        let current_token = self.current_token.kind.clone();
        Ok(match current_token {
            TokenType::Constant(c) => {
                self.advance_token()?;
                Expr::Constant(c)
            },
            TokenType::Identifier(i) => {
                let ident = Rc::clone(&i);
                self.advance_token()?;
                if self.current_token.kind == TokenType::Inc {
                    self.eat(TokenType::Inc)?;
                    Expr::PostfixInc(Incrementation::Increment, Box::new(Expr::Var(ident)))
                } else if self.current_token.kind == TokenType::Dec {
                    self.eat(TokenType::Dec)?;
                    Expr::PostfixInc(Incrementation::Decrement, Box::new(Expr::Var(ident)))
                } else if self.current_token.kind == TokenType::LParen {
                    self.eat(TokenType::LParen)?;
                    let mut args = Vec::new();
                    while self.current_token.kind != TokenType::RParen {
                        args.push(self.parse_expr()?);
                        if self.current_token.kind != TokenType::RParen {
                            self.eat(TokenType::Comma)?;
                        } else {
                            break;
                        }
                    }
                    self.eat(TokenType::RParen)?;

                    Expr::FunCall(ident, args, None)
                } else {
                    Expr::Var(ident)
                }
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

                if self.current_token.kind == TokenType::Inc {
                    self.eat(TokenType::Inc)?;
                    Expr::PostfixInc(Incrementation::Increment, Box::new(expr))
                } else if self.current_token.kind == TokenType::Dec {
                    self.eat(TokenType::Dec)?;
                    Expr::PostfixInc(Incrementation::Decrement, Box::new(expr))
                } else {
                    expr
                }
            },
            TokenType::Not => {
                self.eat(TokenType::Not)?;
                let expr = self.parse_expr()?;
                Expr::Unary(UnaryOp::Not, Box::new(expr))
            },
            TokenType::Inc => {
                self.eat(TokenType::Inc)?;
                let rhs = self.parse_expr()?;
                Expr::PrefixInc(Incrementation::Increment, Box::new(rhs))
            },
            TokenType::Dec => {
                self.eat(TokenType::Dec)?;
                let rhs = self.parse_expr()?;
                Expr::PrefixInc(Incrementation::Decrement, Box::new(rhs))
            },
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

    fn expr4(&mut self) -> Result<ast::Expr, ParserError> {
        let mut ret = self.term()?;
        while self.current_token.kind == TokenType::Plus
            || self.current_token.kind == TokenType::Minus
            || self.current_token.kind == TokenType::BitwiseAnd
            || self.current_token.kind == TokenType::BitwiseOr
            || self.current_token.kind == TokenType::BitwiseXor
            || self.current_token.kind == TokenType::LeftShift
            || self.current_token.kind == TokenType::RightShift {
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
                },
                TokenType::BitwiseXor => {
                    self.eat(TokenType::BitwiseXor)?;
                    let rhs = self.term()?;
                    ret = Expr::Binary(BinaryOp::BitwiseXor, Box::new(ret), Box::new(rhs));
                },
                TokenType::LeftShift => {
                    self.eat(TokenType::LeftShift)?;
                    let rhs = self.term()?;
                    ret = Expr::Binary(BinaryOp::LeftShift, Box::new(ret), Box::new(rhs));
                },
                TokenType::RightShift => {
                    self.eat(TokenType::RightShift)?;
                    let rhs = self.term()?;
                    ret = Expr::Binary(BinaryOp::RightShift, Box::new(ret), Box::new(rhs));
                },
                _ => panic!()
            }
        }

        Ok(ret)
    }

    fn expr3(&mut self) -> Result<ast::Expr, ParserError> {
        let mut ret = self.expr4()?;

        while self.current_token.kind == TokenType::LT
            || self.current_token.kind == TokenType::GT
            || self.current_token.kind == TokenType::GTE
            || self.current_token.kind == TokenType::LTE {
            match self.current_token.kind {
                TokenType::LT => {
                    self.eat(TokenType::LT)?;
                    let rhs = self.expr3()?;
                    ret = Expr::Binary(BinaryOp::LT, Box::new(ret), Box::new(rhs));
                },
                TokenType::GT => {
                    self.eat(TokenType::GT)?;
                    let rhs = self.expr3()?;
                    ret = Expr::Binary(BinaryOp::GT, Box::new(ret), Box::new(rhs));
                },
                TokenType::GTE => {
                    self.eat(TokenType::GTE)?;
                    let rhs = self.expr3()?;
                    ret = Expr::Binary(BinaryOp::GTE, Box::new(ret), Box::new(rhs));
                },
                TokenType::LTE => {
                    self.eat(TokenType::LTE)?;
                    let rhs = self.expr3()?;
                    ret = Expr::Binary(BinaryOp::LTE, Box::new(ret), Box::new(rhs));
                },
                _ => panic!(),
            }
        }

        Ok(ret)
    }

    fn expr2(&mut self) -> Result<ast::Expr, ParserError> {
        let mut ret = self.expr3()?;

        while self.current_token.kind == TokenType::Eq
            || self.current_token.kind == TokenType::NEq {
            match self.current_token.kind {
                TokenType::Eq => {
                    self.eat(TokenType::Eq)?;
                    let rhs = self.expr3()?;
                    ret = Expr::Binary(BinaryOp::Eq, Box::new(ret), Box::new(rhs));
                },
                TokenType::NEq => {
                    self.eat(TokenType::NEq)?;
                    let rhs = self.expr3()?;
                    ret = Expr::Binary(BinaryOp::NEq, Box::new(ret), Box::new(rhs));
                },
                _ => panic!()
            }
        }

        Ok(ret)
    }
    // TODO: theres a better way to do this
    fn expr1(&mut self) -> Result<ast::Expr, ParserError> {
        let mut ret = self.expr2()?;

        while self.current_token.kind == TokenType::And {
            self.eat(TokenType::And)?;
            let rhs = self.expr2()?;
            ret = Expr::Binary(BinaryOp::And, Box::new(ret), Box::new(rhs));
        }
        Ok(ret)
    }

    fn expr0(&mut self) -> Result<ast::Expr, ParserError> {
        let mut ret = self.expr1()?;

        while self.current_token.kind == TokenType::Or {
            self.eat(TokenType::Or)?;
            let rhs = self.expr1()?;
            ret = Expr::Binary(BinaryOp::Or, Box::new(ret), Box::new(rhs));
        }

        Ok(ret)
    }

    fn parse_expr(&mut self) -> Result<ast::Expr, ParserError> {
        let mut ret = self.expr0()?;

        while self.current_token.kind == TokenType::Assignment
            || self.current_token.kind == TokenType::PlusEqual
            || self.current_token.kind == TokenType::MinusEqual
            || self.current_token.kind == TokenType::TimesEqual
            || self.current_token.kind == TokenType::DivEqual
            || self.current_token.kind == TokenType::ModEqual
            || self.current_token.kind == TokenType::LeftShiftEqual
            || self.current_token.kind == TokenType::RightShiftEqual
            || self.current_token.kind == TokenType::BitwiseOrEqual
            || self.current_token.kind == TokenType::BitwiseAndEqual
            || self.current_token.kind == TokenType::BitwiseXOrEqual
            || self.current_token.kind == TokenType::QuestionMark {
            match self.current_token.kind {
                TokenType::Assignment => {
                    self.eat(TokenType::Assignment)?;
                    let rhs = self.expr1()?;
                    ret = Expr::Assignment(Box::new(ret), Box::new(rhs));
                },
                TokenType::PlusEqual => {
                    self.eat(TokenType::PlusEqual)?;
                    let rhs = self.expr1()?;
                    ret = Expr::CompoundAssignment(BinaryOp::Add, Box::new(ret), Box::new(rhs));
                },
                TokenType::MinusEqual => {
                    self.eat(TokenType::MinusEqual)?;
                    let rhs = self.expr1()?;
                    ret = Expr::CompoundAssignment(BinaryOp::Subtract, Box::new(ret), Box::new(rhs));
                },
                TokenType::TimesEqual => {
                    self.eat(TokenType::TimesEqual)?;
                    let rhs = self.expr1()?;
                    ret = Expr::CompoundAssignment(BinaryOp::Multiply, Box::new(ret), Box::new(rhs));
                },
                TokenType::DivEqual => {
                    self.eat(TokenType::DivEqual)?;
                    let rhs = self.expr1()?;
                    ret = Expr::CompoundAssignment(BinaryOp::Divide, Box::new(ret), Box::new(rhs));
                },
                TokenType::ModEqual => {
                    self.eat(TokenType::ModEqual)?;
                    let rhs = self.expr1()?;
                    ret = Expr::CompoundAssignment(BinaryOp::Remainder, Box::new(ret), Box::new(rhs));
                },
                TokenType::LeftShiftEqual => {
                    self.eat(TokenType::LeftShiftEqual)?;
                    let rhs = self.expr1()?;
                    ret = Expr::CompoundAssignment(BinaryOp::LeftShift, Box::new(ret), Box::new(rhs));
                },
                TokenType::RightShiftEqual => {
                    self.eat(TokenType::RightShiftEqual)?;
                    let rhs = self.expr1()?;
                    ret = Expr::CompoundAssignment(BinaryOp::RightShift, Box::new(ret), Box::new(rhs));
                },
                TokenType::BitwiseAndEqual => {
                    self.eat(TokenType::BitwiseAndEqual)?;
                    let rhs = self.expr1()?;
                    ret = Expr::CompoundAssignment(BinaryOp::BitwiseAnd, Box::new(ret), Box::new(rhs));
                },
                TokenType::BitwiseOrEqual => {
                    self.eat(TokenType::BitwiseOrEqual)?;
                    let rhs = self.expr1()?;
                    ret = Expr::CompoundAssignment(BinaryOp::BitwiseOr, Box::new(ret), Box::new(rhs));
                },
                TokenType::BitwiseXOrEqual => {
                    self.eat(TokenType::BitwiseXOrEqual)?;
                    let rhs = self.expr1()?;
                    ret = Expr::CompoundAssignment(BinaryOp::BitwiseXor, Box::new(ret), Box::new(rhs));
                },
                TokenType::QuestionMark => {
                    self.eat(TokenType::QuestionMark)?;
                    let then = self.expr1()?;
                    self.eat(TokenType::Colon)?;
                    let otherwise = self.expr1()?;
                    ret = Expr::Ternary(Box::new(ret), Box::new(then), Box::new(otherwise))
                },
                _ => unreachable!(),
            }
        }

        Ok(ret)
    }

    fn peek_token(&mut self) -> Option<TokenType> {
        self.lex.peek()
            .map(|x| x.as_ref().ok())
            .flatten()
            .map(|x| x.kind.clone())
    }

    fn advance_token(&mut self) -> Result<(), ParserError> {
        assert_ne!(self.current_token.kind, TokenType::Eof);
        self.current_token = self.lex
            .next().unwrap_or(Ok(Token::new(self.current_token.line_num, TokenType::Eof)))?;
        Ok(())
    }
}