use super::{Lexer, ParserError, ParserErrorKind, Token, TokenType, UnexpectedToken};

pub struct Parser<'a> {
    lex: &'a mut Lexer<'a>,
    current_token: Token<'a>
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str, filename: &'a str) -> Result<Self, ParserError> {
        let mut lex =  Lexer::new(input, filename)?;

        Self::new_with_lexer(&mut lex, filename)
    }
    pub fn new_with_lexer(input: &mut Lexer<'a>, filename: &'a str) -> Result<Self, ParserError> {
        let lex = input;
        let current_token = lex.next_token()?;

        Ok(Parser {
            lex, current_token,
        })
    }

    fn eat(&mut self, expected: TokenType) -> Result<(), ParserError> {
        if self.current_token.kind != expected {
             Err(ParserError::new(self.current_token.line_num,
                                    ParserErrorKind::UnexpectedToken(
                                        UnexpectedToken {
                                            expected,
                                            found: self.current_token.kind,
                                        }
                                    ),
                                    self.lex.filename
            ))
        } else if self.current_token.kind != TokenType::Eof {
            Ok(())
        } else {
            self.advance_token();
            Ok(())
        }
    }

    fn advance_token(&mut self) {
        assert!(self.current_token.kind != TokenType::Eof);
        self.current_token = self.lex.next_token().unwrap();
    }
}