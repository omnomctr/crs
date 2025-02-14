use std::str::Chars;
use std::iter::Peekable;
use crate::parser::{ParserError, ParserErrorKind};
use std::iter::Iterator;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum TokenType<'a> {
    Identifier(&'a str),
    Constant(i32),
    IntKeyword,
    VoidKeyword,
    RetKeyword,
    LParen,
    RParen,
    LSquirly,
    RSquirly,
    Semicolon,
    Eof,
    Complement,
    Minus,
    Decrement,
}

#[derive(Debug)]
pub struct Token<'a> {
    pub kind: TokenType<'a>,
    pub line_num: usize,
}

impl<'a> Token<'a> {
    pub fn new(l: &Lexer<'a>, kind: TokenType<'a>) -> Self {
        Token {
            line_num: l.line_num,
            kind,
        }
    }

    pub fn is_ident_char(c: char) -> bool {
        c.is_alphanumeric() || c == '_' || c.is_digit(10)
    }
}

pub struct Lexer<'a> {
    input: &'a str,
    iter: Peekable<Chars<'a>>,
    pub line_num: usize,
    pub ch: char,
    pub pos: usize,
    pub filename: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str, filename: &'a str) -> Result<Self, ParserError> {
        if input.is_empty()  { return Err(ParserError::new(0, ParserErrorKind::EmptyProgram, filename)); }

        let mut iter = input.chars().peekable();
        let ch = iter.next().unwrap();
        let pos = 0;
        Ok(Lexer {
            input,
            iter,
            ch,
            pos,
            line_num: 1,
            filename,
        })
    }

    pub fn next_token(&mut self) -> Result<Token<'a>, ParserError> {
        self.skip_whitespace()?;

        use TokenType as Tok;
        let ret = match self.ch {
            '\0' => {
                return Ok(Token::new(&self, Tok::Eof))
            },
            '(' => Tok::LParen,
            ')' => Tok::RParen,
            '{' => Tok::LSquirly,
            '}' => Tok::RSquirly,
            ';' => Tok::Semicolon,
            '~' => Tok::Complement,
            '-' => {
                if self.peek_char() == Some('-') {
                    self.read_char()?;
                    Tok::Decrement
                } else {
                    Tok::Minus
                }
            },
            x => {
                if self.ch.is_digit(10) {
                    return self.read_constant();
                } else if Token::is_ident_char(self.ch) {
                    return self.read_ident();
                } else {
                    return Err(ParserError::new(self.line_num, ParserErrorKind::IllegalCharacter(x), self.filename));
                }
            }
        };

        self.read_char()?;
        Ok(Token::new(&self, ret))
    }

    fn read_constant(&mut self) -> Result<Token<'a>, ParserError> {
        let starting_pos = self.pos;

        while self.ch.is_digit(10) {
            self.read_char()?;
        }

        let constant = &self.input[starting_pos..self.pos]
            .parse::<i32>().unwrap();

        Ok(Token::new(&self, TokenType::Constant(constant.clone())))
    }

    fn read_ident(&mut self) -> Result<Token<'a>, ParserError> {
        let starting_pos = self.pos;

        while Token::is_ident_char(self.ch) {
            self.read_char()?;
        }

        let ident = &self.input[starting_pos..self.pos];
        use TokenType as Tok;
        let tok = match ident {
            "int" => Tok::IntKeyword,
            "void" => Tok::VoidKeyword,
            "return" => Tok::RetKeyword,
            _ => Tok::Identifier(ident),
        };

        Ok(Token::new(&self, tok))
    }

    fn read_char(&mut self) -> Result<(), ParserError> {
        if self.ch == '\0' {
            /* already at eof */
            return Err(ParserError::new(self.line_num, ParserErrorKind::UnexpectedEOF, self.filename));
        }
        self.ch = self
            .iter.next()
            .unwrap_or('\0');
        self.pos += 1;
        Ok(())
    }

    fn peek_char(&mut self) -> Option<char> {
        self.iter.peek().cloned()
    }

    fn skip_whitespace(&mut self) -> Result<(), ParserError> {
        while self.ch.is_whitespace() || self.ch == '/' {
            if self.peek_char() == Some('/') {
                while self.ch != '\n' && self.ch != '\0' {
                    self.read_char()?;
                }
            }
            if self.ch == '\n' {
                self.line_num += 1;
            }
            self.read_char()?;
        }

        Ok(())
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, ParserError<'a>>;
    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.next_token();
        match ret {
            Err(e) => return Some(Err(e)),
            Ok(t) => {
                if t.kind == TokenType::Eof {
                    None
                } else {
                    Some(Ok(t))
                }
            }
        }
    }
}