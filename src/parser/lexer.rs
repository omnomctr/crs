use std::str::Chars;
use std::iter::Peekable;
use crate::parser::{ParserError, ParserErrorKind};
use std::iter::Iterator;
use std::rc::Rc;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TokenType {
    Identifier(Rc<String>),
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
    Plus,
    Times,
    Divide,
    Mod,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,
    Not,
    And,
    Or,
    Eq,
    NEq,
    LT,
    GT,
    LTE,
    GTE,
    Assignment,
    PlusEqual,
    MinusEqual,
    TimesEqual,
    DivEqual,
    ModEqual,
    Inc,
    Dec,
    LeftShiftEqual,
    RightShiftEqual,
    BitwiseAndEqual,
    BitwiseOrEqual,
    BitwiseXOrEqual,
    If,
    Else,
    QuestionMark,
    Colon,
    Goto,
    While,
    Break,
    Continue,
    Do,
    For,
    Comma,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenType,
    pub line_num: usize,
}

impl Token {
    pub fn new(line_num: usize, kind: TokenType) -> Self {
        Token {
            line_num,
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

    pub fn next_token(&mut self) -> Result<Token, ParserError> {
        self.skip_whitespace()?;

        use TokenType as Tok;
        // we need to get the value because .peek() need mutable reference for some reason
        let peek = self.peek_char();
        let ret = match self.ch {
            '\0' => {
                return Ok(Token::new(self.line_num, Tok::Eof))
            },
            '(' => Tok::LParen,
            ')' => Tok::RParen,
            '{' => Tok::LSquirly,
            '}' => Tok::RSquirly,
            ';' => Tok::Semicolon,
            '~' => Tok::Complement,
            '-' if peek == Some('=') => { self.read_char()?; Tok::PlusEqual },
            '-' if peek == Some('-') => { self.read_char()?; Tok::Dec },
            '-' => Tok::Minus,
            '+' if peek == Some('=') => { self.read_char()?; Tok::PlusEqual },
            '+' if peek == Some('+') => { self.read_char()?;  Tok::Inc },
            '+' => Tok::Plus,
            '/' if peek == Some('=') => { self.read_char()?; Tok::DivEqual },
            '/' => Tok::Divide,
            '*' if peek == Some('=') => { self.read_char()?; Tok::TimesEqual },
            '*' => Tok::Times,
            '%' if peek == Some('=') => { self.read_char()?; Tok::ModEqual },
            '%' => Tok::Mod,
            '^' if peek == Some('=') => { self.read_char()?; Tok::BitwiseXOrEqual },
            '^' => Tok::BitwiseXor,
            '<' if peek == Some('<') => {
                self.read_char()?;
                if self.peek_char() == Some('=') {
                    self.read_char()?;
                    Tok::LeftShiftEqual
                } else {
                    Tok::LeftShift
                }
            },
            '>' if peek == Some('>') => {
                self.read_char()?;
                if self.peek_char() == Some('=') {
                    self.read_char()?;
                    Tok::RightShiftEqual
                } else {
                    Tok::RightShift
                }
            },
            '&' if peek == Some('&') => { self.read_char()?; Tok::And },
            '&' if peek == Some('=') => { self.read_char()?; Tok::BitwiseAndEqual },
            '&' => Tok::BitwiseAnd,
            '|' if peek == Some('|') => { self.read_char()?; Tok::Or },
            '|' if peek == Some('|') => { self.read_char()?; Tok::BitwiseOrEqual },
            '|' => Tok::BitwiseOr,
            '=' if peek == Some('=') => { self.read_char()?; Tok::Eq },
            '=' => Tok::Assignment,
            '!' if peek == Some('=') => { self.read_char()?; Tok::NEq },
            '!' => Tok::Not,
            '<' if peek == Some('=') => { self.read_char()?; Tok::LTE },
            '<' => Tok::LT,
            '>' if peek == Some('=') => { self.read_char()?; Tok::GTE },
            '>' => Tok::GT,
            '?' => Tok::QuestionMark,
            ':' => Tok::Colon,
            ',' => Tok::Comma,
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
        Ok(Token::new(self.line_num, ret))
    }

    fn read_constant(&mut self) -> Result<Token, ParserError> {
        let starting_pos = self.pos;

        while self.ch.is_digit(10) {
            self.read_char()?;
        }

        let constant = &self.input[starting_pos..self.pos]
            .parse::<i32>().unwrap();

        Ok(Token::new(self.line_num, TokenType::Constant(constant.clone())))
    }

    fn read_ident(&mut self) -> Result<Token, ParserError> {
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
            "if" => Tok::If,
            "else" => Tok::Else,
            "goto" => Tok::Goto,
            "while" => Tok::While,
            "break" => Tok::Break,
            "continue" => Tok::Continue,
            "do" => Tok::Do,
            "for" => Tok::For,
            _ => Tok::Identifier(Rc::new(ident.into())),
        };

        Ok(Token::new(self.line_num, tok))
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

    pub fn peek_char(&mut self) -> Option<char> {
        self.iter.peek().cloned()
    }

    fn skip_whitespace(&mut self) -> Result<(), ParserError> {
        while self.ch.is_whitespace() || (self.ch == '/' && self.peek_char() == Some('/')) || (self.ch == '/' && self.peek_char() == Some('*')) {
            // TODO: make this better
            if self.ch == '/' && self.peek_char() == Some('/') {
                while self.ch != '\n' && self.ch != '\0' {
                    self.read_char()?;
                }
            } else if self.ch == '/' && self.peek_char() == Some('*') {
                while !(self.ch == '*' && self.peek_char() == Some('/')) && self.ch != '\0' {
                    self.read_char()?;
                }
                if self.ch == '*' {
                    self.read_char()?;
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
    type Item = Result<Token, ParserError>;
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