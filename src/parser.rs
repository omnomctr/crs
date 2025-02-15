pub mod lexer;
pub mod parser;

pub use lexer::*;

#[derive(Debug)]
pub enum ParserErrorKind {
    UnexpectedEOF,
    #[allow(dead_code)]
    IllegalCharacter(char),
    EmptyProgram,
    UnexpectedToken(UnexpectedToken),
    IllegalToken(TokenType),
}

#[derive(Debug)]
pub struct UnexpectedToken {
    expected: TokenType,
    found: TokenType,
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct ParserError {
    reason: ParserErrorKind,
    line: usize,
    filename: String,
}

impl ParserError {
    fn new(line: usize, reason: ParserErrorKind, file: &str) -> ParserError {
        ParserError { reason, line, filename: file.into() }
    }
}