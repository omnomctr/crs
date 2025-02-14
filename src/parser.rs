pub mod lexer;
pub mod parser;

pub use lexer::*;

#[derive(Debug)]
pub enum ParserErrorKind<'a> {
    UnexpectedEOF,
    #[allow(dead_code)]
    IllegalCharacter(char),
    EmptyProgram,
    UnexpectedToken(UnexpectedToken<'a>)
}

#[derive(Debug)]
struct UnexpectedToken<'a> {
    expected: TokenType<'a>,
    found: TokenType<'a>,
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct ParserError<'a> {
    reason: ParserErrorKind<'a>,
    line: usize,
    filename: String,
}

impl ParserError {
    fn new(line: usize, reason: ParserErrorKind, file: &str) -> ParserError {
        ParserError { reason, line, filename: file.into() }
    }
}