pub mod lexer;
pub mod parser;

pub use lexer::*;

/* I need to add these because the compiler still says its
 * never read from if its only from Debug printing
 */
#[allow(dead_code)]
#[derive(Debug)]
pub enum ParserErrorKind {
    UnexpectedEOF,

    IllegalCharacter(char),
    EmptyProgram,
    UnexpectedToken(UnexpectedToken),
    IllegalToken(TokenType),
    BadForm,
}

#[allow(dead_code)]
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