use logos::{Lexer, Logos};
pub use token::Token;

pub mod parse_num;
mod token;

//#[cfg(test)]
// mod tests;

pub fn lex(input: &str) -> Lexer<Token> {
    Token::lexer(input)
}
