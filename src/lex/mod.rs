use logos::{Lexer as LogosLexer, Logos};
use std::iter::Enumerate;
pub use token::Token;

pub mod parse_num;
mod token;

#[cfg(test)]
mod tests;

// Spanned and LexicalError are types that are required by lalrpop to be in the crate.
pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

#[derive(PartialEq, Debug)]
pub enum LexicalError {}

// Wrapper for the Lexer, allowing it to pass to lalrpop
pub struct Lexer<'a>(pub Enumerate<LogosLexer<'a, Token>>);

impl Iterator for Lexer<'_> {
    type Item = Spanned<Token, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut token = self.0.next()?;

        // Skip over returns, whitespace, and comments
        while matches!(token.1, Token::Whitespace) {
            token = self.0.next()?;
        }

        if crate::DEBUG {
            println!("Lexing: {:?}", token.1);
        }

        Some(Ok((token.0, token.1, token.0 + 1)))
    }
}

pub fn lex(input: &str) -> Lexer {
    Lexer(Token::lexer(input).enumerate())
}
