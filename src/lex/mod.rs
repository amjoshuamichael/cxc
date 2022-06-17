use logos::{Lexer, Logos};
use std::iter::Enumerate;
pub use token::Token;

pub mod parse_num;
mod token;

#[cfg(test)]
mod tests;

// Lalrpop forces you to put a lexical error enum (titled "LexicalError") into your crate and use
// it in the iterator like this. This error is not actually used for anything. It also forces you
// to use this "Spanned" type.
pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

#[derive(PartialEq, Debug)]
pub enum LexicalError {}

pub struct SerfLex<'a>(pub Enumerate<Lexer<'a, Token>>);

impl<'input> Iterator for SerfLex<'input> {
    type Item = Spanned<Token, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.0.next()?;

        Some(Ok((token.0, token.1, token.0 + 1)))
    }
}

pub fn lex(input: &str) -> SerfLex {
    SerfLex(Token::lexer(input).enumerate())
}
