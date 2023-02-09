use logos::Logos;

mod indent_parens;
mod parse_num;
mod tok;

pub use indent_parens::indent_parens;
pub(crate) use tok::Tok;
pub use tok::{TypeName, VarName};

use crate::parse::context::ParseContext;

pub fn lex(input: &str) -> ParseContext<()> {
    if crate::XC_DEBUG {
        println!();
        println!("====NEW LEX====");
    }

    let mut tokens: Vec<(usize, Tok, usize)> = Vec::new();

    let mut lexer = Tok::lexer(input);

    while let Some(tok) = lexer.next() {
        tokens.push((lexer.span().start, tok, lexer.span().end));
    }

    ParseContext::new(tokens)
}
