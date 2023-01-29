use logos::Logos;

mod indent_parens;
mod parse_num;
mod tok;

pub use indent_parens::indent_parens;
pub(crate) use tok::Tok;
pub use tok::{TypeName, VarName};

use crate::parse::context::ParseContext;

pub fn lex(input: &str) -> ParseContext<()> {
    if crate::DEBUG {
        println!();
        println!("====NEW LEX====");
    }

    let tokens: Vec<Tok> = Tok::lexer(input).collect();

    ParseContext::new(tokens)
}
