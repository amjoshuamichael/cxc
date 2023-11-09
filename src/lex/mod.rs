use logos::Logos;

mod indent_parens;
mod parse_num;
mod tok;

pub use indent_parens::indent_parens;
pub(crate) use tok::Tok;
pub use tok::{TypeName, VarName};

use crate::parse::{context::ParseContext, GlobalParseContext};

pub fn lex(input: &str) -> GlobalParseContext {
    #[cfg(feature = "xc-debug")]
    {
        println!();
        println!("====NEW LEX====");
    }

    let mut tokens: Vec<Tok> = Vec::new();
    let mut spans: Vec<(usize, usize)> = Vec::new();

    let mut lexer = Tok::lexer(input);

    while let Some(tok) = lexer.next() {
        tokens.push(tok);
        spans.push((lexer.span().start, lexer.span().end));
    }

    ParseContext::new(tokens, spans, Box::from(input))
}
