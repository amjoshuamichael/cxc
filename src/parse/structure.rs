use super::*;
use crate::lex::Token;

pub fn parse_structure(
    mut lexer: &mut Peekable<impl Iterator<Item = Token>>,
) -> Vec<VarDecl> {
    parse_list(
        Token::LeftCurly,
        Some(Token::Comma),
        Token::RghtCurly,
        parse_var_decl,
        lexer,
    )
}
