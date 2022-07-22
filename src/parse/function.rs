use super::*;

pub fn parse_func(
    lexer: &mut Peekable<impl Iterator<Item = Token>>,
) -> (Vec<VarDecl>, Expr) {
    let arg_list = parse_list(
        Token::LeftParen,
        Some(Token::Comma),
        Token::RghtParen,
        parse_var_decl,
        lexer,
    );

    let func_block = parse_block(lexer);

    (arg_list, func_block)
}
