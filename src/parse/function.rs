use super::*;

pub fn parse_func(
    lexer: &mut Peekable<impl Iterator<Item = Token>>,
) -> (Vec<VarDecl>, Expr) {
    let arg_list = parse_arg_list(lexer);
    let func_block = parse_block(lexer);

    (arg_list, func_block)
}

pub fn parse_arg_list(
    lexer: &mut Peekable<impl Iterator<Item = Token>>,
) -> Vec<VarDecl> {
    assert_eq!(lexer.next(), Some(Token::LeftParen));

    if lexer.peek() == Some(&Token::RghtParen) {
        lexer.next();
        return Vec::new();
    }

    let mut arg_list = vec![parse_var_decl(lexer)];

    loop {
        match lexer.next() {
            Some(Token::Comma) => arg_list.push(parse_var_decl(lexer)),
            Some(Token::RghtParen) => break,
            _ => panic!(),
        }
    }

    arg_list
}
