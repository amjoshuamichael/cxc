use crate::lex::Token;
pub use opcode::Opcode;
use std::iter::Peekable;

mod expression;
mod function;
mod opcode;
mod parsing_data;

pub use expression::*;
pub use function::*;
pub use opcode::*;
pub use parsing_data::*;

pub fn file(mut lexer: Peekable<impl Iterator<Item = Token>>) -> Script {
    let mut declarations = Vec::new();

    loop {
        let decl = parse_var_decl(&mut lexer);

        match lexer.peek() {
            Some(Token::LeftParen) => {
                let parsed_func = parse_func(&mut lexer);
                declarations.push(Declaration::Function {
                    name: decl,
                    args: parsed_func.0,
                    code: parsed_func.1,
                });
            },
            _ => panic!(),
        }

        println!("{:?}", lexer.peek());

        if lexer.peek().is_none() {
            return Script(declarations);
        }
    }
}

fn parse_expr_list(lexer: &mut Peekable<impl Iterator<Item = Token>>) -> Vec<Expr> {
    assert_eq!(lexer.next(), Some(Token::LeftParen));

    if lexer.peek() == Some(&Token::RghtParen) {
        lexer.next();
        return Vec::new();
    }

    let mut arg_list = vec![parse_math_expr(lexer)];

    loop {
        match lexer.next() {
            Some(Token::Comma) => arg_list.push(parse_math_expr(lexer)),
            Some(Token::RghtParen) => break,
            _ => panic!(),
        }
    }

    arg_list
}

fn parse_var_decl(lexer: &mut Peekable<impl Iterator<Item = Token>>) -> VarDecl {
    let var_name = match lexer.next() {
        Some(Token::Ident(name)) => name,
        _ => panic!(),
    };

    let type_spec = match lexer.peek() {
        Some(Token::Colon) => {
            lexer.next();

            Some(parse_type_spec(lexer))
        },
        _ => None,
    };

    VarDecl {
        var_name,
        type_spec,
    }
}

fn parse_type_spec(lexer: &mut Peekable<impl Iterator<Item = Token>>) -> TypeSpec {
    match lexer.next() {
        Some(Token::AmpersandSet(ref_count)) => {
            let Some(Token::Ident(name)) = lexer.next() else { panic!() };

            TypeSpec { ref_count, name }
        },
        Some(Token::Ident(name)) => TypeSpec { ref_count: 0, name },
        _ => panic!(),
    }
}

fn parse_block(lexer: &mut Peekable<impl Iterator<Item = Token>>) -> Expr {
    assert_eq!(lexer.next(), Some(Token::LeftCurly));

    let mut stmts = Vec::new();

    loop {
        match lexer.peek() {
            Some(Token::RghtCurly) => {
                lexer.next();
                return Expr::Block(stmts);
            },
            _ => stmts.push(parse_stmt(lexer)),
        }
    }
}

fn parse_stmt(lexer: &mut Peekable<impl Iterator<Item = Token>>) -> Expr {
    match lexer.peek() {
        Some(Token::Ident(_)) => parse_setvar(lexer),
        _ => parse_expr(lexer),
    }
}

fn parse_setvar(lexer: &mut Peekable<impl Iterator<Item = Token>>) -> Expr {
    let var_decl = parse_var_decl(lexer);

    assert_eq!(lexer.next(), Some(Token::Assignment));

    let expr = parse_expr(lexer);

    Expr::SetVar(var_decl, Box::new(expr))
}

fn parse_expr(lexer: &mut Peekable<impl Iterator<Item = Token>>) -> Expr {
    match lexer.peek() {
        Some(Token::Ident(_)) | Some(Token::Int(_)) | Some(Token::Float(_)) => {
            parse_math_expr(lexer)
        },
        Some(op) if op.get_un_opcode().is_some() => parse_math_expr(lexer),
        Some(Token::At) => parse_for(lexer),
        Some(Token::Question) => parse_if(lexer),
        Some(Token::Bang) => {
            lexer.next();
            parse_expr(lexer)
        },
        Some(_) => todo!(),
        None => unreachable!(),
    }
}

fn parse_for(lexer: &mut Peekable<impl Iterator<Item = Token>>) -> Expr {
    assert_eq!(lexer.next(), Some(Token::At));

    let w = parse_expr(lexer);
    let d = parse_block(lexer);
    Expr::ForWhile(Box::new(w), Box::new(d))
}

fn parse_if(lexer: &mut Peekable<impl Iterator<Item = Token>>) -> Expr {
    assert_eq!(lexer.next(), Some(Token::Question));

    let i = parse_expr(lexer);
    let t = parse_block(lexer);

    Expr::IfThen(Box::new(i), Box::new(t))
}
