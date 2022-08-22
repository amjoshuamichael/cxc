use crate::lex::Token;
pub use opcode::Opcode;
use std::collections::HashMap;
use std::iter::Peekable;

mod expression;
mod list;
mod opcode;
mod parsing_data;
mod structure;

pub use expression::*;
pub use list::*;
pub use opcode::*;
pub use parsing_data::*;
pub use structure::*;

pub fn file(mut lexer: Peekable<impl Iterator<Item = Token>>) -> Script {
    let mut declarations = Vec::new();

    loop {
        let Some(Token::Ident(decl_name)) = lexer.next() else { panic!() };

        let generic_list = if lexer.peek() == Some(&Token::LeftAngle) {
            parse_list(
                Token::LeftAngle,
                Some(Token::Comma),
                Token::RghtAngle,
                |lexer| match lexer.next() {
                    Some(Token::Ident(name)) => name,
                    _ => panic!(),
                },
                &mut lexer,
            )
        } else {
            Vec::new()
        };

        let mut generic_labels = HashMap::new();

        for (index, name) in generic_list.iter().enumerate() {
            generic_labels.insert(name.clone(), index as u8);
        }

        match lexer.peek() {
            Some(Token::LeftParen) => {
                let args = parse_list(
                    Token::LeftParen,
                    Some(Token::Comma),
                    Token::RghtParen,
                    parse_var_decl,
                    &mut lexer,
                );

                assert_eq!(lexer.next(), Some(Token::Colon));

                let ret_type = parse_type_alias(&mut lexer);

                let code = parse_block(&mut lexer);

                declarations.push(Declaration::Function {
                    name: decl_name,
                    ret_type,
                    args,
                    code,
                });
            },
            Some(Token::LeftCurly) => {
                let typ = parse_generic_alias(&mut lexer, &generic_labels);

                let strct = Declaration::Struct {
                    name: decl_name,
                    typ,
                    contains_generics: generic_labels.len() > 0,
                };

                declarations.push(strct);
            },
            _ => panic!(),
        }

        if lexer.peek().is_none() {
            return Script(declarations);
        }
    }
}

enum Assignable {
    Declare(VarDecl),
    Get(Expr),
}

impl Assignable {
    pub fn force_declare(&self) -> VarDecl {
        match self {
            Assignable::Declare(vd) => vd.clone(),
            _ => panic!(),
        }
    }
}

fn parse_var_decl(lexer: &mut Peekable<impl Iterator<Item = Token>>) -> VarDecl {
    let Some(Token::Ident(var_name)) = lexer.next() else { panic!() };

    let type_spec = match lexer.peek() {
        Some(Token::Colon) => {
            lexer.next();

            Some(parse_type_alias(lexer))
        },
        _ => None,
    };

    VarDecl {
        var_name,
        type_spec,
    }
}

fn parse_assignable(
    lexer: &mut Peekable<impl Iterator<Item = Token>>,
) -> Assignable {
    let lhs = parse_expr(lexer);

    if let Expr::Ident(var_name) = lhs {
        let type_spec = match lexer.peek() {
            Some(Token::Colon) => {
                lexer.next();

                Some(parse_type_alias(lexer))
            },
            _ => None,
        };

        Assignable::Declare(VarDecl {
            var_name,
            type_spec,
        })
    } else {
        Assignable::Get(lhs)
    }
}

fn parse_block(lexer: &mut Peekable<impl Iterator<Item = Token>>) -> Expr {
    Expr::Block(parse_list(
        Token::LeftCurly,
        None,
        Token::RghtCurly,
        parse_stmt,
        lexer,
    ))
}

fn parse_stmt(lexer: &mut Peekable<impl Iterator<Item = Token>>) -> Expr {
    match lexer.peek() {
        Some(Token::Ident(_)) => parse_setvar(lexer),
        _ => parse_expr(lexer),
    }
}

fn parse_setvar(lexer: &mut Peekable<impl Iterator<Item = Token>>) -> Expr {
    let assignable = parse_assignable(lexer);

    assert_eq!(lexer.next(), Some(Token::Assignment));

    let rhs = parse_expr(lexer);

    match assignable {
        Assignable::Declare(d) => Expr::MakeVar(d, box rhs),
        Assignable::Get(lhs) => Expr::SetVar(box lhs, box rhs),
    }
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
            Expr::Return(box parse_expr(lexer))
        },
        Some(Token::LeftCurly) => {
            parse_list(
                Token::LeftCurly,
                Some(Token::Comma),
                Token::RghtCurly,
                |lexer| {
                    let Some(Token::Ident(field)) = lexer.next() else { panic!() };
                    assert_eq!(lexer.next(), Some(Token::Assignment));
                    let rhs = parse_expr(lexer);
                    (field, rhs)
                },
                lexer,
            );

            todo!()
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
