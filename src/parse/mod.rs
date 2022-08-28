pub use crate::lex::Lexer;
use crate::lex::Tok;
pub use opcode::Opcode;
use std::collections::HashMap;
use std::collections::HashSet;
pub use std::iter::Peekable;

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

pub fn file(mut lexer: Lexer) -> Script {
    let mut declarations = Vec::new();

    loop {
        let Some(Tok::Ident(decl_name)) = lexer.next() else { panic!() };

        let generic_labels: HashMap<String, u8> =
            if lexer.peek() == Some(Tok::LeftAngle) {
                parse_list(
                    (Tok::LeftAngle, Tok::RghtAngle),
                    Some(Tok::Comma),
                    parse_generic_label,
                    &mut lexer,
                )
                .iter()
                .enumerate()
                .map(|(i, s)| (s.clone(), i as u8))
                .collect()
            } else {
                HashMap::new()
            };

        match lexer.peek() {
            Some(Tok::LeftParen) => {
                declarations.push(parse_func(&mut lexer, decl_name));
            },
            Some(Tok::LeftCurly) => {
                let (typ, methods, dependencies) = parse_advanced_alias(
                    &mut lexer,
                    &mut StructParsingContext {
                        generics: generic_labels.clone(),
                        name: decl_name.clone(),
                        dependencies: HashSet::new(),
                    },
                );

                let strct = Declaration::Type {
                    name: decl_name,
                    typ,
                    contains_generics: generic_labels.len() > 0,
                    dependencies,
                };

                declarations.push(strct);

                for m in methods {
                    declarations.push(m);
                }
            },
            _ => panic!(),
        }

        if lexer.peek().is_none() {
            return Script(declarations);
        }
    }
}

pub fn parse_generic_label(lexer: &mut Lexer) -> String {
    match lexer.next().unwrap() {
        Tok::Ident(name) => name,
        _ => panic!(),
    }
}

pub fn parse_func(lexer: &mut Lexer, name: String) -> Declaration {
    let args = parse_list(
        (Tok::LeftParen, Tok::RghtParen),
        Some(Tok::Comma),
        parse_var_decl,
        lexer,
    );

    assert_eq!(lexer.next(), Some(Tok::Colon));

    let ret_type = parse_type_alias(lexer);

    let code = parse_block(lexer);

    Declaration::Function {
        name,
        ret_type,
        args,
        code,
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

fn parse_var_decl(lexer: &mut Lexer) -> VarDecl {
    let Some(Tok::Ident(var_name)) = lexer.next() else { panic!() };

    let type_spec = match lexer.peek() {
        Some(Tok::Colon) => {
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

fn parse_assignable(lexer: &mut Lexer) -> Assignable {
    let lhs = parse_expr(lexer);

    if let Expr::Ident(var_name) = lhs {
        let type_spec = match lexer.peek() {
            Some(Tok::Colon) => {
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

fn parse_block(lexer: &mut Lexer) -> Expr {
    Expr::Block(parse_list(
        (Tok::LeftCurly, Tok::RghtCurly),
        None,
        parse_stmt,
        lexer,
    ))
}

fn parse_stmt(lexer: &mut Lexer) -> Expr {
    let next = lexer.peek().unwrap();
    let after_that = lexer.peek_by(1).unwrap();

    match (next, after_that) {
        (Tok::Ident(_), Tok::LeftBrack | Tok::Colon | Tok::Assignment) => {
            parse_setvar(lexer)
        },
        _ => parse_expr(lexer),
    }
}

fn parse_setvar(lexer: &mut Lexer) -> Expr {
    let assignable = parse_assignable(lexer);

    assert_eq!(lexer.next(), Some(Tok::Assignment));

    let rhs = parse_expr(lexer);

    match assignable {
        Assignable::Declare(d) => Expr::MakeVar(d, box rhs),
        Assignable::Get(lhs) => Expr::SetVar(box lhs, box rhs),
    }
}

fn parse_expr(lexer: &mut Lexer) -> Expr {
    match lexer.peek() {
        Some(Tok::Ident(_)) | Some(Tok::Int(_)) | Some(Tok::Float(_))
        | Some(Tok::LeftBrack) => parse_math_expr(lexer),
        Some(tok) if tok.is_un_op() => parse_math_expr(lexer),
        Some(Tok::At) => parse_for(lexer),
        Some(Tok::Question) => parse_if(lexer),
        Some(Tok::Bang) => {
            lexer.next();
            Expr::Return(box parse_expr(lexer))
        },
        Some(_) => todo!(),
        None => panic!(),
    }
}

fn parse_for(lexer: &mut Lexer) -> Expr {
    assert_eq!(lexer.next(), Some(Tok::At));

    let w = parse_expr(lexer);
    let d = parse_block(lexer);

    Expr::ForWhile(Box::new(w), Box::new(d))
}

fn parse_if(lexer: &mut Lexer) -> Expr {
    assert_eq!(lexer.next(), Some(Tok::Question));

    let i = parse_expr(lexer);
    let t = parse_block(lexer);

    Expr::IfThen(Box::new(i), Box::new(t))
}
