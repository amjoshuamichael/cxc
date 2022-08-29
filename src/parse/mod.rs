pub use crate::lex::Context;
use crate::lex::Tok;
pub use opcode::Opcode;
use std::collections::HashMap;
use std::collections::HashSet;
pub use std::iter::Peekable;
use std::ops::Deref;
use std::ops::DerefMut;

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

#[derive(Clone, Default, Debug)]
pub struct GenericLabels(HashMap<String, u8>);

impl Deref for GenericLabels {
    type Target = HashMap<String, u8>;
    fn deref(&self) -> &Self::Target { &self.0 }
}

impl DerefMut for GenericLabels {
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.0 }
}

pub fn file(mut lexer: Context) -> Script {
    let mut declarations = Vec::new();

    loop {
        let Some(Tok::Ident(decl_name)) = lexer.next() else { panic!() };

        lexer.generic_labels = parse_generics(&mut lexer);

        match lexer.peek() {
            Some(Tok::LeftParen) => {
                declarations.push(Declaration::Func(parse_func(
                    &mut lexer, decl_name, false,
                )));
            },
            Some(Tok::LeftCurly) => {
                let parse_output = parse_advanced_alias(
                    &mut lexer,
                    &mut StructParsingContext {
                        name: decl_name.clone(),
                        dependencies: HashSet::new(),
                    },
                );

                let strct = TypeDecl {
                    name: decl_name,
                    typ: parse_output.alias,
                    contains_generics: lexer.generic_labels.len() > 0,
                    dependencies: parse_output.dependencies,
                };

                declarations.push(Declaration::Type(strct));

                for m in parse_output.methods {
                    declarations.push(Declaration::Func(m));
                }
            },
            _ => panic!(),
        }

        if lexer.peek().is_none() {
            return Script(declarations);
        }
    }
}

pub fn parse_generics(lexer: &mut Context) -> HashMap<String, u8> {
    if lexer.peek() == Some(Tok::LeftAngle) {
        parse_list(
            (Tok::LeftAngle, Tok::RghtAngle),
            Some(Tok::Comma),
            parse_generic_label,
            lexer,
        )
        .iter()
        .enumerate()
        .map(|(i, s)| (s.clone(), i as u8))
        .collect()
    } else {
        HashMap::new()
    }
}

pub fn parse_generic_label(lexer: &mut Context) -> String {
    match lexer.next().unwrap() {
        Tok::Ident(name) => name,
        _ => panic!(),
    }
}

pub fn parse_func(lexer: &mut Context, name: String, is_method: bool) -> FuncDecl {
    let args = parse_list(
        (Tok::LeftParen, Tok::RghtParen),
        Some(Tok::Comma),
        parse_var_decl,
        lexer,
    );

    assert_eq!(lexer.next(), Some(Tok::Colon));

    let ret_type = parse_type_alias(lexer);

    let code = parse_block(lexer);

    FuncDecl {
        name,
        ret_type,
        args,
        code,
        is_method,
        contains_generics: lexer.generic_labels.len() > 0,
        dependencies: lexer.func_dependencies.clone(),
        generics: Vec::new(),
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

fn parse_var_decl(lexer: &mut Context) -> VarDecl {
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

fn parse_assignable(lexer: &mut Context) -> Assignable {
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

fn parse_block(lexer: &mut Context) -> Expr {
    Expr::Block(parse_list(
        (Tok::LeftCurly, Tok::RghtCurly),
        None,
        |lexer| parse_stmt(lexer),
        lexer,
    ))
}

fn parse_stmt(lexer: &mut Context) -> Expr {
    let next = lexer.peek().unwrap();
    let after_that = lexer.peek_by(1).unwrap();

    match (next, after_that) {
        (Tok::Ident(_), Tok::LeftBrack | Tok::Colon | Tok::Assignment) => {
            parse_setvar(lexer)
        },
        _ => parse_expr(lexer),
    }
}

fn parse_setvar(lexer: &mut Context) -> Expr {
    let assignable = parse_assignable(lexer);

    assert_eq!(lexer.next(), Some(Tok::Assignment));

    let rhs = parse_expr(lexer);

    match assignable {
        Assignable::Declare(d) => Expr::MakeVar(d, box rhs),
        Assignable::Get(lhs) => Expr::SetVar(box lhs, box rhs),
    }
}

fn parse_expr(lexer: &mut Context) -> Expr {
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

fn parse_for(lexer: &mut Context) -> Expr {
    assert_eq!(lexer.next(), Some(Tok::At));

    let w = parse_expr(lexer);
    let d = parse_block(lexer);

    Expr::ForWhile(Box::new(w), Box::new(d))
}

fn parse_if(lexer: &mut Context) -> Expr {
    assert_eq!(lexer.next(), Some(Tok::Question));

    let i = parse_expr(lexer);
    let t = parse_block(lexer);

    Expr::IfThen(Box::new(i), Box::new(t))
}
