pub use crate::lex::ParseContext;
pub use crate::lex::TokenStream;
use crate::lex::{Lexer, Tok, TypeName, VarName};
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

pub type GenericLabels = HashMap<TypeName, u8>;

pub fn file(mut lexer: Lexer) -> Script {
    let mut declarations: Vec<Decl> = Vec::new();

    loop {
        match lexer.next_tok() {
            Some(Tok::VarName(name)) => {
                let generic_labels = parse_generics(&mut lexer);
                let context = lexer.split(name, generic_labels);

                declarations.push(parse_maybe_gen_func(context, false).into());
            },
            Some(Tok::TypeName(name)) => {
                let generic_labels = parse_generics(&mut lexer);
                let context = lexer.split(name, generic_labels);

                let (strct, methods) = parse_type_decl(context);

                declarations.push(Decl::Type(strct));

                for m in methods {
                    declarations.push(m);
                }
            },
            _ => panic!(),
        }

        if lexer.peek_tok().is_none() {
            return Script(declarations);
        }

        if crate::DEBUG {
            println!("moving to the next declaration!");
        }
    }
}

pub fn parse_generics(lexer: &mut Lexer) -> GenericLabels {
    if lexer.peek_tok() == Some(Tok::LeftAngle) {
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

pub fn parse_generic_label(lexer: &mut Lexer) -> TypeName {
    match lexer.next_tok().unwrap() {
        Tok::TypeName(name) => name,
        _ => panic!(),
    }
}

pub fn parse_maybe_gen_func(
    lexer: ParseContext<VarName>,
    is_method: bool,
) -> SomeFuncDecl {
    if lexer.has_generics() {
        SomeFuncDecl::Gen(parse_gen_func(lexer, is_method))
    } else {
        SomeFuncDecl::Func(parse_func(lexer, is_method))
    }
}

pub fn parse_gen_func(
    mut lexer: ParseContext<VarName>,
    is_method: bool,
) -> GenFuncDecl {
    let args = parse_list(
        (Tok::LeftParen, Tok::RghtParen),
        Some(Tok::Comma),
        parse_var_decl,
        &mut lexer,
    );

    assert_eq!(lexer.next_tok(), Some(Tok::Colon));

    let ret_type = parse_type_alias(&mut lexer);

    let code = parse_block(&mut lexer);

    let (name, dependencies, _) = lexer.return_info();

    GenFuncDecl {
        name,
        ret_type,
        args,
        code,
        is_method,
        dependencies,
    }
}

pub fn parse_func(mut lexer: ParseContext<VarName>, is_method: bool) -> FuncDecl {
    let args = parse_list(
        (Tok::LeftParen, Tok::RghtParen),
        Some(Tok::Comma),
        parse_var_decl,
        &mut lexer,
    );

    assert_eq!(lexer.next_tok(), Some(Tok::Colon));

    let ret_type = parse_type_alias(&mut lexer);

    let code = parse_block(&mut lexer);

    let (name, dependencies, _) = lexer.return_info();

    FuncDecl {
        name,
        ret_type,
        args,
        code,
        is_method,
        dependencies,
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

fn parse_var_decl(lexer: &mut ParseContext<VarName>) -> VarDecl {
    let Some(Tok::VarName(var_name)) = lexer.next_tok() else { panic!() };

    let type_spec = match lexer.peek_tok() {
        Some(Tok::Colon) => {
            lexer.next_tok();

            Some(parse_type_alias(lexer))
        },
        _ => None,
    };

    VarDecl {
        var_name,
        type_spec,
    }
}

fn parse_assignable(lexer: &mut ParseContext<VarName>) -> Assignable {
    let lhs = parse_expr(lexer);

    if let Expr::Ident(var_name) = lhs {
        let type_spec = match lexer.peek_tok() {
            Some(Tok::Colon) => {
                lexer.next_tok();

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

fn parse_block(lexer: &mut ParseContext<VarName>) -> Expr {
    Expr::Block(parse_list(
        (Tok::LeftCurly, Tok::RghtCurly),
        None,
        parse_stmt,
        lexer,
    ))
}

fn parse_stmt(lexer: &mut ParseContext<VarName>) -> Expr {
    let next = lexer.peek_tok().unwrap();
    let after_that = lexer.peek_by(1).unwrap();

    match (next, after_that) {
        (
            Tok::VarName(_),
            Tok::LeftBrack | Tok::Dot | Tok::Colon | Tok::Assignment,
        ) => parse_setvar(lexer),
        _ => parse_expr(lexer),
    }
}

fn parse_setvar(lexer: &mut ParseContext<VarName>) -> Expr {
    let assignable = parse_assignable(lexer);

    assert_eq!(lexer.next_tok(), Some(Tok::Assignment));

    let rhs = parse_expr(lexer);

    match assignable {
        Assignable::Declare(d) => Expr::MakeVar(d, box rhs),
        Assignable::Get(lhs) => Expr::SetVar(box lhs, box rhs),
    }
}

fn parse_expr(lexer: &mut ParseContext<VarName>) -> Expr {
    match lexer.peek_tok() {
        Some(Tok::VarName(_))
        | Some(Tok::TypeName(_))
        | Some(Tok::Int(_))
        | Some(Tok::Float(_))
        | Some(Tok::LeftBrack) => parse_math_expr(lexer),
        Some(tok) if tok.is_un_op() => parse_math_expr(lexer),
        Some(Tok::At) => parse_for(lexer),
        Some(Tok::Question) => parse_if(lexer),
        Some(Tok::Bang) => {
            lexer.next_tok();
            Expr::Return(box parse_expr(lexer))
        },
        Some(_) => todo!(),
        None => panic!(),
    }
}

fn parse_for(ctx: &mut ParseContext<VarName>) -> Expr {
    assert_eq!(ctx.next_tok(), Some(Tok::At));

    let w = parse_expr(ctx);
    let d = parse_block(ctx);

    Expr::ForWhile(box w, box d)
}

fn parse_if(ctx: &mut ParseContext<VarName>) -> Expr {
    assert_eq!(ctx.next_tok(), Some(Tok::Question));

    let i = parse_expr(ctx);
    let t = parse_block(ctx);

    Expr::IfThen(box i, box t)
}
