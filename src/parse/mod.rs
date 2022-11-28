pub use crate::lex::ParseContext;
pub use crate::lex::TokenStream;
use crate::lex::{Lexer, Tok, TypeName, VarName};
use crate::typ::TypeOrAlias;
pub use opcode::Opcode;
use std::collections::HashMap;
use std::collections::HashSet;
pub use std::iter::Peekable;

mod error;
mod expression;
mod list;
mod opcode;
mod parsing_data;
mod structure;

pub use error::*;
pub use expression::*;
pub use list::*;
pub use opcode::*;
pub use parsing_data::*;
pub use structure::*;

pub type GenericLabels = HashMap<TypeName, u8>;

pub fn merge(one: &GenericLabels, another: &GenericLabels) -> GenericLabels {
    one.iter()
        .chain(another.iter())
        .map(|(n, i)| (n.clone(), i.clone()))
        .collect()
}

pub fn file(mut lexer: Lexer) -> Result<Script, ParseError> {
    let mut declarations: Vec<Decl> = Vec::new();

    loop {
        match lexer.peek_tok()? {
            Tok::VarName(_) => {
                declarations.push(Decl::Func(parse_func(
                    &mut lexer,
                    None,
                    GenericLabels::new(),
                )?));
            },
            _ => {
                // could either the type we are declaring methods for, as in:
                //
                // Struct.this(): # .. code
                //
                // or it could be the type we are declaring, as in:
                //
                // Struct = { x: i32, y: i32 }
                let generic_labels = parse_generics(&mut lexer)?;

                if matches!(lexer.peek_by(1)?, Tok::LeftAngle | Tok::Assignment)
                    && generic_labels.len() == 0
                {
                    let type_name = lexer.get_type_name_next()?;
                    let generic_labels = parse_generics(&mut lexer)?;

                    lexer.assert_next_tok_is(Tok::Assignment)?;

                    let context = lexer.split(type_name, generic_labels);

                    let type_decl = parse_type_decl(context)?;

                    declarations.push(Decl::Type(type_decl));
                } else {
                    let mut method_of_parser =
                        lexer.split(VarName::temp(), generic_labels.clone());
                    let method_of = parse_type_alias(&mut method_of_parser)?;

                    lexer.assert_next_tok_is(Tok::Dot)?;

                    let methods = parse_one_or_list(
                        Tok::curlys(),
                        None,
                        move |lexer| {
                            parse_func(
                                lexer,
                                Some(method_of.clone()),
                                generic_labels.clone(),
                            )
                        },
                        &mut lexer,
                    )?;

                    for method in methods {
                        declarations.push(Decl::Func(method));
                    }
                }
            },
        }

        if lexer.peek_tok().is_err() {
            return Ok(Script(declarations));
        }

        if crate::DEBUG {
            println!("moving to the next declaration!");
        }
    }
}

pub fn parse_generics(lexer: &mut Lexer) -> Result<GenericLabels, ParseError> {
    if lexer.peek_tok()? == Tok::LeftAngle {
        let list =
            parse_list(Tok::angles(), Some(Tok::Comma), parse_generic_label, lexer)?
                .iter()
                .enumerate()
                .map(|(i, s)| (s.clone(), i as u8))
                .collect();

        Ok(list)
    } else {
        Ok(HashMap::new())
    }
}

pub fn parse_generic_label(lexer: &mut Lexer) -> Result<TypeName, ParseError> {
    match lexer.next_tok()? {
        Tok::TypeName(name) => Ok(name),
        err => Err(ParseError::UnexpectedTok {
            got: err,
            expected: vec![TokName::TypeName],
        }),
    }
}

pub fn parse_func(
    lexer: &mut Lexer,
    method_of: Option<TypeAlias>,
    outer_generics: GenericLabels,
) -> Result<FuncCode, ParseError> {
    let func_name = lexer.get_var_name_next()?;
    let generic_labels = parse_generics(lexer)?;

    let all_generics = merge(&generic_labels, &outer_generics);

    if all_generics.len() != generic_labels.len() + outer_generics.len() {
        todo!("reused generics error")
    }

    let func_context = lexer.split(func_name, all_generics);
    parse_func_code(func_context, method_of)
}

pub fn parse_func_code(
    mut lexer: ParseContext<VarName>,
    method_of: Option<TypeAlias>,
) -> Result<FuncCode, ParseError> {
    let mut args = parse_list(
        (Tok::LeftParen, Tok::RghtParen),
        Some(Tok::Comma),
        parse_var_decl,
        &mut lexer,
    )?;

    assert_or_error(lexer.next_tok()?, Tok::Colon)?;

    let ret_type = parse_type_alias(&mut lexer)?;

    let code = parse_block(&mut lexer)?;

    let generic_count = lexer.generic_count() as u32;
    let (name, _) = lexer.return_info();

    let method_of = method_of.map(|mo| TypeOrAlias::Alias(mo));

    if let Some(method_of) = &method_of {
        args.push(VarDecl {
            name: VarName::from("self"),
            typ: Some(method_of.clone()),
        });
    }

    Ok(FuncCode {
        name,
        ret_type,
        args,
        code,
        generic_count,
        method_of,
    })
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

fn parse_var_decl(lexer: &mut ParseContext<VarName>) -> Result<VarDecl, ParseError> {
    let var_name = lexer.next_tok()?.var_name()?;

    let type_spec = match lexer.peek_tok()? {
        Tok::Colon => {
            lexer.next_tok()?;

            Some(parse_type_alias(lexer)?)
        },
        _ => None,
    };

    Ok(VarDecl {
        name: var_name,
        typ: type_spec.map(|s| s.into()),
    })
}

fn parse_assignable(
    lexer: &mut ParseContext<VarName>,
) -> Result<Assignable, ParseError> {
    let lhs = parse_expr(lexer)?;

    if let Expr::Ident(var_name) = lhs {
        let type_spec = match lexer.peek_tok()? {
            Tok::Colon => {
                lexer.next_tok()?;

                Some(parse_type_alias(lexer)?)
            },
            _ => None,
        };

        Ok(Assignable::Declare(VarDecl {
            name: var_name,
            typ: type_spec.map(|s| s.into()),
        }))
    } else {
        Ok(Assignable::Get(lhs))
    }
}

fn parse_block(lexer: &mut ParseContext<VarName>) -> Result<Expr, ParseError> {
    Ok(Expr::Block(parse_list(
        (Tok::LeftCurly, Tok::RghtCurly),
        None,
        parse_stmt,
        lexer,
    )?))
}

fn parse_stmt(lexer: &mut ParseContext<VarName>) -> Result<Expr, ParseError> {
    let next = lexer.peek_tok()?;
    let after_that = lexer.peek_by(1)?;

    match (next, after_that) {
        (
            Tok::VarName(_),
            Tok::LeftBrack | Tok::Dot | Tok::Colon | Tok::Assignment,
        ) => parse_setvar(lexer),
        _ => parse_expr(lexer),
    }
}

fn parse_setvar(lexer: &mut ParseContext<VarName>) -> Result<Expr, ParseError> {
    let assignable = parse_assignable(lexer)?;

    lexer.assert_next_tok_is(Tok::Assignment)?;

    let rhs = parse_expr(lexer)?;

    match assignable {
        Assignable::Declare(d) => Ok(Expr::MakeVar(d, box rhs)),
        Assignable::Get(lhs) => Ok(Expr::SetVar(box lhs, box rhs)),
    }
}

pub fn parse_expr(lexer: &mut ParseContext<VarName>) -> Result<Expr, ParseError> {
    match lexer.peek_tok()? {
        Tok::VarName(_)
        | Tok::TypeName(_)
        | Tok::Int(_)
        | Tok::Float(_)
        | Tok::Bool(_)
        | Tok::Strin(_)
        | Tok::LeftBrack
        | Tok::LeftParen => parse_math_expr(lexer),
        tok if tok.is_un_op() => parse_math_expr(lexer),
        Tok::At => parse_for(lexer),
        Tok::Question => parse_if(lexer),
        Tok::Semicolon => {
            lexer.next_tok()?;
            Ok(Expr::Return(box parse_expr(lexer)?))
        },
        err => Err(ParseError::UnexpectedTok {
            got: err,
            expected: vec![
                TokName::VarName,
                TokName::TypeName,
                TokName::Int,
                TokName::Float,
                TokName::LeftBrack,
                TokName::UnaryOperator,
                TokName::At,
                TokName::Question,
                TokName::Semicolon,
            ],
        }),
    }
}

fn parse_for(ctx: &mut ParseContext<VarName>) -> Result<Expr, ParseError> {
    ctx.assert_next_tok_is(Tok::At)?;

    let w = parse_expr(ctx)?;
    let d = parse_block(ctx)?;

    Ok(Expr::ForWhile(box w, box d))
}

fn parse_if(ctx: &mut ParseContext<VarName>) -> Result<Expr, ParseError> {
    ctx.assert_next_tok_is(Tok::Question)?;

    let i = parse_expr(ctx)?;
    let t = parse_block(ctx)?;

    if ctx.peek_tok()? == Tok::Colon {
        ctx.next_tok()?;

        let e = parse_block(ctx)?;

        Ok(Expr::IfThenElse(box i, box t, box e))
    } else {
        Ok(Expr::IfThen(box i, box t))
    }
}
