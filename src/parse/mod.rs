pub use crate::lex::ParseContext;
pub use crate::lex::TokenStream;
use crate::lex::{Lexer, Tok, TypeName, VarName};
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

pub fn file(mut lexer: Lexer) -> Result<Script, ParseError> {
    let mut declarations: Vec<Decl> = Vec::new();

    loop {
        match lexer.next_tok()? {
            Tok::VarName(name) => {
                let generic_labels = parse_generics(&mut lexer)?;
                let context = lexer.split(name, generic_labels);

                declarations.push(Decl::Func(parse_func(context, None)?));
            },
            Tok::TypeName(name) => {
                let generic_labels = parse_generics(&mut lexer)?;
                let context = lexer.split(name, generic_labels);

                let (strct, methods) = parse_type_decl(context)?;

                declarations.push(Decl::Type(strct));

                for m in methods {
                    declarations.push(Decl::Func(m));
                }
            },
            err => {
                return Err(ParseError::UnexpectedTok {
                    got: err,
                    expected: vec![TokName::VarName, TokName::TypeName],
                });
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
        let list = parse_list(
            (Tok::LeftAngle, Tok::RghtAngle),
            Some(Tok::Comma),
            parse_generic_label,
            lexer,
        )?
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
    mut lexer: ParseContext<VarName>,
    method_of: Option<TypeName>,
) -> Result<FuncCode, ParseError> {
    let args = parse_list(
        (Tok::LeftParen, Tok::RghtParen),
        Some(Tok::Comma),
        parse_var_decl,
        &mut lexer,
    )?;

    assert_or_error(lexer.next_tok()?, Tok::Colon)?;

    let ret_type = parse_type_alias(&mut lexer)?;

    let code = parse_block(&mut lexer)?;

    let generic_count = lexer.generic_count() as u32;
    let (name, dependencies, _) = lexer.return_info();

    Ok(FuncCode {
        name,
        ret_type,
        args,
        code,
        generic_count,
        method_of,
        dependencies,
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
        typ: type_spec,
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
            typ: type_spec,
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

    assert_eq!(lexer.next_tok()?, Tok::Assignment);

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
        | Tok::LeftBrack => parse_math_expr(lexer),
        tok if tok.is_un_op() => parse_math_expr(lexer),
        Tok::At => parse_for(lexer),
        Tok::Question => parse_if(lexer),
        Tok::Bang => {
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
                TokName::Bang,
            ],
        }),
    }
}

fn parse_for(ctx: &mut ParseContext<VarName>) -> Result<Expr, ParseError> {
    assert_eq!(ctx.next_tok()?, Tok::At);

    let w = parse_expr(ctx)?;
    let d = parse_block(ctx)?;

    Ok(Expr::ForWhile(box w, box d))
}

fn parse_if(ctx: &mut ParseContext<VarName>) -> Result<Expr, ParseError> {
    assert_eq!(ctx.next_tok()?, Tok::Question);

    let i = parse_expr(ctx)?;
    let t = parse_block(ctx)?;

    Ok(Expr::IfThen(box i, box t))
}
