use crate::lex::{Tok, TypeName, VarName};
use crate::Type;
pub use context::{FuncParseContext, GlobalParseContext, ParseContext, TypeParseContext};
pub use opcode::Opcode;
use std::collections::HashMap;
pub use std::iter::Peekable;

pub mod context;
mod error;
mod expression;
mod list;
mod opcode;
mod parsing_data;
mod structure;

pub use error::ParseError;
pub(super) use error::*;
pub(super) use expression::*;
pub(super) use list::*;
pub use opcode::*;
pub use parsing_data::*;
pub(super) use structure::*;

pub fn parse(mut lexer: GlobalParseContext) -> Result<Script, Vec<ParseErrorSpanned>> {
    let script = file(&mut lexer);

    let errors = lexer.errors.deref().unwrap();

    if errors.len() == 0 {
        match script {
            Ok(script) => Ok(script),
            Err(error) => Err(vec![ParseErrorSpanned {
                start: 0,
                end: lexer.inner.len(),
                tokens_between: TokenSpan((*lexer.inner).clone()),
                error,
            }]),
        }
    } else {
        Err(errors.clone())
    }
}

pub fn file(lexer: &mut GlobalParseContext) -> ParseResult<Script> {
    let mut declarations: Vec<Decl> = Vec::new();

    loop {
        match lexer.peek_tok()? {
            Tok::VarName(_) => {
                declarations.push(Decl::Func(parse_func(
                    lexer,
                    TypeSpecRelation::Unrelated,
                    GenericLabels::new(),
                )?));
            },
            _ => {
                // could either the type we are declaring methods for, as in:
                //
                // Struct.this() # .. code
                //
                // or it could be the type we are declaring, as in:
                //
                // Struct = { x: i32, y: i32 }
                let generic_labels = parse_generics(lexer)?;

                if matches!(lexer.peek_by(1)?, Tok::LAngle | Tok::Assignment)
                    && generic_labels.len() == 0
                {
                    let type_name = lexer.next_tok()?.type_name()?;
                    let generic_labels = parse_generics(lexer)?;

                    lexer.assert_next_tok_is(Tok::Assignment)?;

                    let context = lexer.split(type_name, generic_labels);

                    let type_decl = parse_type_decl(context)?;

                    declarations.push(Decl::Type(type_decl));
                } else {
                    let method_of = {
                        let mut method_of_parser = lexer
                            .split(VarName::from("beginning_of_impl"), generic_labels.clone());
                        parse_type_spec(&mut method_of_parser)?
                    };

                    let relation = match lexer.next_tok()? {
                        Tok::ColonDot => TypeRelationGeneric::MethodOf(method_of.clone()),
                        Tok::DoubleColon => TypeRelationGeneric::Static(method_of.clone()),
                        got => {
                            return Err(ParseError::UnexpectedTok {
                                got,
                                expected: vec![TokName::ColonDot, TokName::DoubleColon],
                            })
                        },
                    };

                    let methods = parse_one_or_list(
                        Tok::curlys(),
                        None,
                        move |lexer| {
                            parse_func(lexer, relation.clone(), generic_labels.clone())
                        },
                        lexer,
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

        if crate::XC_DEBUG {
            println!("moving to the next declaration!");
        }
    }
}

pub fn parse_generics(lexer: &mut GlobalParseContext) -> ParseResult<GenericLabels> {
    if lexer.peek_tok()? == Tok::LAngle {
        let list = parse_list(Tok::angles(), Some(Tok::Comma), parse_generic_label, lexer)?
            .iter()
            .enumerate()
            .map(|(i, s)| (s.clone(), i as u8))
            .collect();

        Ok(list)
    } else {
        Ok(HashMap::new())
    }
}

pub fn parse_generic_label(lexer: &mut GlobalParseContext) -> ParseResult<TypeName> {
    match lexer.next_tok()? {
        Tok::TypeName(name) => Ok(name),
        err => Err(ParseError::UnexpectedTok {
            got: err,
            expected: vec![TokName::TypeName],
        }),
    }
}

pub fn parse_func(
    lexer: &mut GlobalParseContext,
    relation: TypeSpecRelation,
    outer_generics: GenericLabels,
) -> Result<FuncCode, ParseError> {
    let func_name = lexer.next_tok()?.var_name()?;
    let generic_labels = parse_generics(lexer)?;

    let all_generics = merge(&generic_labels, &outer_generics);

    if all_generics.len() != generic_labels.len() + outer_generics.len() {
        todo!("reused generics error")
    }

    let func_context = lexer.split(func_name, all_generics);
    parse_func_code(func_context, relation)
}

pub fn parse_func_code(
    mut lexer: FuncParseContext,
    relation: TypeSpecRelation,
) -> ParseResult<FuncCode> {
    let mut args =
        parse_list((Tok::LParen, Tok::RParen), Some(Tok::Comma), parse_var_decl, &mut lexer)?;

    let ret_type = if lexer.peek_tok()? == Tok::Semicolon {
        lexer.next_tok()?;
        parse_type_spec(&mut lexer)?
    } else {
        TypeSpec::Void
    };

    let code = parse_block(&mut lexer)?;

    let generic_count = lexer.generic_count() as u32;

    if let TypeSpecRelation::MethodOf(relation) = &relation {
        args.push(VarDecl {
            name: VarName::from("self"),
            type_spec: relation.clone(),
        });
    }

    Ok(FuncCode {
        name: lexer.name.clone(),
        ret_type: ret_type.into(),
        args,
        code,
        generic_count,
        relation,
    })
}

fn parse_var_decl(lexer: &mut FuncParseContext) -> ParseResult<VarDecl> {
    let var_name = lexer.next_tok()?.var_name()?;

    let type_spec = match lexer.peek_tok()? {
        Tok::Colon => {
            lexer.next_tok()?;

            parse_type_spec(lexer)?
        },
        _ => TypeSpec::unknown(),
    };

    Ok(VarDecl {
        name: var_name,
        type_spec,
    })
}

fn parse_block(lexer: &mut FuncParseContext) -> ParseResult<Expr> {
    Ok(Expr::Block(parse_list((Tok::LCurly, Tok::RCurly), None, parse_stmt, lexer)?))
}

fn parse_stmt(lexer: &mut FuncParseContext) -> ParseResult<Expr> {
    fn inner(lexer: &mut FuncParseContext) -> ParseResult<Expr> {
        let lhs = parse_expr(lexer)?;

        if matches!(lhs, Expr::Ident(_)) {
            let Expr::Ident(var_name) = lhs else { todo!("new err type") };

            let var = if lexer.next_tok()? == Tok::Assignment {
                VarDecl {
                    name: var_name,
                    type_spec: TypeSpec::Type(Type::unknown()),
                }
            } else {
                let type_spec = parse_type_spec(lexer)?;
                lexer.assert_next_tok_is(Tok::Assignment)?;
                VarDecl {
                    name: var_name,
                    type_spec,
                }
            };

            let rhs = parse_expr(lexer)?;
            Ok(Expr::SetVar(var, box rhs))
        } else if lexer.peek_tok()? == Tok::Assignment {
            lexer.assert_next_tok_is(Tok::Assignment)?;

            let rhs = parse_expr(lexer)?;

            Ok(Expr::Set(box lhs, box rhs))
        } else {
            Ok(lhs)
        }
    }

    lexer.recover(inner, vec![Tok::Return])
}

pub fn parse_expr(lexer: &mut FuncParseContext) -> ParseResult<Expr> {
    match lexer.peek_tok()? {
        Tok::VarName(_)
        | Tok::TypeName(_)
        | Tok::Int(_)
        | Tok::DottedNum(_)
        | Tok::Float(_)
        | Tok::Bool(_)
        | Tok::Strin(_)
        | Tok::LBrack
        | Tok::LParen
        | Tok::LCurly => parse_math_expr(lexer),
        tok if tok.is_unary_op() => parse_math_expr(lexer),
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
                TokName::LBrack,
                TokName::UnaryOperator,
                TokName::At,
                TokName::Question,
                TokName::Semicolon,
                TokName::LCurly,
            ],
        }),
    }
}

fn parse_for(ctx: &mut FuncParseContext) -> ParseResult<Expr> {
    ctx.assert_next_tok_is(Tok::At)?;

    let w = parse_expr(ctx)?;
    let d = parse_block(ctx)?;

    Ok(Expr::ForWhile(box w, box d))
}

fn parse_if(ctx: &mut FuncParseContext) -> ParseResult<Expr> {
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
