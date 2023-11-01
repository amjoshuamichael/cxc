use crate::{lex::{Tok, TypeName, VarName}, typ::ABI};

pub use context::{FuncParseContext, GlobalParseContext, ParseContext, TypeParseContext};
pub use opcode::Opcode;
use std::{collections::HashMap, rc::Rc};
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

use self::context::FuncParseData;

pub fn parse(mut lexer: GlobalParseContext) -> Result<Script, Vec<ParseErrorSpanned>> {
    let script = file(&mut lexer);

    let errors = lexer.errors.deref().unwrap();

    if errors.is_empty() {
        match script {
            Ok(script) => Ok(script),
            Err(error) => Err(vec![ParseErrorSpanned {
                start: 0,
                end: lexer.spans.last().unwrap().1,
                tokens_between: TokenSpan::new(&lexer.tokens, 0, lexer.tokens.len()),
                error,
            }]),
        }
    } else {
        Err(errors.clone())
    }
}

pub fn file(lexer: &mut GlobalParseContext) -> ParseResult<Script> {
    let mut script = Script::default();

    if lexer.peek_tok().is_err() {
        // file is empty
        return Ok(Script::default());
    }

    loop {
        match lexer.peek_tok()?.clone() {
            Tok::VarName(_) => {
                script.funcs.push(lexer.recover(|lexer| parse_func(
                    lexer,
                    TypeSpecRelation::Unrelated,
                    GenericLabels::new(),
                )));
            },
            _ if lexer.move_on(Tok::TripleMinus) => {
                let mut func_parser = lexer.split(
                    FuncParseData {
                        name: VarName::from("comp_script"),
                        has_return: false,
                    },
                    HashMap::new()
                );

                let mut statements = Vec::new();

                while lexer.peek_tok() != Err(ParseError::UnexpectedEndOfFile) {
                    statements.push(parse_stmt(&mut func_parser)?);
                }

                let code = FuncCode {
                    ret_type: TypeSpec::Void,
                    ..FuncCode::from_expr(Rc::new(Expr::Block(statements)))
                };

                script.comp_script = Some(code);
            }
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
                    && generic_labels.is_empty()
                {
                    lexer.recover(|lexer| {
                        let type_name = lexer.next_tok()?.type_name()?;
                        let generic_labels = parse_generics(lexer)?;

                        lexer.assert_next_tok_is(Tok::Assignment, TokName::Assignment)?;

                        let context = lexer.split(type_name, generic_labels);

                        let type_decl = parse_type_decl(context)?;

                        script.types.push(type_decl);

                        Ok(())
                    });
                } else {
                    let method_of = {
                        let mut method_of_parser = lexer
                            .split(TypeName::Anonymous, generic_labels.clone());
                        parse_type(&mut method_of_parser)?
                    };

                    let relation = match lexer.next_tok()? {
                        Tok::ColonDot => TypeRelationGeneric::MethodOf(method_of.clone()),
                        Tok::DoubleColon => TypeRelationGeneric::Static(method_of.clone()),
                        got => {
                            return ParseError::unexpected(got, vec![TokName::Assignment, TokName::ColonDot, TokName::DoubleColon]);
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
                        script.funcs.push(method);
                    }
                }
            },
        }

        if lexer.peek_tok() == Err(ParseError::UnexpectedEndOfFile) {
            return Ok(script);
        }

        #[cfg(feature = "xc-debug")]
        {
            println!("\\ \\Next declaration");
        }
    }
}

pub fn parse_generics(lexer: &mut GlobalParseContext) -> ParseResult<GenericLabels> {
    if lexer.peek_tok()? == &Tok::LAngle {
        let list = parse_list(Tok::angles(), COMMAS, parse_generic_label, lexer)?
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
        Tok::TypeName(name) => Ok(name.clone()),
        got => ParseError::unexpected(got, vec![TokName::TypeName]),
    }
}

pub fn parse_func(
    glexer: &mut GlobalParseContext,
    relation: TypeSpecRelation,
    outer_generics: GenericLabels,
) -> ParseResult<FuncCode> {
    let func_name = glexer.next_tok()?.var_name()?;
    let generic_labels = parse_generics(glexer)?;

    let all_generics = merge(&generic_labels, &outer_generics);

    if all_generics.len() != generic_labels.len() + outer_generics.len() {
        todo!("reused generics error")
    }

    glexer.generic_labels = all_generics.clone();

    let mut args = parse_list(Tok::parens(), COMMAS, parse_var_decl, glexer)?;

    let ret_type = match glexer.peek_tok()? {
        Tok::Semicolon => {
            glexer.next_tok()?;
            parse_type_spec(glexer)?
        },
        Tok::LCurly => {
            TypeSpec::Void
        }
        got => {
            return ParseError::unexpected(got, vec![TokName::Semicolon, TokName::LCurly]);
        }
    };

    let mut flexer = glexer.split(FuncParseData {
        name: func_name,
        has_return: ret_type != TypeSpec::Void,
    }, all_generics);

    let code = parse_block(&mut flexer)?;

    let generic_count = flexer.generic_count();

    if let TypeSpecRelation::MethodOf(relation) = &relation {
        args.insert(0, VarDecl {
            name: VarName::from("self"),
            type_spec: relation.clone(),
        });
    }

    Ok(FuncCode {
        name: flexer.inner_data.name.clone(),
        ret_type,
        args,
        code: Rc::new(code),
        generic_count,
        relation,
        is_external: false,
        abi: ABI::C,
    })
}

fn parse_var_decl<T: Clone>(lexer: &mut ParseContext<T>) -> ParseResult<VarDecl> {
    let var_name = lexer.next_tok()?.var_name()?;

    let type_spec = match lexer.peek_tok()? {
        Tok::Colon => {
            lexer.next_tok()?;

            parse_type_spec(lexer)?
        },
        _ => TypeSpec::Unknown,
    };

    Ok(VarDecl {
        name: var_name,
        type_spec,
    })
}

fn parse_block(lexer: &mut FuncParseContext) -> ParseResult<Expr> {
    Ok(Expr::Block(parse_list(Tok::curlys(), None, parse_stmt, lexer)?))
}

fn parse_stmt(lexer: &mut FuncParseContext) -> ParseResult<Expr> {
    Ok(lexer.recover(|lexer| {
        if lexer.move_on(Tok::With) {
            let with_expr = parse_expr(lexer)?;

            if lexer.move_on(Tok::As) {
                let alias_name = lexer.next_tok()?.var_name()?;
                return Ok(Expr::WithAs(Box::new(with_expr), alias_name));
            } else {
                return Ok(Expr::With(Box::new(with_expr)));
            }
        }

        let lhs = parse_expr(lexer)?;

        if matches!(lhs, Expr::Ident(_)) && lexer.peek_tok()? == &Tok::Colon {
            let Expr::Ident(var_name) = lhs else { todo!("new err type") };

            lexer.assert_next_tok_is(Tok::Colon, TokName::Colon)?;

            let var = if lexer.peek_tok()? == &Tok::Assignment {
                lexer.next_tok()?;

                VarDecl {
                    name: var_name,
                    type_spec: TypeSpec::Unknown,
                }
            } else {
                let type_spec = lexer.recover_with(vec![&Tok::Assignment], parse_type_spec);

                lexer.assert_next_tok_is(Tok::Assignment, TokName::VarAssignment)?;

                VarDecl {
                    name: var_name,
                    type_spec,
                }
            };

            let rhs = lexer.recover(parse_expr);
            Ok(Expr::SetVar(var, Box::new(rhs)))
        } else if lexer.move_on(Tok::Assignment) {
            let rhs = lexer.recover(parse_expr);

            Ok(Expr::Set(Box::new(lhs), Box::new(rhs)))
        } else {
            Ok(lhs)
        }
    }))
}

pub fn parse_expr(lexer: &mut FuncParseContext) -> ParseResult<Expr> {
    match lexer.peek_tok()?.clone() {
        Tok::VarName(_)
        | Tok::Label(_)
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
        _ if lexer.move_on(Tok::At) => parse_while(lexer),
        _ if lexer.move_on(Tok::Question) => parse_if(lexer),
        _ if lexer.move_on(Tok::For) => parse_for(lexer),
        _ if lexer.move_on(Tok::Semicolon) => {
            if lexer.inner_data.has_return || matches!(lexer.peek_tok()?, Tok::Label(_)) {
                Ok(Expr::Return(Some(Box::new(parse_expr(lexer)?))))
            } else {
                Ok(Expr::Return(None))
            }
        },
        got => {
            return ParseError::unexpected(&got, vec![
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
            ]);
        },
    }
}

fn parse_for(ctx: &mut FuncParseContext) -> ParseResult<Expr> {
    let for_ = parse_expr(ctx)?;
    let as_ = ctx.recover_with(vec![&Tok::RCurly], |lexer| {
        if lexer.move_on(Tok::As) {
            Ok(Some(lexer.next_tok()?.var_name()?))
        } else {
            Ok(None)
        }
    });
    let do_ = parse_block(ctx)?;

    Ok(Expr::For(Box::new(for_), as_, Box::new(do_)))
}

fn parse_while(ctx: &mut FuncParseContext) -> ParseResult<Expr> {
    let w = parse_expr(ctx)?;
    let d = parse_block(ctx)?;

    Ok(Expr::While(Box::new(w), Box::new(d)))
}

fn parse_if(ctx: &mut FuncParseContext) -> ParseResult<Expr> {
    let i = parse_expr(ctx)?;
    let t = parse_block(ctx)?;

    if ctx.peek_tok()? == &Tok::Colon {
        ctx.next_tok()?;

        let e = parse_block(ctx)?;

        Ok(Expr::IfThenElse(Box::new(i), Box::new(t), Box::new(e)))
    } else {
        Ok(Expr::IfThen(Box::new(i), Box::new(t)))
    }
}
