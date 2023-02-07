use super::*;

#[derive(Clone)]
enum Atom {
    Expr(Expr),
    Op(Opcode),
}

impl Into<Atom> for Expr {
    fn into(self) -> Atom { Atom::Expr(self) }
}

impl Atom {
    fn unwrap_expr(self) -> ParseResult<Expr> {
        let Atom::Expr(expr) = self else { return Err(ParseError::ImproperExpression) };
        Ok(expr)
    }

    fn unwrap_op(self) -> ParseResult<Opcode> {
        let Atom::Op(op) = self else { return Err(ParseError::ImproperExpression) };
        Ok(op)
    }
}

pub fn parse_math_expr(lexer: &mut FuncParseContext) -> ParseResult<Expr> {
    let mut atoms = Vec::<Atom>::new();

    let mut last_atom = Atom::Op(Opcode::Plus);

    while let Ok(next) = lexer.peek_tok() {
        let atom = if matches!(last_atom, Atom::Op(_)) {
            // if the last atom was an operator, we expect these
            let atom = match next {
                Tok::Int(val) => Expr::Number(val).into(),
                Tok::Float(val) => Expr::Float(val).into(),
                Tok::DottedNum(_) => {
                    // convert dotted int back into float
                    Expr::Float(next.to_string().parse().unwrap()).into()
                },
                Tok::Bool(val) => Expr::Bool(val).into(),
                Tok::Strin(val) => Expr::Strin(val).into(),
                Tok::VarName(val) => Expr::Ident(val.clone()).into(),
                opcode if opcode.is_unary_op() => Atom::Op(opcode.get_un_opcode()?),
                Tok::LBrack => Expr::Array(parse_list(
                    (Tok::LBrack, Tok::RBrack),
                    Some(Tok::Comma),
                    parse_expr,
                    lexer,
                )?)
                .into(),
                Tok::LParen => {
                    lexer.assert_next_tok_is(Tok::LParen)?;
                    let enclosed = Expr::Enclosed(box parse_math_expr(lexer)?).into();
                    lexer.assert_next_tok_is(Tok::RParen)?;
                    enclosed
                },
                _ => {
                    if let Ok(type_spec) = parse_type_spec(lexer) {
                        if lexer.peek_tok()? == Tok::Colon {
                            lexer.next_tok()?;
                            let func_name = lexer.next_tok()?.var_name()?;
                            Expr::StaticMethodPath(type_spec, func_name).into()
                        } else if matches!(
                            lexer.peek_by(1)?,
                            Tok::VarName(_) | Tok::DoublePlus | Tok::DoubleMinus
                        ) {
                            parse_struct_literal(lexer, type_spec)?.into()
                        } else {
                            let exprs = parse_list(
                                Tok::curlys(),
                                Some(Tok::Comma),
                                parse_math_expr,
                                lexer,
                            )?;

                            Expr::Tuple(type_spec, exprs, StructFill::NoFill).into()
                        }
                    } else {
                        break;
                    }
                },
            };

            // we've detected these, but we need to move the token pos forward by one.
            if matches!(
                atom,
                Atom::Expr(Expr::Number(_))
                    | Atom::Expr(Expr::Float(_))
                    | Atom::Expr(Expr::Bool(_))
                    | Atom::Expr(Expr::Strin(_))
                    | Atom::Expr(Expr::Ident(_))
                    | Atom::Op(_)
            ) {
                lexer.next_tok()?;
            }

            atom
        } else if matches!(next, Tok::LAngle) && after_generics(lexer, Tok::LParen)? {
            let generics = parse_list(
                (Tok::LAngle, Tok::RAngle),
                Some(Tok::Comma),
                parse_type_spec,
                lexer,
            )?;

            parse_call(&mut atoms, generics, lexer)?
        } else if matches!(next, Tok::LParen) {
            parse_call(&mut atoms, Vec::new(), lexer)?
        } else if matches!((&last_atom, &next), (Atom::Expr(Expr::Ident(_)), Tok::LBrack)) {
            lexer.next_tok()?;
            let index = parse_expr(lexer)?;
            lexer.assert_next_tok_is(Tok::RBrack)?;

            let object = atoms.pop().unwrap().unwrap_expr()?;

            Expr::Index(box object, box index).into()
        } else if matches!(next, Tok::DottedNum(..)) {
            lexer.next_tok()?;
            // parse tuple index
            let object = atoms.pop().unwrap().unwrap_expr()?;

            let Tok::DottedNum((_, right)) = next else { unreachable!() };

            Expr::Member(box object, right.to_string().into()).into()
        } else if next == Tok::Dot {
            lexer.next_tok()?;
            let previous = atoms
                .pop()
                .ok_or(ParseError::ImproperExpression)?
                .unwrap_expr()?;
            let field = lexer.next_tok()?.var_name()?;

            Expr::Member(box previous, field).into()
        } else if let Ok(opcode) = next.get_bin_opcode() {
            lexer.next_tok()?;

            Atom::Op(opcode)
        } else {
            break;
        };

        last_atom = atom.clone();

        atoms.push(atom.into());
    }

    detect_unary_ops(&mut atoms)?;
    detect_binary_ops(&mut atoms)?;

    if atoms.len() != 1 {
        return Err(ParseError::ImproperExpression);
    }

    Ok(atoms.pop().unwrap().unwrap_expr()?)
}

fn parse_call(
    atoms: &mut Vec<Atom>,
    generics: Vec<TypeSpec>,
    lexer: &mut FuncParseContext,
) -> ParseResult<Atom> {
    let mut args = parse_list(Tok::parens(), Some(Tok::Comma), parse_expr, lexer)?;

    let func = atoms.pop().ok_or(ParseError::ImproperExpression)?;

    let call_expr = match func {
        Atom::Expr(Expr::Member(object, method_name)) => {
            args.push(Expr::UnarOp(Opcode::Ref, object));

            Expr::Call {
                func: box Expr::Ident(method_name),
                generics,
                args,
                is_method: true,
            }
        },
        Atom::Expr(_) => Expr::Call {
            func: box func.unwrap_expr()?,
            generics,
            args,
            is_method: false,
        },
        _ => return Err(ParseError::ImproperExpression),
    };

    Ok(call_expr.into())
}

fn detect_unary_ops(atoms: &mut Vec<Atom>) -> ParseResult<()> {
    for prec_level in 0..=Opcode::MAX_UNARY_PRECEDENT_LEVEL {
        while let Some(opcode_pos) = atoms.iter().rposition(|atom| {
            let Atom::Op(opcode) = atom else { return false };
            opcode.un_prec_level() == Some(prec_level)
        }) {
            if opcode_pos + 1 >= atoms.len() {
                return Err(ParseError::ImproperExpression);
            }

            let opcode = atoms.remove(opcode_pos).unwrap_op()?;

            let rhs = box atoms.remove(opcode_pos).unwrap_expr()?;

            atoms.insert(opcode_pos, Expr::UnarOp(opcode, rhs).into());
        }
    }

    Ok(())
}

fn detect_binary_ops(atoms: &mut Vec<Atom>) -> ParseResult<()> {
    for prec_level in 0..=Opcode::MAX_BINARY_PRECEDENT_LEVEL {
        while let Some(opcode_pos) = atoms.iter().position(
            |atom| matches!(atom, Atom::Op(opcode) if opcode.bin_prec_level() == Some(prec_level)),
        ) {
            if opcode_pos + 1 >= atoms.len() || opcode_pos == 0 {
                return Err(ParseError::ImproperExpression);
            }

            let lhs = box atoms.remove(opcode_pos - 1).unwrap_expr()?;
            let opcode = atoms.remove(opcode_pos - 1).unwrap_op().unwrap();
            let rhs = box atoms.remove(opcode_pos - 1).unwrap_expr()?;

            let new_binop = Expr::BinOp(opcode, lhs, rhs);

            atoms.insert(opcode_pos - 1, new_binop.into());
        }
    }

    Ok(())
}

fn parse_struct_literal(
    lexer: &mut FuncParseContext,
    type_spec: TypeSpec,
) -> Result<Expr, ParseError> {
    let mut fields = Vec::new();

    lexer.assert_next_tok_is(Tok::LCurly)?;

    while let Ok(field_name) = lexer.peek_tok()?.var_name() {
        lexer.next_tok()?;

        lexer.assert_next_tok_is(Tok::Assignment)?;

        let rhs = lexer.recover(parse_expr, vec![Tok::Return])?;

        fields.push((field_name, rhs));

        if lexer.peek_tok()? != Tok::Comma {
            break;
        }

        lexer.next_tok()?;
    }

    let initialize = match lexer.next_tok()? {
        Tok::DoublePlus => {
            lexer.assert_next_tok_is(Tok::RCurly)?;
            StructFill::Default
        },
        Tok::DoubleMinus => {
            lexer.assert_next_tok_is(Tok::RCurly)?;
            StructFill::Uninit
        },
        Tok::RCurly => StructFill::NoFill,
        got => {
            return Err(ParseError::UnexpectedTok {
                got,
                expected: vec![TokName::DoublePlus, TokName::RCurly, TokName::Comma],
            })
        },
    };

    Ok(Expr::Struct(type_spec, fields, initialize))
}

fn after_generics(lexer: &mut FuncParseContext, tok: Tok) -> Result<bool, ParseError> {
    if lexer.peek_tok()? != Tok::LAngle {
        return Ok(false);
    }

    let mut scope = 0;

    for index in 0.. {
        match lexer.peek_by(index)? {
            Tok::LAngle => {
                scope += 1;
            },
            Tok::RAngle => {
                scope -= 1;

                if scope == 0 {
                    return Ok(lexer.peek_by(index + 1)? == tok);
                }
            },
            Tok::LCurly => return Ok(false),
            _ => {},
        }
    }

    unreachable!()
}
