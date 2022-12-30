use super::*;

pub fn parse_math_expr(lexer: &mut ParseContext<VarName>) -> ParseResult<Expr> {
    let mut atoms = Vec::new();

    let mut last_atom = Expr::Op(Opcode::Plus);

    while let Ok(next) = lexer.peek_tok() {
        let atom = if matches!(last_atom, Expr::Op(_)) {
            // if the last atom was an operator, we expect these
            let atom = match next {
                Tok::Int(val) => Expr::Number(val),
                Tok::Float(val) => Expr::Float(val),
                Tok::DottedNum(_) => {
                    // convert dotted int back into float
                    Expr::Float(next.to_string().parse().unwrap())
                },
                Tok::Bool(val) => Expr::Bool(val),
                Tok::Strin(val) => Expr::Strin(val),
                Tok::VarName(val) => Expr::Ident(val.clone()),
                opcode if opcode.is_unary_op() => Expr::Op(opcode.get_un_opcode().unwrap()),
                Tok::LBrack => Expr::Array(parse_list(
                    (Tok::LBrack, Tok::RBrack),
                    Some(Tok::Comma),
                    |lexer| parse_expr(lexer),
                    lexer,
                )?),
                Tok::LParen => {
                    lexer.next_tok()?;
                    let enclosed = Expr::Enclosed(box parse_math_expr(lexer)?);
                    lexer.next_tok()?;
                    enclosed
                },
                _ => {
                    // attempt to parse a type alias, in case this is a struct or tuple literal
                    match parse_type_spec(lexer) {
                        Ok(type_spec) => {
                            if lexer.peek_tok()? == Tok::Colon {
                                lexer.next_tok()?;
                                let func_name = lexer.next_tok()?.var_name()?;
                                Expr::StaticMethodPath(type_spec, func_name)
                            } else if matches!(lexer.peek_by(1)?, Tok::VarName(_)) {
                                parse_struct_literal(lexer, type_spec)?
                            } else {
                                let exprs = parse_list(
                                    Tok::curlys(),
                                    Some(Tok::Comma),
                                    parse_math_expr,
                                    lexer,
                                )?;

                                Expr::Tuple(type_spec, exprs, false)
                            }
                        },
                        Err(_) => break,
                    }
                },
            };

            // we've detected these, but we need to move the token pos forward by one.
            if matches!(
                atom,
                Expr::Number(_)
                    | Expr::Float(_)
                    | Expr::Bool(_)
                    | Expr::Strin(_)
                    | Expr::Ident(_)
                    | Expr::Op(_)
            ) {
                lexer.next_tok()?;
            }

            atom
        } else if matches!(last_atom.clone(), Expr::Ident(_) | Expr::StaticMethodPath(..))
            && matches!(next, Tok::LAngle)
            && after_generics(lexer, Tok::LParen)?
        {
            let generics = parse_list(
                (Tok::LAngle, Tok::RAngle),
                Some(Tok::Comma),
                parse_type_spec,
                lexer,
            )?;

            let params =
                parse_list((Tok::LParen, Tok::RParen), Some(Tok::Comma), parse_expr, lexer)?;

            Expr::ArgList(generics, params)
        } else if matches!(next, Tok::LParen) {
            let params =
                parse_list((Tok::LParen, Tok::RParen), Some(Tok::Comma), parse_expr, lexer)?;

            Expr::ArgList(Vec::new(), params)
        } else if matches!(last_atom, Expr::Ident(_)) && matches!(next, Tok::LBrack) {
            lexer.next_tok()?;
            let index = parse_expr(lexer)?;
            lexer.assert_next_tok_is(Tok::RBrack)?;

            atoms.pop();

            let object = last_atom.clone();

            Expr::Index(box object, box index)
        } else if matches!(next, Tok::DottedNum(..)) {
            lexer.next_tok()?;
            // parse tuple index
            atoms.pop();
            let object = last_atom.clone();
            let Tok::DottedNum((_, right)) = next else { unreachable!() };

            Expr::Member(box object, right.to_string().into())
        } else {
            let Ok(opcode) = next.get_bin_opcode() else { break; };

            lexer.next_tok()?;

            Expr::Op(opcode)
        };

        last_atom = atom.clone();

        atoms.push(atom);
    }

    detect_member_statements(&mut atoms)?;
    detect_calls(&mut atoms);
    detect_unary_ops(&mut atoms);
    detect_binary_ops(&mut atoms);

    assert_eq!(atoms.len(), 1);
    Ok(atoms[0].clone())
}

pub fn detect_member_statements(atoms: &mut Vec<Expr>) -> ParseResult<()> {
    while let Some(dot_pos) = atoms
        .iter()
        .position(|atom| matches!(*atom, Expr::Op(Opcode::Dot)))
    {
        let field = match atoms.remove(dot_pos + 1) {
            Expr::Ident(field) => field,
            got => {
                return Err(ParseError::UnexpectedTok {
                    got: got.first_tok(),
                    expected: vec![TokName::VarName],
                })
            },
        };

        let object = box atoms.remove(dot_pos - 1);
        let new_member = Expr::Member(object, field);

        atoms[dot_pos - 1] = new_member;
    }

    Ok(())
}

pub fn detect_calls(atoms: &mut Vec<Expr>) {
    while let Some(args_pos) = atoms
        .iter()
        .position(|atom| matches!(atom, Expr::ArgList(..)))
    {
        let Expr::ArgList(generics, mut args) = atoms.remove(args_pos) 
            else { unreachable!() };

        let being_called = atoms.remove(args_pos - 1);

        if let Expr::Member(object, field) = being_called {
            args.push(Expr::UnarOp(Opcode::Ref(1), object));

            atoms.push(Expr::Call(box Expr::Ident(field), generics, args, true));
        } else {
            atoms.push(Expr::Call(box being_called, generics, args, false));
        }
    }
}

pub fn detect_unary_ops(atoms: &mut Vec<Expr>) {
    for prec_level in 0..=Opcode::MAX_UNARY_PRECEDENT_LEVEL {
        while let Some(opcode_pos) = atoms.iter().rposition(|atom| {
            let Expr::Op(opcode) = atom else { return false };
            opcode.un_prec_level() == Some(prec_level)
        }) {
            let Expr::Op(opcode) = atoms[opcode_pos] else { unreachable!() };

            let rhs = Box::new(atoms.remove(opcode_pos + 1));
            let new_unop = Expr::UnarOp(opcode, rhs);

            atoms[opcode_pos] = new_unop;
        }
    }
}

pub fn detect_binary_ops(atoms: &mut Vec<Expr>) {
    for prec_level in 0..=Opcode::MAX_BINARY_PRECEDENT_LEVEL {
        loop {
            let opcode_pos = atoms.iter().position(|atom| match atom {
                Expr::Op(opcode) => opcode.bin_prec_level() == Some(prec_level),
                _ => false,
            });

            let Some(opcode_pos) = opcode_pos else { break; };
            let Expr::Op(opcode) = atoms[opcode_pos] else { unreachable!() };

            let rhs = Box::new(atoms.remove(opcode_pos + 1));
            let lhs = Box::new(atoms.remove(opcode_pos - 1));
            let new_binop = Expr::BinOp(opcode, lhs, rhs);

            atoms[opcode_pos - 1] = new_binop;
        }
    }
}

pub fn parse_struct_literal(
    lexer: &mut ParseContext<VarName>,
    type_spec: TypeSpec,
) -> Result<Expr, ParseError> {
    let mut fields = Vec::new();

    lexer.assert_next_tok_is(Tok::LCurly)?;

    while let Ok(field_name) = lexer.next_tok()?.var_name() {
        lexer.assert_next_tok_is(Tok::Assignment)?;
        let rhs = parse_expr(lexer)?;

        fields.push((field_name, rhs));

        if lexer.peek_tok()? != Tok::Comma {
            break;
        }
        lexer.next_tok()?;
        if matches!(lexer.peek_tok()?, Tok::RCurly | Tok::DoublePlus) {
            break;
        }
    }

    let initialize = match lexer.next_tok()? {
        Tok::DoublePlus => {
            lexer.assert_next_tok_is(Tok::RCurly)?;
            true
        },
        Tok::RCurly => false,
        got => {
            return Err(ParseError::UnexpectedTok {
                got,
                expected: vec![TokName::DoublePlus, TokName::RCurly],
            })
        },
    };

    Ok(Expr::Struct(type_spec, fields, initialize))
}

fn after_generics(lexer: &mut ParseContext<VarName>, tok: Tok) -> Result<bool, ParseError> {
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
