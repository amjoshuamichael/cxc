use super::*;

#[derive(Clone, Debug)]
enum Atom {
    Expr(Expr),
    Op(Opcode),
}

impl From<Expr> for Atom {
    fn from(expr: Expr) -> Atom { Atom::Expr(expr) }
}

impl Errable for Atom {
    fn err() -> Self { Atom::Expr(Expr::err()) }
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

    while let Ok(next) = lexer.peek_tok().cloned() {
        let atom = if matches!(last_atom, Atom::Op(_)) {
            // if the last atom was an operator, we expect these
            match parse_atom_after_op(lexer)? {
                Some(atom) => atom,
                _ => break,
            }
        } else if matches!(next, Tok::LAngle) && after_generics(lexer, Tok::LParen)? {
            let generics = parse_list(
                Tok::angles(),
                COMMAS,
                parse_type_spec,
                lexer,
            )?;

            parse_call(&mut atoms, generics, lexer)?
        } else if matches!(next, Tok::LParen) && !lexer.next_is_whitespace() {
            parse_call(&mut atoms, Vec::new(), lexer)?
        } else if matches!((&last_atom, &next), (Atom::Expr(Expr::Ident(_)), Tok::LBrack)) {
            lexer.next_tok()?;
            let index = parse_expr(lexer)?;
            lexer.assert_next_tok_is(Tok::RBrack, TokName::RBrack)?;

            let object = atoms.pop().unwrap().unwrap_expr()?;

            Expr::Index(Box::new(object), Box::new(index)).into()
        } else if matches!(next, Tok::DottedNum(..)) {
            lexer.next_tok()?;
            // parse tuple index
            let object = atoms.pop().unwrap().unwrap_expr()?;

            let Tok::DottedNum((_, right)) = next else { unreachable!() };

            Expr::Member(Box::new(object), VarName::TupleIndex(right as usize)).into()
        } else if next == Tok::Dot {
            lexer.next_tok()?;
            let previous = atoms
                .pop()
                .ok_or(ParseError::ImproperExpression)?
                .unwrap_expr()?;
            let field = lexer.next_tok()?.var_name()?;

            Expr::Member(Box::new(previous), field).into()
        } else if let Ok(opcode) = next.get_bin_opcode() {
            if opcode == Opcode::Multiplier {
                // this might be a dereference just before an assignment. for example,
                //
                // x: u32 = alloc(0)
                // *x = 5

                let mut detached = lexer.detach();

                if let Ok(_actually_lhs) = parse_expr(&mut detached) && 
                    detached.peek_tok()? == &Tok::Assignment {
                    break;
                }
            }

            lexer.next_tok()?;

            Atom::Op(opcode)
        } else {
            break;
        };

        last_atom = atom.clone();

        atoms.push(atom);
    }

    detect_unary_ops(&mut atoms)?;
    detect_binary_ops(&mut atoms)?;

    if atoms.len() != 1 {
        return Err(ParseError::ImproperExpression);
    }

    atoms.pop().unwrap().unwrap_expr()
}

// parses atoms that go in between operators. Like the 1, 2, and "Hello" in 1 *
// 2 + "hello"
fn parse_atom_after_op(lexer: &mut FuncParseContext) -> ParseResult<Option<Atom>> {
    let atom = match lexer.peek_tok()?.clone() {
        Tok::Int(val) => Expr::Number(val).into(),
        Tok::Float(val) => Expr::Float(val).into(),
        Tok::DottedNum((l, r)) => {
            // convert dotted int back into float
            Expr::Float(format!("{l}.{r}").parse().unwrap()).into()
        },
        Tok::Bool(val) => Expr::Bool(val).into(),
        Tok::Strin(val) => Expr::String(val.clone()).into(),
        Tok::VarName(val) => Expr::Ident(val.clone()).into(),
        Tok::Label(label) => Expr::Label(label).into(),
        opcode if opcode.is_unary_op() => Atom::Op(opcode.get_un_opcode()?),
        Tok::LBrack => parse_array_literal(lexer)?.into(),
        Tok::LParen => {
            lexer.assert_next_tok_is(Tok::LParen, TokName::LParen)?;
            let enclosed = Expr::Enclosed(Box::new(parse_math_expr(lexer)?)).into();
            lexer.assert_next_tok_is(Tok::RParen, TokName::RParen)?;
            enclosed
        },
        Tok::LCurly if lexer.peek_by(1)? == &Tok::RCurly => {
            lexer.next_tok()?;
            lexer.next_tok()?;

            Expr::Tuple(Vec::new(), InitOpts::NoFill).into()
        },
        _ if let Ok(type_spec) = parse_type_spec(&mut lexer.detach()) => {
            parse_type_spec(lexer)?;

            if let Tok::Label(label) = lexer.peek_tok()?.clone() {
                lexer.next_tok()?;
                Atom::Expr(Expr::StaticMethodPath(type_spec, label))
            } else {
                let value = lexer
                    .recover(parse_atom_after_op)
                    .ok_or(ParseError::ImproperExpression)?;
                let Atom::Expr(expr) = value 
                    else { return Err(ParseError::ImproperExpression) };
                Atom::Expr(Expr::TypedValue(type_spec, Box::new(expr)))
            }
        },
        Tok::LCurly 
            if matches!( 
                (lexer.peek_by(1)?, lexer.peek_by(2)?),
                (Tok::DoublePlus, _) |
                (Tok::DoubleMinus, _) 
            ) || 
            (
                matches!(lexer.peek_by(1)?, Tok::VarName(_)) && 
                matches!(lexer.peek_by(2)?, Tok::Assignment)
            ) => 
        {
            parse_struct_literal(lexer)?.into()
        },
        Tok::LCurly => {
            let exprs = parse_list(Tok::curlys(), COMMAS, parse_math_expr, lexer)?;

            Expr::Tuple(exprs, InitOpts::NoFill).into()
        },
        _ => return Ok(None)
    };

    // we've detected these, but we need to move the token pos forward by one.
    if matches!(
        atom,
        Atom::Expr(Expr::Number(_))
            | Atom::Expr(Expr::Float(_))
            | Atom::Expr(Expr::Bool(_))
            | Atom::Expr(Expr::String(_))
            | Atom::Expr(Expr::Ident(_))
            | Atom::Expr(Expr::Label(_))
            | Atom::Op(_)
    ) {
        lexer.next_tok()?;
    }

    Ok(Some(atom))
}

fn parse_call(
    atoms: &mut Vec<Atom>,
    generics: Vec<TypeSpec>,
    lexer: &mut FuncParseContext,
) -> ParseResult<Atom> {
    let mut args = parse_list(Tok::parens(), COMMAS, parse_expr, lexer)?;

    let func = atoms
        .pop()
        .ok_or(ParseError::ArgListWithImproperPredecessor)?;

    let call_expr = match func {
        Atom::Expr(Expr::Member(object, method_name)) => {
            args.insert(0, *object);

            Expr::Call {
                func: Box::new(Expr::Ident(method_name)),
                generics,
                args,
                is_method: true,
            }
        },
        Atom::Expr(_) => Expr::Call {
            func: Box::new(func.unwrap_expr()?),
            generics,
            args,
            is_method: false,
        },
        _ => return Err(ParseError::ArgListWithImproperPredecessor),
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

            let rhs = atoms.remove(opcode_pos).unwrap_expr()?;

            atoms.insert(opcode_pos, Expr::UnarOp(opcode, Box::new(rhs)).into());
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

            let lhs = Box::new(atoms.remove(opcode_pos - 1).unwrap_expr()?);
            let opcode = atoms.remove(opcode_pos - 1).unwrap_op().unwrap();
            let rhs = Box::new(atoms.remove(opcode_pos - 1).unwrap_expr()?);

            let new_binop = Expr::BinOp(opcode, lhs, rhs);

            atoms.insert(opcode_pos - 1, new_binop.into());
        }
    }

    Ok(())
}

fn parse_struct_literal(lexer: &mut FuncParseContext) -> Result<Expr, ParseError> {
    let mut fields = Vec::new();

    lexer.assert_next_tok_is(Tok::LCurly, TokName::LCurly)?;

    while let Ok(field_name) = lexer.peek_tok()?.var_name() {
        lexer.next_tok()?;

        lexer.assert_next_tok_is(Tok::Assignment, TokName::FieldAssignment)?;

        let rhs = lexer.recover_with(vec![&Tok::Comma], parse_expr);

        fields.push((field_name, rhs));

        if lexer.peek_tok()? != &Tok::Comma {
            break;
        }

        lexer.next_tok()?;
    }

    let initialize = match lexer.next_tok()? {
        Tok::DoublePlus => {
            lexer.assert_next_tok_is(Tok::RCurly, TokName::RCurly)?;
            InitOpts::Default
        },
        Tok::DoubleMinus => {
            lexer.assert_next_tok_is(Tok::RCurly, TokName::RCurly)?;
            InitOpts::Uninit
        },
        Tok::RCurly => InitOpts::NoFill,
        got => {
            return ParseError::unexpected(
                got, 
                vec![TokName::DoublePlus, TokName::RCurly, TokName::Comma]
            );
        },
    };

    Ok(Expr::Struct(fields, initialize))
}

fn parse_array_literal(lexer: &mut FuncParseContext) -> Result<Expr, ParseError> {
    lexer.assert_next_tok_is(Tok::LBrack, TokName::LBrack)?;
    let mut expressions = Vec::new();

    loop {
        match lexer.peek_tok()? {
            Tok::DoublePlus => {
                lexer.next_tok()?;
                lexer.assert_next_tok_is(Tok::RBrack, TokName::RBrack)?;
                return Ok(Expr::Array(expressions, InitOpts::Default));
            },
            Tok::DoubleMinus => {
                lexer.next_tok()?;
                lexer.assert_next_tok_is(Tok::RBrack, TokName::RBrack)?;
                return Ok(Expr::Array(expressions, InitOpts::Uninit));
            },
            Tok::RBrack => {
                lexer.next_tok()?;
                return Ok(Expr::Array(expressions, InitOpts::NoFill));
            },
            _ => {
                let expr =
                    lexer.recover_with(vec![&Tok::Comma, &Tok::RBrack], parse_math_expr);
                expressions.push(expr);
            },
        };

        match lexer.next_tok()? {
            Tok::RBrack => {
                return Ok(Expr::Array(expressions, InitOpts::NoFill));
            },
            Tok::Comma => {},
            got => {
                return ParseError::unexpected(got, vec![TokName::RBrack, TokName::Comma]);
            },
        }
    }
}

fn after_generics(lexer: &mut FuncParseContext, tok: Tok) -> ParseResult<bool> {
    if lexer.peek_tok()? != &Tok::LAngle {
        return Ok(false);
    }

    let mut detached = lexer.detach();

    let list = parse_list(Tok::angles(), COMMAS, parse_type_spec, &mut detached);

    Ok(detached.errors.deref().unwrap().is_empty()
        && list.is_ok()
        && detached.peek_tok()? == &tok)
}
