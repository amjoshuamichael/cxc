use super::*;

pub fn parse_math_expr(lexer: &mut ParseContext<VarName>) -> Result<Expr, ParseError> {
    let mut atoms = Vec::new();

    let mut last_atom = Expr::Op(Opcode::Plus);
    loop {
        let next = match lexer.peek_tok() {
            Ok(tok) => tok,
            Err(ParseError::UnexpectedEndOfFile) => break,
            Err(other_error) => return Err(other_error),
        };

        let atom = if matches!(last_atom, Expr::Op(_)) {
            let atom = match next {
                Tok::Int(val) => Expr::Number(val),
                Tok::Float(val) => Expr::Float(val),
                Tok::Bool(val) => Expr::Bool(val),
                Tok::Strin(val) => Expr::Strin(val),
                Tok::VarName(val) => Expr::Ident(val.clone()),
                Tok::LeftBrack => Expr::Array(parse_list(
                    (Tok::LeftBrack, Tok::RghtBrack),
                    Some(Tok::Comma),
                    |lexer| parse_expr(lexer),
                    lexer,
                )?),
                Tok::TypeName(struct_name) => {
                    lexer.next_tok()?;

                    parse_struct_literal(lexer, struct_name)?
                },
                opcode if opcode.is_un_op() => {
                    Expr::Op(opcode.get_un_opcode().unwrap())
                },
                _ => break,
            };

            if !matches!(atom, Expr::Array(_) | Expr::Struct{..}) {
                lexer.next_tok()?;
            }

            atom
        } else if matches!(last_atom.clone(), Expr::Ident(_))
            && matches!(next, Tok::LeftAngle) 
            && after_generics(lexer, Tok::LeftParen)? {
            let generics = parse_list(
                (Tok::LeftAngle, Tok::RghtAngle),
                Some(Tok::Comma),
                parse_type_alias,
                lexer,
            )?;

            let params = parse_list(
                (Tok::LeftParen, Tok::RghtParen),
                Some(Tok::Comma),
                parse_expr,
                lexer,
            )?;

            Expr::ArgList(generics, params)
        } else if matches!(next, Tok::LeftParen) {
            let params = parse_list(
                (Tok::LeftParen, Tok::RghtParen),
                Some(Tok::Comma),
                parse_expr,
                lexer,
            )?;

            Expr::ArgList(Vec::new(), params)
        } else if matches!(last_atom.clone(), Expr::Ident(_))
            && matches!(next, Tok::LeftBrack) {

            assert_eq!(lexer.next_tok()?, Tok::LeftBrack);
            let index = parse_expr(lexer)?;
            assert_eq!(lexer.next_tok()?, Tok::RghtBrack);

            let object = atoms.pop().unwrap();

            Expr::Index(box object, box index)
        } else {
            let Ok(opcode) = next.get_bin_opcode() else { break; };

            lexer.next_tok()?;

            Expr::Op(opcode)
        };

        last_atom = atom.clone();

        atoms.push(atom);
    }

    members(&mut atoms);
    calls(&mut atoms);
    unops(&mut atoms);
    binops(&mut atoms);

    assert_eq!(atoms.len(), 1);
    Ok(atoms[0].clone())
}

pub fn members(atoms: &mut Vec<Expr>) {
    while let Some(dot_pos) = atoms
        .iter()
        .position(|atom| matches!(*atom, Expr::Op(Opcode::Dot)))
    {
        let Expr::Ident(field) = atoms.remove(dot_pos + 1) else { panic!() };
        let object = box atoms.remove(dot_pos - 1);
        let new_member = Expr::Member(object, field);

        atoms[dot_pos - 1] = new_member;
    }
}

pub fn calls(atoms: &mut Vec<Expr>) {
    while let Some(args_pos) = atoms
        .iter()
        .position(|atom| matches!(atom, Expr::ArgList(..)))
    {
        let Expr::ArgList(generics, mut args) = atoms.remove(args_pos) 
            else { unreachable!() };

        let being_called = atoms.remove(args_pos - 1);

        if let Expr::Ident(name) = being_called {
            atoms.push(Expr::Call(name, generics, args, false));
        } else if let Expr::Member(object, field) = being_called {
            args.push(Expr::UnarOp(Opcode::Ref(1), object));
            atoms.push(Expr::Call(field, generics, args, true));
        }
    }
}

pub fn unops(atoms: &mut Vec<Expr>) {
    for prec_level in 0..=Opcode::MAX_UN_PREC {
        loop {
            let opcode_pos = atoms.iter().position(|atom| match atom {
                Expr::Op(opcode) => opcode.un_prec_level() == Some(prec_level),
                _ => false,
            });

            let Some(opcode_pos) = opcode_pos else { break; };
            let Expr::Op(opcode) = atoms[opcode_pos] else { unreachable!() };

            let rhs = Box::new(atoms.remove(opcode_pos + 1));
            let new_unop = Expr::UnarOp(opcode, rhs);

            atoms[opcode_pos] = new_unop;
        }
    }
}

pub fn binops(atoms: &mut Vec<Expr>) {
    for prec_level in 0..=Opcode::MAX_BIN_PREC {
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

pub fn parse_struct_literal(lexer: &mut ParseContext<VarName>, struct_name: TypeName) -> Result<Expr, ParseError> {
    let generics = if lexer.peek_tok()? == Tok::LeftAngle {
        Some(parse_list(
            (Tok::LeftAngle, Tok::RghtAngle),
            Some(Tok::Comma),
            parse_type_alias,
            lexer,
        )?)
    } else { None };

    let fields = parse_list(
        (Tok::LeftCurly, Tok::RghtCurly),
        Some(Tok::Comma),
        |lexer| {
            let field = lexer.next_tok()?.var_name()?;

            assert_eq!(lexer.next_tok()?, Tok::Assignment);

            let rhs = parse_expr(lexer)?;

            Ok((field, rhs))
        },
        lexer,
    )?;

    if let Some(generics) = generics {
        Ok(Expr::Struct(TypeAlias::Generic(struct_name, generics), fields))
    } else {
        Ok(Expr::Struct(TypeAlias::Named(struct_name), fields))
    }
}


fn after_generics(lexer: &mut ParseContext<VarName>, tok: Tok) -> Result<bool, ParseError> {
    if lexer.peek_tok()? != Tok::LeftAngle {
        return Ok(false);
    }

    // TODO: make this check over scope
    for index in 0.. {
        match lexer.peek_by(index)? {
            Tok::RghtAngle => {
                return Ok(lexer.peek_by(index + 1)? == tok);
            },
            Tok::LeftCurly => return Ok(false),
            _ => {},
        }
    }

    unreachable!()
}
