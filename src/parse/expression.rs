use super::*;

pub fn parse_math_expr(lexer: &mut ParseContext) -> Expr {
    let mut atoms = Vec::new();

    let mut last_atom = Expr::Op(Opcode::Plus);
    loop {
        let next = lexer.peek_tok();

        let atom = if matches!(last_atom, Expr::Op(_)) {
            let atom = match next.unwrap() {
                Tok::Int(val) => Expr::Number(val),
                Tok::Float(val) => Expr::Float(val),
                Tok::Ident(val) => Expr::Ident(val.clone()),
                Tok::LeftBrack => Expr::Array(parse_list(
                    (Tok::LeftBrack, Tok::RghtBrack),
                    Some(Tok::Comma),
                    |lexer| parse_expr(lexer),
                    lexer,
                )),
                opcode if opcode.get_un_opcode().is_some() => {
                    Expr::Op(opcode.get_un_opcode().unwrap())
                },
                _ => break,
            };

            if !matches!(atom, Expr::Array(_)) {
                lexer.next_tok();
            }

            atom
        } else if let Expr::Ident(func_name) = last_atom.clone() 
            && matches!(next, Some(Tok::LeftAngle)) 
            && after_generics(lexer, Tok::LeftParen) {
            let generics = parse_list(
                (Tok::LeftAngle, Tok::RghtAngle),
                Some(Tok::Comma),
                parse_type_alias,
                lexer,
            );

            lexer.push_func_dependency(GenFuncDependency {
                name: func_name, types: generics
            });

            let params = parse_list(
                (Tok::LeftParen, Tok::RghtParen),
                Some(Tok::Comma),
                parse_expr,
                lexer,
            );

            Expr::ArgList(params)

        } else if matches!(next, Some(Tok::LeftParen)) {
            let params = parse_list(
                (Tok::LeftParen, Tok::RghtParen),
                Some(Tok::Comma),
                parse_expr,
                lexer,
            );

            Expr::ArgList(params)
        } else if matches!( last_atom.clone(), Expr::Ident(_))
            && matches!(next, Some(Tok::LeftBrack)) {

            assert_eq!(lexer.next_tok(), Some(Tok::LeftBrack));
            let index = parse_expr(lexer);
            assert_eq!(lexer.next_tok(), Some(Tok::RghtBrack));

            let object = atoms.pop().unwrap();

            Expr::Index(box object, box index)
        } else if let Expr::Ident(struct_name) = last_atom 
            && (matches!(next, Some(Tok::LeftCurly)) || after_generics(lexer, Tok::LeftCurly)){
            let struct_literal = parse_struct_literal(lexer, struct_name);

            atoms.pop();

            struct_literal
        } else {
            let Some(possible_opcode) = next else { break; };
            let Some(opcode) = possible_opcode.get_bin_opcode() else { break; };

            lexer.next_tok();

            Expr::Op(opcode)
        };

        last_atom = atom.clone();

        atoms.push(atom);
    }

    members(&mut atoms);
    calls(&mut atoms);
    unops(&mut atoms);
    binops(&mut atoms);

    if crate::DEBUG {
        println!("parsed math expression: ");
        dbg!(&atoms);
    }

    assert_eq!(atoms.len(), 1);
    return atoms[0].clone();
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
        .position(|atom| matches!(atom, Expr::ArgList(_)))
    {
        let Expr::ArgList(mut args) = atoms.remove(args_pos) else { unreachable!() };

        let being_called = atoms.remove(args_pos - 1);

        if let Expr::Ident(name) = being_called {
            atoms.push(Expr::Call(name, args, false));
        } else if let Expr::Member(object, field) = being_called {
            args.push(Expr::UnarOp(Opcode::Ref(1), object));
            atoms.push(Expr::Call(field, args, true));
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

pub fn parse_struct_literal(lexer: &mut ParseContext, struct_name: String) -> Expr {
    let generics = if lexer.peek_tok() == Some(Tok::LeftAngle) {
        Some(parse_list(
            (Tok::LeftAngle, Tok::RghtAngle),
            Some(Tok::Comma),
            parse_type_alias,
            lexer,
        ))
    } else { None };

    let fields = parse_list(
        (Tok::LeftCurly, Tok::RghtCurly),
        Some(Tok::Comma),
        |lexer| {
            let Some(Tok::Ident(field)) = lexer.next_tok() else { panic!() };
            assert_eq!(lexer.next_tok(), Some(Tok::Assignment));
            let rhs = parse_expr(lexer);
            (field, rhs)
        },
        lexer,
    );

    let output = if let Some(generics) = generics {
        Expr::Struct(TypeAlias::Generic(struct_name, generics), fields)
    } else {
        Expr::Struct(TypeAlias::Named(struct_name), fields)
    };

    output
}


fn after_generics(lexer: &mut ParseContext, tok: Tok) -> bool {
    if lexer.peek_tok() != Some(Tok::LeftAngle) {
        return false;
    }

    // TODO: make this check over scope
    for index in 0.. {
        match lexer.peek_by(index) {
            Some(Tok::RghtAngle) => {
                return lexer.peek_by(index + 1) == Some(tok);
            },
            Some(Tok::LeftCurly) | None => return false,
            _ => {},
        }
    }

    unreachable!()
}
