use super::*;

pub fn parse_math_expr(lexer: &mut Peekable<impl Iterator<Item = Token>>) -> Expr {
    let mut atoms = Vec::new();

    let mut last_atom = Expr::Op(Opcode::Plus);
    loop {
        let atom = if matches!(last_atom, Expr::Op(_)) {
            let atom = match lexer.peek() {
                Some(Token::Int(val)) => Expr::Number(*val),
                Some(Token::Float(val)) => Expr::Float(*val),
                Some(Token::Ident(val)) => Expr::Ident(val.clone()),
                Some(opcode) if opcode.get_un_opcode().is_some() => {
                    Expr::Op(opcode.get_un_opcode().unwrap())
                },
                _ => break,
            };

            lexer.next();

            atom
        } else if matches!(lexer.peek(), Some(Token::LeftParen)) {
            let params = parse_list(
                Token::LeftParen,
                Some(Token::Comma),
                Token::RghtParen,
                parse_expr,
                lexer,
            );

            Expr::ArgList(params)
        } else if let Expr::Ident(struct_name) = last_atom 
            && matches!(lexer.peek(), Some(Token::LeftCurly) | Some(Token::LeftAngle)) {
            let struct_literal = parse_struct_literal(lexer, struct_name);

            atoms.pop();

            struct_literal
        } else {
            let Some(possible_opcode) = lexer.peek() else { break; };
            let Some(opcode) = possible_opcode.get_bin_opcode() else { break; };
            dbg!(opcode);

            lexer.next();

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
        let Expr::ArgList(args) = atoms.remove(args_pos) else { unreachable!() };

        let new_call = Expr::Call(box atoms.remove(args_pos - 1), args);

        atoms.push(new_call);
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
            }) else { unreachable!() };

            let Some(opcode_pos) = opcode_pos else { break; };
            let Expr::Op(opcode) = atoms[opcode_pos] else { unreachable!() };

            let rhs = Box::new(atoms.remove(opcode_pos + 1));
            let lhs = Box::new(atoms.remove(opcode_pos - 1));
            let new_binop = Expr::BinOp(opcode, lhs, rhs);

            atoms[opcode_pos - 1] = new_binop;
        }
    }
}

pub fn parse_struct_literal(lexer: &mut Peekable<impl Iterator<Item = Token>>, struct_name: String) -> Expr {
    let generics = if lexer.peek() == Some(&Token::LeftAngle) {
        Some(parse_list(
            Token::LeftAngle,
            Some(Token::Comma),
            Token::RghtAngle,
            parse_type_alias,
            lexer,
        ))
    } else { None };

    let fields = parse_list(
        Token::LeftCurly,
        Some(Token::Comma),
        Token::RghtCurly,
        |lexer| {
            let Some(Token::Ident(field)) = lexer.next() else { panic!() };
            assert_eq!(lexer.next(), Some(Token::Assignment));
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
    dbg!(&output);
    output
}
