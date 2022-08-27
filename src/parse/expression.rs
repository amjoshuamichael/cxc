use super::*;

pub fn parse_math_expr(lexer: &mut Lexer) -> Expr {
    let mut atoms = Vec::new();

    let mut last_atom = Expr::Op(Opcode::Plus);
    loop {
        let next = lexer.peek();

        let atom = if matches!(last_atom, Expr::Op(_)) {
            let atom = match next.unwrap() {
                Token::Int(val) => Expr::Number(val),
                Token::Float(val) => Expr::Float(val),
                Token::Ident(val) => Expr::Ident(val.clone()),
                Token::LeftBrack => Expr::Array(parse_list(
                    Token::LeftBrack,
                    Some(Token::Comma),
                    Token::RghtBrack,
                    parse_expr,
                    lexer,
                )),
                opcode if opcode.get_un_opcode().is_some() => {
                    Expr::Op(opcode.get_un_opcode().unwrap())
                },
                _ => break,
            };

            if !matches!(atom, Expr::Array(_)) {
                lexer.next();
            }

            atom
        } else if matches!(next, Some(Token::LeftParen)) {
            let params = parse_list(
                Token::LeftParen,
                Some(Token::Comma),
                Token::RghtParen,
                parse_expr,
                lexer,
            );

            Expr::ArgList(params)
        } else if let Expr::Ident(var_name) = last_atom.clone() && matches!(next, Some(Token::LeftBrack)) {
            assert_eq!(lexer.next(), Some(Token::LeftBrack));
            let index = parse_expr(lexer);
            assert_eq!(lexer.next(), Some(Token::RghtBrack));

            let object = atoms.pop().unwrap();

            Expr::Index(box object, box index)
        } else if let Expr::Ident(struct_name) = last_atom 
            && (matches!(next, Some(Token::LeftCurly)) || check_for_generics(lexer)){
            let struct_literal = parse_struct_literal(lexer, struct_name);

            atoms.pop();

            struct_literal
        } else {
            let Some(possible_opcode) = next else { break; };
            let Some(opcode) = possible_opcode.get_bin_opcode() else { break; };

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
        let Expr::ArgList(mut args) = atoms.remove(args_pos) else { unreachable!() };

        let being_called = atoms.remove(args_pos - 1);

        if let Expr::Ident(name) = being_called {
            atoms.push(Expr::Call(name, args));
        } else if let Expr::Member(object, field) = being_called {
            let mut args_with_obj = vec![*object];
            args_with_obj.append(&mut args);
            atoms.push(Expr::Call(field, args_with_obj));
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

pub fn parse_struct_literal(lexer: &mut Lexer, struct_name: String) -> Expr {
    let generics = if lexer.peek() == Some(Token::LeftAngle) {
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

    output
}


pub fn check_for_generics(lexer: &mut Lexer) -> bool {
    if lexer.peek() != Some(Token::LeftAngle) {
        return false;
    }

    // TODO: make this check over scope
    for index in 0.. {
        match lexer.peek_by(index) {
            Some(Token::RghtAngle) => {
                return lexer.peek_by(index + 1) == Some(Token::LeftCurly);
            },
            Some(Token::LeftCurly) | None => return false,
            _ => {},
        }
    }

    unreachable!()
}
