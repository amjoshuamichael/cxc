use super::*;

pub fn parse_math_expr(lexer: &mut Peekable<impl Iterator<Item = Token>>) -> Expr {
    let mut atoms = Vec::new();

    let mut last_atom = Expr::Op(Opcode::Plus);
    loop {
        if matches!(last_atom, Expr::Ident(_))
            && matches!(lexer.peek(), Some(Token::LeftParen))
        {
            let params = parse_expr_list(lexer);
            let func_name = last_atom.clone();

            let last_atom_index = atoms.len() - 1;
            atoms[last_atom_index] = Expr::Call(Box::new(func_name), params);
            continue;
        }

        let atom = if matches!(last_atom, Expr::Op(_)) {
            match lexer.peek() {
                Some(Token::Int(val)) => Expr::Number(*val),
                Some(Token::Float(val)) => Expr::Float(*val),
                Some(Token::Ident(val)) => Expr::Ident(val.clone()),
                Some(possible_opcode) => match possible_opcode.get_un_opcode() {
                    Some(opcode) => Expr::Op(opcode),
                    None => break,
                },
                None => break,
            }
        } else {
            let Some(possible_opcode) = lexer.peek() else { break; };
            let Some(opcode) = possible_opcode.get_bin_opcode() else { break; };
            Expr::Op(opcode)
        };

        last_atom = atom.clone();

        lexer.next();
        atoms.push(atom);
    }

    handle_unops(&mut atoms);
    handle_binops(&mut atoms);

    dbg!(&atoms);
    assert_eq!(atoms.len(), 1);
    return atoms[0].clone();
}

pub fn handle_unops(atoms: &mut Vec<Expr>) {
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

pub fn handle_binops(atoms: &mut Vec<Expr>) {
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
