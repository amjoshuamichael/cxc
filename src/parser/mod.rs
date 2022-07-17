use crate::lex::Token;
use crate::parse::*;
use logos::Lexer;
use std::fmt;
use std::iter::Peekable;

#[derive(Debug)]
enum S {
    Int(u128),
    BinOp(Opcode, Box<S>, Box<S>),
    UnOp(Opcode, Box<S>),
    Op(Opcode),
}

fn parse(lexer: Lexer<Token>) {
    parse_expr(&mut lexer.peekable());
}

fn parse_expr(lexer: &mut Peekable<impl Iterator<Item = Token>>) -> Option<S> {
    let mut atoms = Vec::new();

    loop {
        let atom = match lexer.peek() {
            Some(Token::Int(val)) => Some(S::Int(*val)),
            Some(possible_opcode) => match possible_opcode.get_opcode() {
                Some(opcode) => Some(S::Op(opcode)),
                None => break,
            },
            None => break,
        };

        atoms.push(atom);
    }

    None
}

fn infix_binding_power(op: char) -> (u8, u8) {
    match op {
        '+' | '-' => (1, 2),
        '*' | '/' => (3, 4),
        _ => panic!("bad op: {op:?}"),
    }
}

#[test]
fn tests() {
    let s = parse(lex("1"));
    assert_eq!(s.to_string(), "1");
}
