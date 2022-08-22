use super::*;
use crate::lex::Token;
use indexmap::IndexMap;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum TypeAlias {
    Named(String),
    Generic(String, Vec<TypeAlias>),
    GenParam(u8),
    Int(u32),
    Float(u32),
    Ref(Box<TypeAlias>),
    Struct(IndexMap<String, TypeAlias>),
}

pub fn parse_generic_alias(
    mut lexer: &mut Peekable<impl Iterator<Item = Token>>,
    generic_labels: &HashMap<String, u8>,
) -> TypeAlias {
    let beginning_of_alias = lexer.peek().unwrap().clone();

    match beginning_of_alias {
        Token::LeftCurly => TypeAlias::Struct(parse_struct(lexer, generic_labels)),
        Token::AmpersandSet(count) => {
            lexer.next();

            let mut output = parse_generic_alias(lexer, generic_labels);

            for _ in 0..count {
                output = TypeAlias::Ref(box output);
            }

            output
        },
        Token::Ident(name) => {
            lexer.next();
            let first_char = name.chars().next();

            if let Some(generic_index) = generic_labels.get(&name) {
                return TypeAlias::GenParam(*generic_index);
            }

            if matches!(first_char, Some('i') | Some('u') | Some('f'))
                && name.chars().skip(1).all(|c| c.is_digit(10))
            {
                // TypeSpec is accessing a primitive value
                let bit_width: u32 =
                    name.chars().skip(1).collect::<String>().parse().unwrap();

                return match first_char {
                    Some('u') | Some('i') => TypeAlias::Int(bit_width),
                    Some('f') => TypeAlias::Float(bit_width),
                    _ => unreachable!(),
                };
            }

            if let Some(Token::LeftAngle) = lexer.peek() {
                let generics = parse_list(
                    Token::LeftAngle,
                    Some(Token::Comma),
                    Token::RghtAngle,
                    |lexer| parse_generic_alias(lexer, generic_labels),
                    lexer,
                );

                return TypeAlias::Generic(name.clone(), generics);
            }

            TypeAlias::Named(name.clone())
        },
        _ => panic!(),
    }
}

pub fn parse_type_alias(
    lexer: &mut Peekable<impl Iterator<Item = Token>>,
) -> TypeAlias {
    let empty_hashmap = HashMap::new();
    parse_generic_alias(lexer, &empty_hashmap)
}

pub fn parse_struct(
    mut lexer: &mut Peekable<impl Iterator<Item = Token>>,
    generic_labels: &HashMap<String, u8>,
) -> IndexMap<String, TypeAlias> {
    let mut fields_vec = parse_list(
        Token::LeftCurly,
        Some(Token::Comma),
        Token::RghtCurly,
        |lexer| {
            let Some(Token::Ident(name)) = lexer.next() else { panic!() };
            assert_eq!(Some(Token::Colon), lexer.next());
            let typ = parse_generic_alias(lexer, generic_labels);

            (name, typ)
        },
        lexer,
    );

    fields_vec.iter_mut().map(|t| t.clone()).collect()
}
