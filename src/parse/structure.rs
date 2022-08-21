use super::*;
use crate::lex::Token;
use indexmap::IndexMap;

#[derive(Debug, Clone)]
pub enum TypeAlias {
    Named(String),
    Int(u32),
    Float(u32),
    Ref(Box<TypeAlias>),
    Struct(IndexMap<String, TypeAlias>),
}

pub fn parse_type_alias(
    mut lexer: &mut Peekable<impl Iterator<Item = Token>>,
) -> TypeAlias {
    let beginning_of_alias = lexer.peek().unwrap().clone();
    match beginning_of_alias {
        Token::LeftCurly => TypeAlias::Struct(parse_struct(lexer)),
        Token::AmpersandSet(count) => {
            lexer.next();

            let mut output = parse_type_alias(lexer);

            for _ in 0..count {
                output = TypeAlias::Ref(box output);
            }

            output
        },
        Token::Ident(name) => {
            lexer.next();
            let first_char = name.chars().next();

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

            TypeAlias::Named(name.clone())
        },
        _ => panic!(),
    }
}

pub fn parse_struct(
    mut lexer: &mut Peekable<impl Iterator<Item = Token>>,
) -> IndexMap<String, TypeAlias> {
    let mut fields_vec = parse_list(
        Token::LeftCurly,
        Some(Token::Comma),
        Token::RghtCurly,
        parse_var_decl,
        lexer,
    );

    fields_vec
        .iter_mut()
        .map(|vd| (vd.var_name.clone(), vd.type_spec.clone().unwrap()))
        .collect()
}
