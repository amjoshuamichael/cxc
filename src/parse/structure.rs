use super::*;
use crate::lex::Token;
use indexmap::IndexMap;
use std::collections::HashMap;
use std::collections::HashSet;

#[derive(Debug, Clone)]
pub enum TypeAlias {
    Named(String),
    Generic(String, Vec<TypeAlias>),
    GenParam(u8),
    Int(u32),
    Float(u32),
    Ref(Box<TypeAlias>),
    Struct(IndexMap<String, TypeAlias>, HashSet<String>),
    Array(Box<TypeAlias>, u32),
}

#[derive(Default)]
pub struct StructParsingContext {
    pub generics: HashMap<String, u8>,
    pub name: String,
}

pub fn parse_generic_alias(
    mut lexer: &mut Lexer,
    context: &StructParsingContext,
) -> (TypeAlias, Vec<Declaration>) {
    let beginning_of_alias = lexer.peek().unwrap().clone();

    let mut declarations = Vec::default();
    let type_alias = match beginning_of_alias {
        Token::LeftCurly => {
            let strct = parse_struct(lexer, context);
            declarations = strct.1;
            strct.0
        },
        Token::AmpersandSet(count) => {
            lexer.next();

            let mut output = parse_generic_alias(lexer, context).0;

            for _ in 0..count {
                output = TypeAlias::Ref(box output);
            }

            output
        },
        Token::Ident(name) => {
            lexer.next();
            let first_char = name.chars().next();

            if let Some(generic_index) = context.generics.get(&name) {
                TypeAlias::GenParam(*generic_index)
            } else if matches!(first_char, Some('i') | Some('u') | Some('f'))
                && name.chars().skip(1).all(|c| c.is_digit(10))
            {
                // TypeSpec is accessing a primitive value
                let bit_width: u32 =
                    name.chars().skip(1).collect::<String>().parse().unwrap();

                match first_char {
                    Some('u') | Some('i') => TypeAlias::Int(bit_width),
                    Some('f') => TypeAlias::Float(bit_width),
                    _ => unreachable!(),
                }
            } else if let Some(Token::LeftAngle) = lexer.peek() {
                let generics = parse_list(
                    Token::LeftAngle,
                    Some(Token::Comma),
                    Token::RghtAngle,
                    |lexer| parse_generic_alias(lexer, context).0,
                    lexer,
                );

                TypeAlias::Generic(name.clone(), generics)
            } else {
                TypeAlias::Named(name.clone())
            }
        },
        _ => panic!(),
    };

    let suffix = lexer.peek().unwrap().clone();

    let type_alias = match suffix {
        Token::LeftBrack => {
            lexer.next();
            let Some(Token::Int(count)) = lexer.next() else { panic!() };
            lexer.next();

            TypeAlias::Array(box type_alias, count.try_into().unwrap())
        },
        _ => type_alias,
    };

    (type_alias, declarations)
}

// TODO: introduce a "parse type alias without methods or generics" option,
// which does require a context, so no name or generic labels
pub fn parse_type_alias(lexer: &mut Lexer) -> TypeAlias {
    let context = StructParsingContext::default();
    parse_generic_alias(lexer, &context).0
}

pub fn parse_struct(
    mut lexer: &mut Lexer,
    generic_labels: &StructParsingContext,
) -> (TypeAlias, Vec<Declaration>) {
    let mut parts = parse_list(
        Token::LeftCurly,
        None,
        Token::RghtCurly,
        |lexer| parse_struct_part(lexer, generic_labels),
        lexer,
    );

    let mut fields = IndexMap::new();
    let mut methods = HashSet::new();
    let mut method_declarations = Vec::new();

    for p in parts {
        match p {
            StructPart::Field { name, typ } => {
                fields.insert(name, typ);
            },
            StructPart::Method { is_static, decl } => {
                methods.insert(decl.name().clone());
                method_declarations.push(decl);
            },
        }
    }

    (TypeAlias::Struct(fields, methods), method_declarations)
}

enum StructPart {
    Field { name: String, typ: TypeAlias },
    Method { is_static: bool, decl: Declaration },
}

fn parse_struct_part(
    lexer: &mut Lexer,
    context: &StructParsingContext,
) -> StructPart {
    if let Some(Token::Ident(name)) = lexer.next_if(|t| matches!(t, Token::Ident(_)))
    {
        assert_eq!(Some(Token::Colon), lexer.next());
        let typ = parse_generic_alias(lexer, context).0;

        StructPart::Field { name, typ }
    } else if lexer.next_if(|t| matches!(t, Token::Dot)).is_some() {
        let Some(Token::Ident(name)) = lexer.next() else { panic!() };
        let name = context.name.clone() + &*name;

        let mut decl = parse_func(lexer, name);
        match decl {
            Declaration::Function { ref mut args, .. } => {
                let mut og_args = args.clone();
                *args = vec![VarDecl {
                    var_name: "self".into(),
                    type_spec: Some(TypeAlias::Named(context.name.clone())),
                }];
                args.append(&mut og_args);
            },
            _ => unreachable!(),
        }
        StructPart::Method {
            is_static: false,
            decl,
        }
    } else {
        panic!()
    }
}
