use super::*;
use crate::lex::Tok;
use indexmap::IndexMap;
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq, Eq)]
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
    pub name: String,
    pub dependencies: HashSet<String>,
}

fn parse_type_and_methods(lexer: &mut ParseContext) -> (TypeAlias, Vec<FuncDecl>) {
    let beginning_of_alias = lexer.peek_tok().unwrap().clone();

    let mut methods = Vec::new();
    let type_alias = match beginning_of_alias {
        Tok::LeftCurly => {
            let strct = parse_struct(lexer);
            methods = strct.1;
            strct.0
        },
        Tok::AmpersandSet(count) => {
            lexer.next_tok();

            let mut output = parse_type_and_methods(lexer).0;

            for _ in 0..count {
                output = TypeAlias::Ref(box output);
            }

            output
        },
        Tok::Ident(name) => {
            lexer.next_tok();
            let first_char = name.chars().next().unwrap();

            if let Some(generic_index) = lexer.get_generic_label(&name) {
                TypeAlias::GenParam(generic_index)
            } else if matches!(first_char, 'i' | 'u' | 'f')
                && name.chars().skip(1).all(|c| c.is_digit(10))
            {
                // TypeSpec is accessing a primitive value
                let bit_width: u32 =
                    name.chars().skip(1).collect::<String>().parse().unwrap();

                match first_char {
                    'u' | 'i' => TypeAlias::Int(bit_width),
                    'f' => TypeAlias::Float(bit_width),
                    _ => unreachable!(),
                }
            } else if let Some(Tok::LeftAngle) = lexer.peek_tok() {
                let generics = parse_list(
                    (Tok::LeftAngle, Tok::RghtAngle),
                    Some(Tok::Comma),
                    |lexer| parse_type_and_methods(lexer).0,
                    lexer,
                );

                lexer.push_type_dependency(name.clone());

                TypeAlias::Generic(name, generics)
            } else {
                lexer.push_type_dependency(name.clone());

                TypeAlias::Named(name)
            }
        },
        _ => panic!(),
    };

    let suffix = lexer.peek_tok().unwrap().clone();

    let alias = match suffix {
        Tok::LeftBrack => {
            lexer.next_tok();
            let Some(Tok::Int(count)) = lexer.next_tok() else { panic!() };
            lexer.next_tok();

            TypeAlias::Array(box type_alias, count.try_into().unwrap())
        },
        _ => type_alias,
    };

    (alias, methods)
}

pub fn parse_type_decl(mut lexer: ParseContext) -> (TypeDecl, Vec<FuncDecl>) {
    let (alias, methods) = parse_type_and_methods(&mut lexer);

    let contains_generics = lexer.has_generics();
    let (name, _, dependencies) = lexer.return_info();

    let type_decl = TypeDecl {
        name,
        typ: alias,
        contains_generics,
        dependencies,
    };

    (type_decl, methods)
}

pub fn parse_type_alias(lexer: &mut ParseContext) -> TypeAlias {
    let temp_lexer = lexer.create_new_with_name(String::new());
    parse_type_decl(temp_lexer).0.typ
}

pub fn parse_struct(lexer: &mut ParseContext) -> (TypeAlias, Vec<FuncDecl>) {
    let parts =
        parse_list((Tok::LeftCurly, Tok::RghtCurly), None, parse_struct_part, lexer);

    let mut fields = IndexMap::new();
    let mut methods = HashSet::new();
    let mut method_declarations = Vec::new();

    for p in parts {
        match p {
            StructPart::Field { name, typ } => {
                fields.insert(name, typ);
            },
            StructPart::Method { decl, .. } => {
                methods.insert(decl.name.clone());
                method_declarations.push(decl);
            },
        }
    }

    (TypeAlias::Struct(fields, methods), method_declarations)
}

enum StructPart {
    Field { name: String, typ: TypeAlias },
    Method { is_static: bool, decl: FuncDecl },
}

fn parse_struct_part(lexer: &mut ParseContext) -> StructPart {
    let beginning_of_part = lexer.peek_tok().unwrap();

    if let Some(name) = beginning_of_part.clone().ident_name() {
        lexer.next_tok();
        assert_eq!(Some(Tok::Colon), lexer.next_tok());
        let typ = parse_type_and_methods(lexer).0;

        StructPart::Field { name, typ }
    } else if beginning_of_part == Tok::Dot {
        lexer.next_tok();
        let name = lexer.next_tok().unwrap().ident_name().unwrap();
        let func_context = lexer.create_new_with_name(name);

        let mut decl = parse_func(func_context, true);

        decl.args.push(VarDecl {
            var_name: "self".into(),
            type_spec: Some(TypeAlias::Named(lexer.name_of_this())),
        });

        StructPart::Method {
            is_static: false,
            decl,
        }
    } else {
        panic!()
    }
}
