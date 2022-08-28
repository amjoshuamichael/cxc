use super::*;
use crate::lex::Tok;
use indexmap::IndexMap;
use std::collections::HashMap;
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
    pub generics: HashMap<String, u8>,
    pub name: String,
    pub dependencies: HashSet<String>,
}

pub struct TypeParsingOutput {
    pub alias: TypeAlias,
    pub methods: Vec<FuncDecl>,
    pub dependencies: HashSet<String>,
}

pub fn parse_advanced_alias(
    mut lexer: &mut Lexer,
    context: &mut StructParsingContext,
) -> TypeParsingOutput {
    let beginning_of_alias = lexer.peek().unwrap().clone();

    let mut methods = Vec::new();
    let type_alias = match beginning_of_alias {
        Tok::LeftCurly => {
            let strct = parse_struct(lexer, context);
            methods = strct.1;
            strct.0
        },
        Tok::AmpersandSet(count) => {
            lexer.next();

            let mut output = parse_advanced_alias(lexer, context).alias;

            for _ in 0..count {
                output = TypeAlias::Ref(box output);
            }

            output
        },
        Tok::Ident(name) => {
            lexer.next();
            let first_char = name.chars().next().unwrap();

            if let Some(generic_index) = context.generics.get(&name) {
                TypeAlias::GenParam(*generic_index)
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
            } else if let Some(Tok::LeftAngle) = lexer.peek() {
                let generics = parse_list(
                    (Tok::LeftAngle, Tok::RghtAngle),
                    Some(Tok::Comma),
                    |lexer| parse_advanced_alias(lexer, context).alias,
                    lexer,
                );

                context.dependencies.insert(name.clone());

                TypeAlias::Generic(name, generics)
            } else {
                context.dependencies.insert(name.clone());

                TypeAlias::Named(name)
            }
        },
        _ => panic!(),
    };

    let suffix = lexer.peek().unwrap().clone();

    let alias = match suffix {
        Tok::LeftBrack => {
            lexer.next();
            let Some(Tok::Int(count)) = lexer.next() else { panic!() };
            lexer.next();

            TypeAlias::Array(box type_alias, count.try_into().unwrap())
        },
        _ => type_alias,
    };

    TypeParsingOutput {
        alias,
        methods,
        dependencies: context.dependencies.clone(),
    }
}

pub fn parse_type_alias(lexer: &mut Lexer) -> TypeAlias {
    let mut context = StructParsingContext::default();
    parse_advanced_alias(lexer, &mut context).alias
}

pub fn parse_struct(
    mut lexer: &mut Lexer,
    generic_labels: &mut StructParsingContext,
) -> (TypeAlias, Vec<FuncDecl>) {
    let mut parts = parse_list(
        (Tok::LeftCurly, Tok::RghtCurly),
        None,
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

fn parse_struct_part(
    lexer: &mut Lexer,
    context: &mut StructParsingContext,
) -> StructPart {
    let beginning_of_part = lexer.peek().unwrap();

    if let Some(name) = beginning_of_part.clone().ident_name() {
        lexer.next();
        assert_eq!(Some(Tok::Colon), lexer.next());
        let typ = parse_advanced_alias(lexer, context).alias;

        StructPart::Field { name, typ }
    } else if beginning_of_part == Tok::Dot {
        lexer.next();
        let name = lexer.next().unwrap().ident_name().unwrap();

        let mut decl = parse_func(lexer, name, true);

        decl.args.push(VarDecl {
            var_name: "self".into(),
            type_spec: Some(TypeAlias::Named(context.name.clone())),
        });

        StructPart::Method {
            is_static: false,
            decl,
        }
    } else {
        panic!()
    }
}
