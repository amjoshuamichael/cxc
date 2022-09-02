use super::*;
use crate::lex::Tok;
use indexmap::IndexMap;
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeAlias {
    Named(TypeName),
    Generic(TypeName, Vec<TypeAlias>),
    GenParam(u8),
    Int(u32),
    Float(u32),
    Ref(Box<TypeAlias>),
    Struct(IndexMap<VarName, TypeAlias>, HashSet<VarName>),
    Array(Box<TypeAlias>, u32),
}

#[derive(Default)]
pub struct StructParsingContext {
    pub name: String,
    pub dependencies: HashSet<String>,
}

fn parse_type_and_methods(
    lexer: &mut ParseContext<TypeName>,
) -> (TypeAlias, Vec<Decl>) {
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
        Tok::TypeName(name) => {
            lexer.next_tok();
            match name {
                TypeName::I64 => TypeAlias::Int(64),
                TypeName::I32 => TypeAlias::Int(32),
                TypeName::I16 => TypeAlias::Int(16),
                TypeName::I8 => TypeAlias::Int(8),
                TypeName::F128 => TypeAlias::Float(128),
                TypeName::F64 => TypeAlias::Float(64),
                TypeName::F32 => TypeAlias::Float(32),
                TypeName::F16 => TypeAlias::Float(16),
                TypeName::Anonymous => unreachable!(),
                TypeName::Other(_) => {
                    if let Some(generic_index) = lexer.get_generic_label(&name) {
                        TypeAlias::GenParam(generic_index)
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

pub fn parse_type_decl(mut lexer: ParseContext<TypeName>) -> (TypeDecl, Vec<Decl>) {
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

pub fn parse_type_alias(lexer: &mut ParseContext<VarName>) -> TypeAlias {
    let temp_lexer = lexer.create_new_with_name(TypeName::Anonymous);
    parse_type_decl(temp_lexer).0.typ
}

pub fn parse_struct(lexer: &mut ParseContext<TypeName>) -> (TypeAlias, Vec<Decl>) {
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
                methods.insert(decl.name().clone());
                method_declarations.push(decl.into());
            },
        }
    }

    (TypeAlias::Struct(fields, methods), method_declarations)
}

enum StructPart {
    Field { name: VarName, typ: TypeAlias },
    Method { is_static: bool, decl: SomeFuncDecl },
}

fn parse_struct_part(lexer: &mut ParseContext<TypeName>) -> StructPart {
    let beginning_of_part = lexer.peek_tok().unwrap();

    if let Some(name) = beginning_of_part.clone().var_name() {
        lexer.next_tok();
        assert_eq!(Some(Tok::Colon), lexer.next_tok());
        let typ = parse_type_and_methods(lexer).0;

        StructPart::Field { name, typ }
    } else if beginning_of_part == Tok::Dot {
        lexer.next_tok();

        let name = lexer.next_tok().unwrap().var_name().unwrap();
        let func_context = lexer.create_new_with_name(name);

        let mut decl = parse_maybe_gen_func(func_context, true);
        let args = decl.args_mut();

        if lexer.has_generics() {
            let mut generic_params = Vec::new();

            for g in 0..lexer.generic_count() {
                generic_params.push(TypeAlias::GenParam(g as u8));
            }

            args.push(VarDecl {
                var_name: "self".into(),
                type_spec: Some(TypeAlias::Ref(box TypeAlias::Generic(
                    lexer.name_of_this().clone(),
                    generic_params,
                ))),
            });
        } else {
            args.push(VarDecl {
                var_name: "self".into(),
                type_spec: Some(TypeAlias::Ref(box TypeAlias::Named(
                    lexer.name_of_this().clone(),
                ))),
            });
        }

        StructPart::Method {
            is_static: false,
            decl,
        }
    } else {
        panic!()
    }
}
