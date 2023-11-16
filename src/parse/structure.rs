use std::sync::Arc;

use super::*;
use crate::Field;
use crate::lex::{lex, Tok};
use crate::typ::FloatType;

#[derive(Hash, Default, Debug, Clone, PartialEq, Eq)]
pub struct FieldSpec {
    pub inherited: bool,
    pub name: VarName,
    pub type_spec: TypeSpec,
}

#[derive(Hash, Default, Debug, Clone, PartialEq, Eq)]
pub enum TypeSpec {
    Named(TypeName),
    Generic(TypeName, Vec<TypeSpec>),
    GetGeneric(Box<TypeSpec>, u32),
    GenParam(u8),
    Int(u32),
    UInt(u32),
    Float(FloatType),
    Bool,
    Ref(Box<TypeSpec>),
    Deref(Box<TypeSpec>),
    StructMember(Box<TypeSpec>, VarName),
    SumMember(Box<TypeSpec>, TypeName),
    Struct(Vec<FieldSpec>),
    Union(Vec<FieldSpec>),
    Sum(Vec<(TypeName, TypeSpec)>),
    Tuple(Vec<TypeSpec>),
    Function(Vec<TypeSpec>, Box<TypeSpec>),
    FuncReturnType(Box<TypeSpec>),
    FuncArgType(Box<TypeSpec>, usize),
    TypeLevelFunc(TypeName, Vec<TypeSpec>),
    Array(Box<TypeSpec>, u32),
    ArrayElem(Box<TypeSpec>),
    Destructor(Box<TypeSpec>, Arc<Expr>),
    RemoveWrappers(Box<TypeSpec>),
    #[default]
    Void,
    Unknown,
    Me,
}

impl TypeSpec {
    pub fn get_ref(self) -> TypeSpec { TypeSpec::Ref(Box::new(self)) }
    pub fn get_deref(self) -> TypeSpec { TypeSpec::Deref(Box::new(self)) }
    pub fn get_names<'a>(&'a self, list: &mut Vec<&'a TypeName>) {
        match self {
            TypeSpec::Named(name) => list.push(name),
            TypeSpec::Generic(name, specs) => {
                list.push(name);
                for spec in specs {
                    spec.get_names(list);
                }
            }
            TypeSpec::Ref(spec) |
            TypeSpec::Deref(spec) |
            TypeSpec::StructMember(spec, _) |
            TypeSpec::SumMember(spec, _) |
            TypeSpec::FuncReturnType(spec) |
            TypeSpec::FuncArgType(spec, _) |
            TypeSpec::ArrayElem(spec) |
            TypeSpec::Destructor(spec, _) |
            TypeSpec::Array(spec, _) |
            TypeSpec::GetGeneric(spec, _) => spec.get_names(list),
            TypeSpec::Struct(fields) => {
                for field in fields {
                    field.type_spec.get_names(list);
                }
            },
            TypeSpec::Sum(members) => {
                for member in members {
                    member.1.get_names(list);
                }
            },
            TypeSpec::TypeLevelFunc(_, specs) |
            TypeSpec::Tuple(specs) => {
                for spec in specs {
                    spec.get_names(list);
                }
            },
            TypeSpec::Function(args, ret) => {
                for arg in args {
                    arg.get_names(list);
                }
            },
            _ => {}
        }
    }
}

impl From<&str> for TypeSpec {
    fn from(value: &str) -> Self {
        let mut lexed = lex(value);
        let generic_labels = ["T", "U", "V", "W", "X"]
            .into_iter()
            .enumerate()
            .map(|(index, label)| (TypeName::from(label), index as u8))
            .collect::<GenericLabels>();

        let mut context = lexed.split(TypeName::Anonymous, generic_labels);

        parse_type(&mut context).unwrap()
    }
}

#[derive(PartialEq, Eq, Debug)]
enum TypeOps {
    Plus,
    Destructor,
}

pub fn parse_type_spec<T: Clone>(lexer: &mut ParseContext<T>) -> Result<TypeSpec, ParseError> {
    let mut temp_lexer = lexer.split(TypeName::Anonymous, lexer.generic_labels.clone());
    parse_type(&mut temp_lexer)
}

pub fn parse_type(lexer: &mut TypeParseContext) -> ParseResult<TypeSpec> {
    let atom = parse_type_atom(lexer)?;

    if lexer.move_on(Tok::Tilde) {
        let mut destructor_parser = lexer.split(FuncParseData {
            name: VarName::None,
            has_return: false,
        }, lexer.generic_labels.clone());

        let destructor = if lexer.peek_tok()? == &Tok::LCurly {
            parse_block(&mut destructor_parser)?
        } else {
            Expr::Block(vec![parse_expr(&mut destructor_parser)?])
        };

        Ok(TypeSpec::Destructor(Box::new(atom), Arc::new(destructor)))
    } else {
        Ok(atom)
    }
}

fn parse_type_atom(lexer: &mut TypeParseContext) -> ParseResult<TypeSpec> {
    let beginning_of_spec = lexer.peek_tok()?.clone();

    let mut type_spec = match beginning_of_spec {
        Tok::LCurly => match (lexer.peek_by(1), lexer.peek_by(2), lexer.peek_by(3)) {
            (Ok(Tok::VarName(_)), Ok(Tok::Colon), _) |
            (Ok(Tok::Plus), Ok(Tok::VarName(_)), Ok(Tok::Colon))
                => parse_struct(lexer)?,
            (Ok(Tok::TypeName(_)), Ok(Tok::Colon), _) |
            (Ok(Tok::Plus), Ok(Tok::TypeName(_)), Ok(Tok::Colon))
                => parse_sum(lexer)?,
            _ => {
                lexer.next_tok()?;

                let mut elems = Vec::new();

                if lexer.peek_tok()? == &Tok::RCurly {
                    lexer.next_tok()?;
                    return Ok(TypeSpec::Tuple(Vec::new()));
                }

                elems.push(parse_type(lexer)?);

                match (lexer.next_tok()?.clone(), lexer.peek_tok()) {
                    (Tok::RCurly, _) => elems.into_iter().next().unwrap(),
                    (Tok::Comma, Ok(Tok::RCurly)) => {
                        lexer.next_tok()?;

                        TypeSpec::Tuple(elems)
                    },
                    (Tok::Comma, Ok(_)) => {
                        loop {
                            if lexer.peek_tok() == Ok(&Tok::RCurly) {
                                lexer.next_tok()?;
                                break;
                            }

                            elems.push(parse_type(lexer)?);

                            match lexer.next_tok()? {
                                Tok::Comma => continue,
                                Tok::RCurly => break,
                                got => {
                                    return ParseError::unexpected(got, vec![TokName::RCurly, TokName::Comma]);
                                },
                            }
                        }

                        TypeSpec::Tuple(elems)
                    },
                    (got, _) => {
                        return ParseError::unexpected(&got, vec![TokName::RCurly, TokName::Comma]);
                    },
                }
            },
        },
        Tok::AmpersandSet(count) => {
            lexer.next_tok()?;

            let mut output = parse_type_atom(lexer)?;

            for _ in 0..count {
                output = output.get_ref();
            }

            output
        },
        Tok::AsterickSet(count) => {
            lexer.next_tok()?;

            let mut output = parse_type_atom(lexer)?;

            for _ in 0..count {
                output = output.get_deref();
            }

            output
        },
        Tok::LBrack => {
            lexer.next_tok()?;
            let count = lexer.next_tok()?.int_value()?;
            lexer.next_tok()?;

            TypeSpec::Array(Box::new(parse_type(lexer)?), count.try_into().unwrap())
        },
        Tok::Minus => {
            lexer.next_tok()?;
            TypeSpec::RemoveWrappers(Box::new(parse_type(lexer)?))
        }
        Tok::TypeName(name) => {
            lexer.next_tok()?;

            match name {
                TypeName::I(size) => TypeSpec::Int(size),
                TypeName::U(size) => TypeSpec::UInt(size),
                TypeName::Bool => TypeSpec::Bool,
                TypeName::F64 => TypeSpec::Float(FloatType::F64),
                TypeName::F32 => TypeSpec::Float(FloatType::F32),
                TypeName::Me => TypeSpec::Me,
                TypeName::Other(_) => {
                    let name = name.clone();

                    if lexer.peek_tok() == Ok(&Tok::LParen) {
                        let type_level_func_args =
                            parse_list(Tok::parens(), COMMAS, parse_type, lexer)?;

                        TypeSpec::TypeLevelFunc(name.clone(), type_level_func_args)
                    } else if let Some(generic_index) = lexer.get_generic_label(&name) {
                        TypeSpec::GenParam(generic_index)
                    } else if lexer.peek_tok() == Ok(&Tok::LAngle) {
                        let generics =
                            parse_list(Tok::angles(), COMMAS, parse_type, lexer)?;

                        TypeSpec::Generic(name, generics)
                    } else {
                        TypeSpec::Named(name)
                    }
                },
                TypeName::Anonymous => unreachable!(),
            }
        },
        Tok::LParen => {
            let arg_types = parse_list(Tok::parens(), COMMAS, parse_type, lexer)?;

            let ret_type = match lexer.peek_tok() {
                Ok(Tok::Semicolon) => {
                    lexer.next_tok()?;
                    parse_type(lexer)?
                }
                _ => TypeSpec::Void,
            };

            TypeSpec::Function(arg_types, Box::new(ret_type))
        },
        _ => {
            return ParseError::unexpected(
                &beginning_of_spec, 
                vec![TokName::TypeName, TokName::LCurly, TokName::Ref]
            );
        },
    };

    loop {
        type_spec = if lexer.move_on(Tok::Dot) {
            match lexer.next_tok()? {
                Tok::VarName(name) => 
                    TypeSpec::StructMember(Box::new(type_spec), name.clone()),
                Tok::TypeName(name) => 
                    TypeSpec::SumMember(Box::new(type_spec), name.clone()),
                got => {
                    return ParseError::unexpected(
                        got, 
                        vec![TokName::VarName, TokName::TypeName]
                    );
                },
            }
        } else if lexer.peek_by(0) == Ok(&Tok::LBrack) && lexer.peek_by(1) == Ok(&Tok::RBrack) {
            lexer.next_tok()?;
            lexer.next_tok()?;

            TypeSpec::ArrayElem(Box::new(type_spec))
        } else {
            break
        }
    };

    Ok(type_spec)
}

pub fn parse_type_decl(mut lexer: TypeParseContext) -> Result<TypeDecl, ParseError> {
    let spec = parse_type(&mut lexer)?;

    let contains_generics = lexer.has_generics();
    let name = lexer.inner_data.clone();

    let type_decl = TypeDecl {
        name,
        typ: spec,
        contains_generics,
    };

    Ok(type_decl)
}

pub fn parse_struct(lexer: &mut TypeParseContext) -> ParseResult<TypeSpec> {
    let mut fields = Vec::<FieldSpec>::new();

    #[derive(PartialEq)]
    enum ObjectKind { Struct, Union, Unknown };
    let mut parsing = ObjectKind::Unknown;

    lexer.assert_next_tok_is(Tok::LCurly, TokName::LCurly)?;

    while !lexer.move_on(Tok::RCurly) {
        let inherited = if lexer.peek_tok()? == &Tok::Plus {
            lexer.next_tok()?;
            true 
        } else {
            false
        };

        let name = lexer.next_tok()?.var_name()?;

        lexer.assert_next_tok_is(Tok::Colon, TokName::Colon)?;

        let type_spec = lexer.recover(parse_type);

        match lexer.peek_tok()? {
            Tok::RCurly => { 
                lexer.next_tok()?; 
                fields.push(FieldSpec { inherited, name, type_spec });
                break 
            },

            Tok::Comma if parsing == ObjectKind::Unknown => parsing = ObjectKind::Struct,
            Tok::Comma if parsing == ObjectKind::Struct => {},
            Tok::Bar if parsing == ObjectKind::Unknown => parsing = ObjectKind::Union,
            Tok::Bar if parsing == ObjectKind::Union => {},

            Tok::Bar if parsing == ObjectKind::Struct => 
                return Err(ParseError::ImproperObjectDelimiters),
            Tok::Comma if parsing == ObjectKind::Union => 
                return Err(ParseError::ImproperObjectDelimiters),

            got if parsing == ObjectKind::Struct => 
                return ParseError::unexpected(got, vec![TokName::Comma]),
            got if parsing == ObjectKind::Union => 
                return ParseError::unexpected(got, vec![TokName::Bar]),
            got => 
                return ParseError::unexpected(got, vec![TokName::Comma, TokName::Bar]),
        }

        lexer.next_tok()?;
        fields.push(FieldSpec { inherited, name, type_spec });
    }

    if parsing == ObjectKind::Struct || parsing == ObjectKind::Unknown {
        Ok(TypeSpec::Struct(fields))
    } else {
        Ok(TypeSpec::Union(fields))
    }
}

pub fn parse_sum(lexer: &mut TypeParseContext) -> ParseResult<TypeSpec> {
    fn parse_variant(lexer: &mut TypeParseContext) -> ParseResult<(TypeName, TypeSpec)> {
        let variant_name = lexer.next_tok()?.type_name()?;

        if !matches!(variant_name, TypeName::Other(_)) {
            return Err(ParseError::BadVariantName(variant_name));
        }

        lexer.assert_next_tok_is(Tok::Colon, TokName::Colon)?;

        let typ = parse_type(lexer)?;

        Ok((variant_name, typ))
    }

    let fields = 
        parse_list(Tok::curlys(), Some((Tok::Slash, TokName::Slash)), parse_variant, lexer)?;

    Ok(TypeSpec::Sum(fields))
}
