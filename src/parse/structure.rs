use super::*;
use crate::lex::{lex, Tok};
use crate::typ::FloatType;
use crate::Type;

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
    Struct(Vec<(VarName, TypeSpec)>),
    Sum(Vec<(TypeName, TypeSpec)>),
    Tuple(Vec<TypeSpec>),
    Function(Vec<TypeSpec>, Box<TypeSpec>),
    FuncReturnType(Box<TypeSpec>),
    TypeLevelFunc(TypeName, Vec<TypeSpec>),
    Array(Box<TypeSpec>, u32),
    ArrayElem(Box<TypeSpec>),
    Union(Box<TypeSpec>, Box<TypeSpec>),

    #[default]
    Void,
    Type(Type),
    Me,
}

impl TypeSpec {
    pub fn get_ref(self) -> TypeSpec { TypeSpec::Ref(Box::new(self)) }
    pub fn get_deref(self) -> TypeSpec { TypeSpec::Deref(Box::new(self)) }
    pub fn unknown() -> TypeSpec { TypeSpec::Type(Type::unknown()) }
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
}

pub fn parse_type_spec(lexer: &mut FuncParseContext) -> Result<TypeSpec, ParseError> {
    let mut temp_lexer = lexer.split(TypeName::Anonymous, lexer.generic_labels.clone());
    parse_type(&mut temp_lexer)
}

fn parse_type(lexer: &mut TypeParseContext) -> ParseResult<TypeSpec> {
    let mut types_between_ops = Vec::<TypeSpec>::new();
    let mut ops = Vec::<TypeOps>::new();

    loop {
        types_between_ops.push(parse_type_atom(lexer)?);

        let op = match lexer.peek_tok() {
            Ok(Tok::Plus) => TypeOps::Plus,
            _ => break,
        };

        lexer.next_tok()?;
        ops.push(op);
    }

    while let Some(plus_pos) = ops.iter().position(|a| *a == TypeOps::Plus) {
        ops.remove(plus_pos);

        let right = types_between_ops.remove(plus_pos + 1);
        let left = types_between_ops.remove(plus_pos);

        let new_union = TypeSpec::Union(Box::new(left), Box::new(right));
        types_between_ops.insert(plus_pos, new_union);
    }

    assert_eq!(types_between_ops.len(), 1, "fatal error when parsing type operations");

    Ok(types_between_ops.remove(0))
}

fn parse_type_atom(lexer: &mut TypeParseContext) -> ParseResult<TypeSpec> {
    let beginning_of_alias = lexer.peek_tok()?;

    let type_alias = match beginning_of_alias {
        Tok::LCurly => match (lexer.peek_by(1), lexer.peek_by(2)) {
            (Ok(Tok::VarName(_)), Ok(Tok::Colon)) => parse_struct(lexer)?,
            (Ok(Tok::TypeName(_)), Ok(Tok::Colon)) => parse_sum(lexer)?,
            _ => {
                lexer.assert_next_tok_is(Tok::LCurly)?;

                let mut elems = Vec::new();

                if lexer.peek_tok()? == Tok::RCurly {
                    lexer.next_tok()?;
                    return Ok(TypeSpec::Tuple(Vec::new()));
                }

                elems.push(parse_type(lexer)?);

                match (lexer.next_tok()?, lexer.peek_tok()) {
                    (Tok::RCurly, _) => elems.into_iter().next().unwrap(),
                    (Tok::Comma, Ok(Tok::RCurly)) => {
                        lexer.assert_next_tok_is(Tok::RCurly)?;
                        TypeSpec::Tuple(elems)
                    },
                    (Tok::Comma, Ok(_)) => {
                        loop {
                            if lexer.peek_tok() == Ok(Tok::RCurly) {
                                lexer.next_tok()?;
                                break;
                            }

                            elems.push(parse_type(lexer)?);

                            match lexer.next_tok()? {
                                Tok::Comma => continue,
                                Tok::RCurly => break,
                                got => {
                                    return Err(ParseError::UnexpectedTok {
                                        expected: vec![TokName::RCurly, TokName::Comma],
                                        got,
                                    })
                                },
                            }
                        }

                        TypeSpec::Tuple(elems)
                    },
                    (got, _) => {
                        return Err(ParseError::UnexpectedTok {
                            expected: vec![TokName::RCurly, TokName::Comma],
                            got,
                        })
                    },
                }
            },
        },
        Tok::AmpersandSet(count) => {
            lexer.next_tok()?;

            let mut output = parse_type(lexer)?;

            for _ in 0..count {
                output = output.get_ref();
            }

            output
        },
        Tok::AsterickSet(count) => {
            lexer.next_tok()?;

            let mut output = parse_type(lexer)?;

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
        Tok::TypeName(name) => {
            lexer.next_tok()?;

            match name {
                TypeName::I(size) => TypeSpec::Int(size),
                TypeName::U(size) => TypeSpec::UInt(size),
                TypeName::Bool => TypeSpec::Bool,
                TypeName::F64 => TypeSpec::Float(FloatType::F64),
                TypeName::F32 => TypeSpec::Float(FloatType::F32),
                TypeName::F16 => TypeSpec::Float(FloatType::F16),
                TypeName::Me => TypeSpec::Me,
                TypeName::Other(_) => {
                    if lexer.peek_tok() == Ok(Tok::LParen) {
                        let type_level_func_args =
                            parse_list(Tok::parens(), Some(Tok::Comma), parse_type, lexer)?;

                        TypeSpec::TypeLevelFunc(name, type_level_func_args)
                    } else if let Some(generic_index) = lexer.get_generic_label(&name) {
                        TypeSpec::GenParam(generic_index)
                    } else if lexer.peek_tok() == Ok(Tok::LAngle) {
                        let generics =
                            parse_list(Tok::angles(), Some(Tok::Comma), parse_type, lexer)?;

                        TypeSpec::Generic(name, generics)
                    } else {
                        TypeSpec::Named(name)
                    }
                },
                TypeName::Anonymous => unreachable!(),
            }
        },
        Tok::LParen => {
            let arg_types = parse_list(Tok::parens(), Some(Tok::Comma), parse_type, lexer)?;

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
            return Err(ParseError::UnexpectedTok {
                got: beginning_of_alias,
                expected: vec![TokName::TypeName, TokName::LCurly, TokName::Ref],
            })
        },
    };

    let final_output = match lexer.peek_tok() {
        Ok(Tok::Dot) => {
            lexer.next_tok()?;

            match lexer.next_tok()? {
                Tok::VarName(name) => TypeSpec::StructMember(Box::new(type_alias), name),
                Tok::TypeName(name) => TypeSpec::SumMember(Box::new(type_alias), name),
                got => {
                    return Err(ParseError::UnexpectedTok {
                        got,
                        expected: vec![TokName::VarName, TokName::TypeName],
                    })
                },
            }
        },
        _ => type_alias,
    };

    Ok(final_output)
}

pub fn parse_type_decl(mut lexer: TypeParseContext) -> Result<TypeDecl, ParseError> {
    let alias = parse_type(&mut lexer)?;

    let contains_generics = lexer.has_generics();
    let name = lexer.name.clone();

    let type_decl = TypeDecl {
        name,
        typ: alias,
        contains_generics,
    };

    Ok(type_decl)
}

pub fn parse_struct(lexer: &mut TypeParseContext) -> ParseResult<TypeSpec> {
    fn parse_field(lexer: &mut TypeParseContext) -> ParseResult<(VarName, TypeSpec)> {
        let field_name = lexer.next_tok()?.var_name()?;

        lexer.assert_next_tok_is(Tok::Colon)?;

        let typ = lexer.recover(parse_type)?;

        Ok((field_name, typ))
    }

    let fields = parse_list(Tok::curlys(), Some(Tok::Comma), parse_field, lexer)?;

    Ok(TypeSpec::Struct(fields))
}

pub fn parse_sum(lexer: &mut TypeParseContext) -> ParseResult<TypeSpec> {
    fn parse_variant(lexer: &mut TypeParseContext) -> ParseResult<(TypeName, TypeSpec)> {
        let variant_name = lexer.next_tok()?.type_name()?;

        if !matches!(variant_name, TypeName::Other(_)) {
            return Err(ParseError::BadVariantName(variant_name));
        }

        lexer.assert_next_tok_is(Tok::Colon)?;

        let typ = parse_type(lexer)?;

        Ok((variant_name, typ))
    }

    let fields = parse_list(Tok::curlys(), Some(Tok::Divider), parse_variant, lexer)?;

    Ok(TypeSpec::Sum(fields))
}
