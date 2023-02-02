use super::*;
use crate::lex::{lex, Tok};
use crate::typ::FloatType;
use crate::Type;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    Void,
    Type(Type),
    Me,
}

impl TypeSpec {
    pub fn get_ref(self) -> TypeSpec { TypeSpec::Ref(box self) }
    pub fn unknown() -> TypeSpec { TypeSpec::Type(Type::unknown()) }
}

impl From<&str> for TypeSpec {
    fn from(value: &str) -> Self {
        let mut lexed = lex(value);
        let generic_labels = ["T", "S", "U", "V", "W"]
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
    Ok(parse_type(&mut temp_lexer)?)
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

        let new_union = TypeSpec::Union(box left, box right);
        types_between_ops.insert(plus_pos, new_union);
    }

    assert_eq!(types_between_ops.len(), 1, "fatal error when parsing type operations");

    Ok(types_between_ops.remove(0))
}

fn parse_type_atom(lexer: &mut TypeParseContext) -> ParseResult<TypeSpec> {
    let beginning_of_alias = lexer.peek_tok()?.clone();

    let type_alias = match beginning_of_alias {
        Tok::LCurly => match (lexer.peek_by(1), lexer.peek_by(2)) {
            (Ok(Tok::VarName(_)), Ok(Tok::Colon)) => parse_struct(lexer)?,
            (Ok(Tok::TypeName(_)), Ok(Tok::Colon)) => parse_sum(lexer)?,
            _ => {
                TypeSpec::Tuple(parse_list(Tok::curlys(), Some(Tok::Comma), parse_type, lexer)?)
            },
        },
        Tok::AmpersandSet(count) => {
            lexer.next_tok()?;

            let mut output = parse_type(lexer)?;

            for _ in 0..count {
                output = TypeSpec::Ref(box output);
            }

            output
        },
        Tok::AsterickSet(count) => {
            lexer.next_tok()?;

            let mut output = parse_type(lexer)?;

            for _ in 0..count {
                output = TypeSpec::Deref(box output);
            }

            output
        },
        Tok::LBrack => {
            lexer.next_tok()?;
            let count = lexer.next_tok()?.int_value()?;
            lexer.next_tok()?;

            TypeSpec::Array(box parse_type(lexer)?, count.try_into().unwrap())
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

            {
                let probably_arrow = lexer.next_tok()?;
                if probably_arrow != Tok::Semicolon {
                    return Err(ParseError::UnexpectedTok {
                        got: probably_arrow,
                        expected: vec![TokName::Semicolon],
                    });
                }
            }

            let ret_type = parse_type(lexer)?;

            TypeSpec::Function(arg_types, box ret_type)
        },
        _ => {
            return Err(ParseError::UnexpectedTok {
                got: beginning_of_alias,
                expected: vec![TokName::TypeName, TokName::LCurly, TokName::Ref],
            })
        },
    };

    let final_output = match lexer.peek_tok().clone() {
        Ok(Tok::Dot) => {
            lexer.next_tok()?;

            match lexer.next_tok()? {
                Tok::VarName(name) => TypeSpec::StructMember(box type_alias, name),
                Tok::TypeName(name) => TypeSpec::SumMember(box type_alias, name),
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

        let typ = parse_type(lexer)?;

        Ok((field_name, typ))
    }

    let fields = parse_list(Tok::curlys(), Some(Tok::Comma), parse_field, lexer)?;

    Ok(TypeSpec::Struct(fields))
}

pub fn parse_sum(lexer: &mut TypeParseContext) -> ParseResult<TypeSpec> {
    fn parse_variant(lexer: &mut TypeParseContext) -> ParseResult<(TypeName, TypeSpec)> {
        let variant_name = lexer.next_tok()?.type_name()?;
        // TODO: throw error if variant_name is not a regular type name, and is a
        // primitive instead, e.g. `i32` or `f64`

        lexer.assert_next_tok_is(Tok::Colon)?;

        let typ = parse_type(lexer)?;

        Ok((variant_name, typ))
    }

    let fields = parse_list(Tok::curlys(), Some(Tok::Divider), parse_variant, lexer)?;

    Ok(TypeSpec::Sum(fields))
}
