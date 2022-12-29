use super::*;
use crate::lex::Tok;
use crate::typ::FloatType;
use crate::Type;
use std::collections::HashSet;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TypeSpec {
    Named(TypeName),
    Generic(TypeName, Vec<TypeSpec>),
    GenParam(u8),
    Int(u32),
    Float(FloatType),
    Bool,
    Ref(Box<TypeSpec>),
    Struct(Vec<(VarName, TypeSpec)>),
    Tuple(Vec<TypeSpec>),
    Function(Vec<TypeSpec>, Box<TypeSpec>),
    Array(Box<TypeSpec>, u32),
    Union(Box<TypeSpec>, Box<TypeSpec>),
    Void,
    Type(Type),
}

impl TypeSpec {
    pub fn get_ref(self) -> TypeSpec { TypeSpec::Ref(box self) }
}

#[derive(Default)]
pub struct StructParsingContext {
    pub name: String,
    pub dependencies: HashSet<String>,
}

fn parse_type(lexer: &mut ParseContext<TypeName>) -> Result<TypeSpec, ParseError> {
    let beginning_of_alias = lexer.peek_tok()?.clone();

    let type_alias = match beginning_of_alias {
        Tok::LCurly => {
            if matches!(lexer.peek_by(1)?, Tok::VarName(_)) {
                parse_struct(lexer)?
            } else {
                TypeSpec::Tuple(parse_list(Tok::curlys(), Some(Tok::Comma), parse_type, lexer)?)
            }
        },
        Tok::AmpersandSet(count) => {
            lexer.next_tok()?;

            let mut output = parse_type(lexer)?;

            for _ in 0..count {
                output = TypeSpec::Ref(box output);
            }

            output
        },
        Tok::TypeName(name) => {
            lexer.next_tok()?;
            match name {
                TypeName::I(size) => TypeSpec::Int(size),
                TypeName::Bool => TypeSpec::Bool,
                TypeName::F64 => TypeSpec::Float(FloatType::F64),
                TypeName::F32 => TypeSpec::Float(FloatType::F32),
                TypeName::F16 => TypeSpec::Float(FloatType::F16),
                TypeName::Anonymous => unreachable!(),
                TypeName::Other(_) => {
                    if let Some(generic_index) = lexer.get_generic_label(&name) {
                        TypeSpec::GenParam(generic_index)
                    } else if let Tok::LAngle = lexer.peek_tok()? {
                        let generics =
                            parse_list(Tok::angles(), Some(Tok::Comma), parse_type, lexer)?;

                        lexer.push_type_dependency(name.clone());

                        TypeSpec::Generic(name, generics)
                    } else {
                        lexer.push_type_dependency(name.clone());

                        TypeSpec::Named(name)
                    }
                },
            }
        },
        Tok::LParen => {
            let arg_types = parse_list(Tok::parens(), Some(Tok::Comma), parse_type, lexer)?;

            {
                let probably_arrow = lexer.next_tok()?;
                if probably_arrow != Tok::RArrow {
                    return Err(ParseError::UnexpectedTok {
                        got: probably_arrow,
                        expected: vec![TokName::RArrow],
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

    let suffix = lexer.peek_tok().clone();

    let type_alias = match suffix {
        Ok(Tok::LBrack) => {
            lexer.next_tok()?;
            let count = lexer.next_tok()?.int_value()?;
            lexer.next_tok()?;

            TypeSpec::Array(box type_alias, count.try_into().unwrap())
        },
        Ok(Tok::Plus) => {
            lexer.next_tok()?;
            let other_alias = parse_type(lexer)?;

            TypeSpec::Union(box type_alias, box other_alias)
        },
        _ => type_alias,
    };

    Ok(type_alias)
}

pub fn parse_type_decl(mut lexer: ParseContext<TypeName>) -> Result<TypeDecl, ParseError> {
    let alias = parse_type(&mut lexer)?;

    let contains_generics = lexer.has_generics();
    let (name, dependencies) = lexer.return_info();

    let type_decl = TypeDecl {
        name,
        typ: alias,
        contains_generics,
        dependencies,
    };

    Ok(type_decl)
}

pub fn parse_type_spec(lexer: &mut ParseContext<VarName>) -> Result<TypeSpec, ParseError> {
    let mut temp_lexer = lexer.create_new_with_name(TypeName::Anonymous);
    Ok(parse_type(&mut temp_lexer)?)
}

pub fn parse_struct(lexer: &mut ParseContext<TypeName>) -> Result<TypeSpec, ParseError> {
    fn parse_field(
        lexer: &mut ParseContext<TypeName>,
    ) -> Result<(VarName, TypeSpec), ParseError> {
        let field = lexer.next_tok()?;
        let Tok::VarName(name) = field else {
            return Err(ParseError::UnexpectedTok { 
                got: field, 
                expected: vec![TokName::VarName] 
            });
        };

        lexer.assert_next_tok_is(Tok::Colon)?;

        let typ = parse_type(lexer)?;

        Ok((name, typ))
    }

    let fields = parse_list((Tok::LCurly, Tok::RCurly), Some(Tok::Comma), parse_field, lexer)?;

    Ok(TypeSpec::Struct(fields))
}

enum StructPart {
    Field { name: VarName, typ: TypeSpec },
    Method { is_static: bool, decl: FuncCode },
}
