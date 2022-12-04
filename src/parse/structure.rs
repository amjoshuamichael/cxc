use super::*;
use crate::lex::Tok;
use crate::typ::FloatType;
use std::collections::HashSet;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TypeAlias {
    Named(TypeName),
    Generic(TypeName, Vec<TypeAlias>),
    GenParam(u8),
    Int(u32),
    Float(FloatType),
    Bool,
    Ref(Box<TypeAlias>),
    Struct(Vec<(VarName, TypeAlias)>),
    Function(Vec<TypeAlias>, Box<TypeAlias>),
    Array(Box<TypeAlias>, u32),
    Union(Box<TypeAlias>, Box<TypeAlias>),
}

#[derive(Default)]
pub struct StructParsingContext {
    pub name: String,
    pub dependencies: HashSet<String>,
}

fn parse_type(lexer: &mut ParseContext<TypeName>) -> Result<TypeAlias, ParseError> {
    let beginning_of_alias = lexer.peek_tok()?.clone();

    let type_alias = match beginning_of_alias {
        Tok::LeftCurly => parse_struct(lexer)?,
        Tok::AmpersandSet(count) => {
            lexer.next_tok()?;

            let mut output = parse_type(lexer)?;

            for _ in 0..count {
                output = TypeAlias::Ref(box output);
            }

            output
        },
        Tok::TypeName(name) => {
            lexer.next_tok()?;
            match name {
                TypeName::I(size) => TypeAlias::Int(size),
                TypeName::Bool => TypeAlias::Bool,
                TypeName::F64 => TypeAlias::Float(FloatType::F64),
                TypeName::F32 => TypeAlias::Float(FloatType::F32),
                TypeName::F16 => TypeAlias::Float(FloatType::F16),
                TypeName::Anonymous => unreachable!(),
                TypeName::Other(_) => {
                    if let Some(generic_index) = lexer.get_generic_label(&name) {
                        TypeAlias::GenParam(generic_index)
                    } else if let Tok::LeftAngle = lexer.peek_tok()? {
                        let generics = parse_list(
                            Tok::angles(),
                            Some(Tok::Comma),
                            parse_type,
                            lexer,
                        )?;

                        lexer.push_type_dependency(name.clone());

                        TypeAlias::Generic(name, generics)
                    } else {
                        lexer.push_type_dependency(name.clone());

                        TypeAlias::Named(name)
                    }
                },
            }
        },
        Tok::LeftParen => {
            let arg_types =
                parse_list(Tok::parens(), Some(Tok::Comma), parse_type, lexer)?;

            {
                let probably_arrow = lexer.next_tok()?;
                if probably_arrow != Tok::RghtArrow {
                    return Err(ParseError::UnexpectedTok {
                        got: probably_arrow,
                        expected: vec![TokName::RightArrow],
                    });
                }
            }

            let ret_type = parse_type(lexer)?;

            TypeAlias::Function(arg_types, box ret_type)
        },
        _ => {
            return Err(ParseError::UnexpectedTok {
                got: beginning_of_alias,
                expected: vec![TokName::TypeName, TokName::LeftCurly, TokName::Ref],
            })
        },
    };

    let suffix = lexer.peek_tok().clone();

    let type_alias = match suffix {
        Ok(Tok::LeftBrack) => {
            lexer.next_tok()?;
            let count = lexer.next_tok()?.int_value()?;
            lexer.next_tok()?;

            TypeAlias::Array(box type_alias, count.try_into().unwrap())
        },
        Ok(Tok::Plus) => {
            lexer.next_tok()?;
            let other_alias = parse_type(lexer)?;

            TypeAlias::Union(box type_alias, box other_alias)
        },
        _ => type_alias,
    };

    Ok(type_alias)
}

pub fn parse_type_decl(
    mut lexer: ParseContext<TypeName>,
) -> Result<TypeDecl, ParseError> {
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

pub fn parse_type_alias(
    lexer: &mut ParseContext<VarName>,
) -> Result<TypeAlias, ParseError> {
    let temp_lexer = lexer.create_new_with_name(TypeName::Anonymous);
    Ok(parse_type_decl(temp_lexer)?.typ)
}

pub fn parse_struct(
    lexer: &mut ParseContext<TypeName>,
) -> Result<TypeAlias, ParseError> {
    fn parse_field(
        lexer: &mut ParseContext<TypeName>,
    ) -> Result<(VarName, TypeAlias), ParseError> {
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

    let fields = parse_list(
        (Tok::LeftCurly, Tok::RghtCurly),
        Some(Tok::Comma),
        parse_field,
        lexer,
    )?;

    Ok(TypeAlias::Struct(fields))
}

enum StructPart {
    Field { name: VarName, typ: TypeAlias },
    Method { is_static: bool, decl: FuncCode },
}
