use std::fmt::Display;

use crate::{
    parse::ParseErrorSpanned, typ::SumType, StructType, Type, TypeName, UniqueFuncInfo, VarName,
};

pub type CResultMany<T> = Result<T, Vec<CErr>>;
pub type CResult<T> = Result<T, CErr>;
pub type TResult<T> = Result<T, TErr>;

#[derive(Debug)]
pub enum CErr {
    Parse(ParseErrorSpanned),
    Type(TErr),
    Func(FErr),
}

impl Display for CErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Parse(parse_error) => write!(f, "{parse_error}"),
            Self::Type(type_error) => write!(f, "{type_error:?}"),
            Self::Func(func_error) => write!(f, "{func_error:?}"),
        }
    }
}

impl From<TErr> for CErr {
    fn from(value: TErr) -> Self { Self::Type(value) }
}

impl From<FErr> for CErr {
    fn from(value: FErr) -> Self { Self::Func(value) }
}

#[derive(Debug)]
pub enum TErr {
    Unknown(TypeName),
    UnknownFunc(TypeName),
    FieldNotFound(StructType, VarName),
    NoFieldOnNonStruct(Type, VarName),
    VariantNotFound(SumType, TypeName),
    NoVariantOnNonEnum(Type, TypeName),
    CantGetGeneric(Type, Vec<Type>, u32),
    TooFewGenerics(Vec<Type>, u8),
}

#[derive(Debug)]
pub enum FErr {
    NotFound(UniqueFuncInfo),
}
