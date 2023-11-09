use std::{fmt::Display, error::Error};

use crate::{
    parse::{ParseErrorSpanned, TypeSpec}, Type, TypeName, VarName, hlr::expr_tree::ExprID, TypeRelation, TypeEnum,
};

pub type CResultMany<T> = Result<T, Vec<CErr>>;
pub type CResult<T> = Result<T, CErr>;
pub type TResult<T> = Result<T, TErr>;

#[derive(Debug)]
pub enum CErr {
    Parse(ParseErrorSpanned),
    Type(TErr),
    Func(FErr),
    Usage(UErr),
}

impl From<TErr> for CErr {
    fn from(value: TErr) -> Self { Self::Type(value) }
}

impl From<FErr> for CErr {
    fn from(value: FErr) -> Self { Self::Func(value) }
}

impl From<UErr> for CErr {
    fn from(value: UErr) -> Self { Self::Usage(value) }
}

impl From<TErr> for Vec<CErr> {
    fn from(value: TErr) -> Self { vec![CErr::Type(value)] }
}

impl From<UErr> for Vec<CErr> {
    fn from(value: UErr) -> Self { vec![CErr::Usage(value)] }
}

impl From<CErr> for Vec<CErr> {
    fn from(value: CErr) -> Self { vec![value] }
}


#[derive(Clone, Debug)]
pub enum TErr {
    Unknown(TypeName),
    UnknownFunc(TypeName),
    FieldNotFound(TypeEnum, VarName),
    NoFieldOnNonStruct(Type, VarName),
    NoVariantOnNonEnum(Type, TypeName),
    CantGetGeneric(Type, Vec<Type>, u32),
    TooFewGenerics(TypeSpec, Vec<Type>, u8),
    WrongArgCount(usize, usize),
    CantDeref(Type),
    NotAFunction(Type),
    NotAStruct(Type),
    NotAnArray(Type),
}

#[derive(Debug)]
pub enum FErr {
    NotFound(VarName, TypeRelation),
}

#[derive(Debug)]
pub enum UErr {
    NoReturn,
    BadTypeOfStructLit(ExprID),
}

#[derive(Default, Debug)]
pub struct ParseErrorReport {
    pub parse_errors: Vec<ParseErrorSpanned>,
    pub code: Box<str>,
}

impl Display for ParseErrorReport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for err in &self.parse_errors {
            writeln!(f, "{}", err.error);
            let mut line_start = err.start;

            for line in self.code[err.start..err.end].lines() {
                let line_end = line_start + line.len();
                
                writeln!(f, "{line}");
                if line_start < err.at && err.at < line_end {
                    writeln!(f, "{}", "^".repeat(line.len()));
                }

                line_start += line.len();    
            }
            writeln!(f, "-------------------")?;
        }

        Ok(())
    }
}
impl Error for ParseErrorReport { }
