use crate::{lex::Tok, TypeName, VarName, typ::ABI};
use std::{fmt::{Debug, Display}, rc::Rc};

use super::{Expr, FuncCode, TypeSpec, TypeSpecRelation, VarDecl};

pub trait Errable {
    fn err() -> Self;
}

impl Errable for () {
    fn err() -> Self {}
}

impl Errable for bool {
    fn err() -> Self { false }
}

impl Errable for Expr {
    fn err() -> Self { Expr::Error }
}

impl Errable for TypeSpec {
    fn err() -> Self { TypeSpec::Unknown }
}

impl Errable for VarDecl {
    fn err() -> Self {
        VarDecl {
            name: VarName::Error,
            type_spec: TypeSpec::err(),
        }
    }
}

impl Errable for VarName {
    fn err() -> Self { Self::Error }
}
impl Errable for TypeName {
    fn err() -> Self { Self::Anonymous }
}

impl Errable for FuncCode {
    fn err() -> Self {
        Self {
            name: VarName::Error,
            ret_type: TypeSpec::err(),
            args: Vec::new(),
            generic_count: 0,
            code: Rc::new(Expr::err()),
            relation: TypeSpecRelation::Unrelated,
            is_external: false,
            abi: ABI::C,
        }
    }
}

impl<T: Errable> Errable for Option<T> {
    fn err() -> Self { Some(T::err()) }
}

impl<T: Errable, U: Errable> Errable for (T, U) {
    fn err() -> Self { (T::err(), U::err()) }
}

impl<T: Errable, U: Errable, V: Errable> Errable for (T, U, V) {
    fn err() -> Self { (T::err(), U::err(), V::err()) }
}

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    IncorrectBeginningOfDeclaration,
    UnexpectedEndOfFile,
    UnexpectedTok { got: Tok, expected: Vec<TokName> },
    ImproperExpression,
    ArgListWithImproperPredecessor,
    BadVariantName(TypeName),
}

impl ParseError {
    pub(crate) fn unexpected<T>(got: &Tok, expected: Vec<TokName>) -> ParseResult<T> {
        Err(ParseError::UnexpectedTok { got: got.clone(), expected })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseErrorSpanned {
    pub error: ParseError,
    pub start: usize,
    pub end: usize,
    pub tokens_between: TokenSpan,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenSpan(pub Vec<Tok>);

impl TokenSpan {
    pub fn new(all: &Vec<Tok>, start: usize, end: usize) -> Self {
        Self(
            all[start..end.min(all.len())]
                .iter()
                .map(|tok| tok.clone())
                .collect(),
        )
    }
}

impl Display for ParseErrorSpanned {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.error)?;

        for tok in &self.tokens_between.0 {
            write!(f, "{}", &*tok.to_string())?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokName {
    Comma,
    Semicolon,
    Bang,

    Plus,
    Minus,
    Multiply,
    Slash,
    Modulus,

    BitOR,
    BitXOR,
    BitAND,
    BitShiftR,
    BitShiftL,

    DoublePlus,
    DoubleMinus,
    TripleMinus,

    Ref,
    Deref,

    And,
    Or,

    Dot,

    LessThan,
    GreaterThan,
    LessOrEqual,
    GreaterOrEqual,

    Question,
    Colon,
    At,

    ColonDot,
    DoubleColon,

    LParen,
    RParen,
    LCurly,
    RCurly,
    LBrack,
    RBrack,
    LAngle,
    RAngle,
    LArrow,
    RArrow,

    RightAngle,

    Assignment,
    VarAssignment,
    FieldAssignment,

    VarName,
    TypeName,

    Int,
    Float,
    Bool,
    Strin,

    Error,
    Whitespace,

    Operator,
    UnaryOperator,
    BinaryOperator,
}

pub type TokWithName = (Tok, TokName);
