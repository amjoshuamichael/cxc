use crate::{lex::Tok, TypeName, VarName, typ::ABI};
use std::{fmt::{Debug, Display}, sync::Arc};

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
            code: Arc::new(Expr::err()),
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
    ImproperObjectDelimiters,
    ImproperExpression,
    ArgListWithImproperPredecessor,
    BadVariantName(TypeName),
}

impl ParseError {
    pub(crate) fn unexpected<T>(got: &Tok, expected: Vec<TokName>) -> ParseResult<T> {
        Err(ParseError::UnexpectedTok { got: got.clone(), expected })
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::IncorrectBeginningOfDeclaration => 
                write!(f, "Declaration cannot begin like this"),
            ParseError::UnexpectedEndOfFile => 
                write!(f, "Unexpected end of file"),
            ParseError::UnexpectedTok { got, expected } => 
                write!(f, "Got a bad token, expected {expected:?} got {got:?}"),
            ParseError::ImproperExpression =>
                write!(f, "Bad expression"),
            ParseError::ArgListWithImproperPredecessor => todo!(),
            ParseError::ImproperObjectDelimiters => 
                write!(f, "Mixed delimiters for object type. Use either only `,` for structs, or only `|` for unions."),
            ParseError::BadVariantName(name) => 
                write!(f, "name {name} is not a valid variant name"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseErrorSpanned {
    pub error: ParseError,
    pub start: usize,
    pub end: usize,
    pub at: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokName {
    Comma,
    Bar,
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
