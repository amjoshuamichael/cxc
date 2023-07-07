use crate::{lex::Tok, Type, TypeName, VarName};
use std::fmt::{Debug, Display};

use super::{Expr, FuncCode, TypeSpec, TypeSpecRelation, VarDecl};

pub trait Errable {
    fn err() -> Self;
}

impl Errable for () {
    fn err() -> Self {}
}

impl Errable for Expr {
    fn err() -> Self { Expr::Error }
}

impl Errable for TypeSpec {
    fn err() -> Self { TypeSpec::Type(Type::unknown()) }
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
            code: Expr::err(),
            relation: TypeSpecRelation::Unrelated,
        }
    }
}

impl<T: Errable> Errable for Option<T> {
    fn err() -> Self { Some(T::err()) }
}

impl<T: Errable, U: Errable> Errable for (T, U) {
    fn err() -> Self { (T::err(), U::err()) }
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
    Divider,
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

impl From<Tok> for TokName {
    fn from(tok: Tok) -> Self {
        use Tok::*;
        match tok {
            Comma => TokName::Comma,
            Semicolon => TokName::Semicolon,
            Bang => TokName::Bang,
            Plus => TokName::Plus,
            Minus => TokName::Minus,
            AsterickSet(_) => todo!(),
            Divider => TokName::Divider,
            Modulus => TokName::Modulus,
            BitOR => TokName::BitOR,
            BitXOR => TokName::BitXOR,
            BitShiftR => TokName::BitShiftR,
            BitShiftL => TokName::BitShiftL,
            Dot => TokName::Dot,
            DoublePlus => TokName::DoublePlus,
            DoubleMinus => TokName::DoubleMinus,
            TripleMinus => TokName::TripleMinus,
            AmpersandSet(_) => todo!(),
            Or => TokName::Or,
            LessOrEqual => TokName::LessOrEqual,
            GreaterOrEqual => TokName::GreaterOrEqual,
            Equal => todo!(),
            Inequal => todo!(),
            Question => TokName::Question,
            Colon => TokName::Colon,
            At => TokName::At,
            ColonDot => TokName::ColonDot,
            DoubleColon => TokName::DoubleColon,
            LParen => TokName::LParen,
            RParen => TokName::RParen,
            LCurly => TokName::LCurly,
            RCurly => TokName::RCurly,
            LBrack => TokName::LBrack,
            RBrack => TokName::RBrack,
            LAngle => TokName::LAngle,
            RAngle => TokName::RightAngle,
            LArrow => TokName::LArrow,
            RArrow => TokName::RArrow,
            Assignment => TokName::Assignment,
            VarName(_) => TokName::VarName,
            TypeName(_) => TokName::TypeName,
            Int(_) => TokName::Int,
            // Could also be a tuple member, but this is a safe bet.
            DottedNum(_) => TokName::Float,
            Float(_) => TokName::Float,
            Bool(_) => TokName::Bool,
            Strin(_) => TokName::Strin,
            Error => TokName::Error,
            Space | Comment | Return | Tab => TokName::Whitespace,
        }
    }
}
