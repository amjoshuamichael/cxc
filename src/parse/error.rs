use crate::{lex::Tok, Type};

use super::{Expr, TypeSpec};

pub trait Errable {
    fn err() -> Self;
}

impl Errable for Expr {
    fn err() -> Self { Expr::Error }
}

impl Errable for TypeSpec {
    fn err() -> Self { TypeSpec::Type(Type::unknown()) }
}

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    IncorrectBeginningOfDeclaration,
    UnexpectedEndOfFile,
    UnexpectedTok { got: Tok, expected: Vec<TokName> },
    ImproperExpression,
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
            Space | Comment | Return => TokName::Whitespace,
        }
    }
}
