use super::parse_num::*;
use crate::parse::Opcode;
use logos::{Lexer, Logos};
use std::{
    fmt::{Debug, Display},
    sync::Arc,
};

pub trait Ident {
    fn from_tok(t: &mut Lexer<Tok>) -> Self;
    fn to_string(&self) -> String;
}

#[derive(PartialEq, Eq, Clone, Hash)]
pub struct VarName(Arc<str>);

impl Ident for VarName {
    fn from_tok(t: &mut Lexer<Tok>) -> Self { Self(Arc::from(t.slice())) }
    fn to_string(&self) -> String { String::from(&*self.0) }
}

impl Display for VarName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Debug for VarName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<&str> for VarName {
    fn from(s: &str) -> Self { Self(Arc::from(s)) }
}

#[derive(PartialEq, Eq, Clone, Hash)]
pub enum TypeName {
    Other(Arc<str>),
    I64,
    I32,
    I16,
    I8,
    F64,
    F32,
    F16,
    Anonymous,
}

impl Ident for TypeName {
    fn from_tok(t: &mut Lexer<Tok>) -> Self {
        match t.slice() {
            "i64" => Self::I64,
            "i32" => Self::I32,
            "i16" => Self::I16,
            "i8" => Self::I8,
            "f16" => Self::F16,
            "f32" => Self::F32,
            "f64" => Self::F64,
            _ => Self::Other(Arc::from(t.slice())),
        }
    }

    fn to_string(&self) -> String { format!("{self:?}") }
}

impl Display for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Debug for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let out = match self {
            Self::Other(name) => &*name,
            Self::I64 => "i64",
            Self::I32 => "i32",
            Self::I16 => "i16",
            Self::I8 => "i8",
            Self::F64 => "f64",
            Self::F32 => "f32",
            Self::F16 => "f16",
            Self::Anonymous => "unnamed",
        };

        write!(f, "{}", out)
    }
}

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Tok {
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token("!")]
    Bang,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[regex(r"\*+", |set| set.slice().len() as u8)]
    AsterickSet(u8),
    #[token("/")]
    Divider,
    #[token("%")]
    Modulus,
    #[token("|")]
    BitOR,
    #[token("^")]
    BitXOR,
    #[token(">>")]
    BitShiftR,
    #[token("<<")]
    BitShiftL,
    // Dots require a higher priority than floats, because "." technically
    // classifies as a float under the current regex statement.
    #[token(".", priority = 3)]
    Dot,

    #[regex(r"&+", |set| set.slice().len() as u8)]
    AmpersandSet(u8),
    #[token("||")]
    Or,

    #[token("<=")]
    LessOrEqual,
    #[token(">=")]
    GreaterOrEqual,

    #[token("==")]
    Equal,
    #[token("!=")]
    Inequal,

    #[token("?")]
    Question,
    #[token(":")]
    Colon,
    #[token("@")]
    At,

    #[token("(")]
    LeftParen,
    #[token(")")]
    RghtParen,
    #[token("{")]
    LeftCurly,
    #[token("}")]
    RghtCurly,
    #[token("[")]
    LeftBrack,
    #[token("]")]
    RghtBrack,

    #[token("<")]
    LeftAngle,
    #[token(">")]
    RghtAngle,

    #[token("=")]
    Assignment,

    #[regex(
        "[a-z_][A-Za-z0-9_]+|\
        [a-z_]",
        VarName::from_tok
    )]
    VarName(VarName),

    #[regex(
        "[A-Z][A-Za-z0-9_]+(:[A-Za-z0-9_]+)*|\
        [A-Z]|\
        i8|i16|i32|i64|f16|f32|f64|f128",
        TypeName::from_tok,
        priority = 2
    )]
    TypeName(TypeName),

    #[regex(
        "0b[01][01_]*|\
        0o[0-8][0-8_]*|\
        0x[0-9abcdef][0-9abcdef_]*|\
        [0-9][0-9_]*",
        parse_int
    )]
    Int(u128),

    #[regex(r"[0-9_]*\.[0-9_]*(e[+-]?[0-9_]+)?", parse_float)]
    Float(f64),

    #[error]
    Error,

    #[regex(r"(#.*\n)|[ \t\n\f]+", logos::skip)]
    Whitespace,
}

impl Tok {
    pub fn get_un_opcode(&self) -> Option<Opcode> {
        match self {
            Tok::AsterickSet(count) => Some(Opcode::Deref(*count)),
            Tok::AmpersandSet(count) => Some(Opcode::Ref(*count)),
            _ => None,
        }
    }

    pub fn is_un_op(&self) -> bool { self.get_un_opcode().is_some() }

    pub fn get_bin_opcode(&self) -> Option<Opcode> {
        match self {
            Tok::AsterickSet(2) => Some(Opcode::Exponential),
            Tok::Plus => Some(Opcode::Plus),
            Tok::Minus => Some(Opcode::Minus),
            Tok::AsterickSet(1) => Some(Opcode::Multiplier),
            Tok::Divider => Some(Opcode::Divider),
            Tok::Modulus => Some(Opcode::Modulus),
            Tok::AmpersandSet(1) => Some(Opcode::BitAND),
            Tok::BitOR => Some(Opcode::BitOR),
            Tok::BitXOR => Some(Opcode::BitXOR),
            Tok::BitShiftR => Some(Opcode::BitShiftR),
            Tok::BitShiftL => Some(Opcode::BitShiftL),
            Tok::AmpersandSet(2) => Some(Opcode::BitAND),
            Tok::Or => Some(Opcode::Or),
            Tok::LeftAngle => Some(Opcode::LessThan),
            Tok::RghtAngle => Some(Opcode::GrtrThan),
            Tok::LessOrEqual => Some(Opcode::LessOrEqual),
            Tok::GreaterOrEqual => Some(Opcode::GreaterOrEqual),
            Tok::Equal => Some(Opcode::Equal),
            Tok::Inequal => Some(Opcode::Inequal),
            Tok::Dot => Some(Opcode::Dot),
            _ => None,
        }
    }

    pub fn is_bin_op(&self) -> bool { self.get_bin_opcode().is_some() }

    pub fn var_name(self) -> Option<VarName> {
        match self {
            Tok::VarName(name) => Some(name),
            _ => None,
        }
    }
}
