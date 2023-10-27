use super::parse_num::*;
use crate::parse::Opcode;
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::parse::ParsedFloat;
use crate::parse::TokName;
use crate::parse::TokWithName;
use logos::{Lexer, Logos};
use std::{
    fmt::{Debug, Display},
    sync::Arc,
};
use crate::{XcReflect, xc_opaque};

use crate as cxc;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, XcReflect)]
#[repr(C)]
#[xc_opaque]
pub enum VarName {
    Other(Arc<str>),
    TupleIndex(usize),
    None,
    Error,
    Sret,
}

impl Default for VarName {
    fn default() -> Self { Self::None }
}

impl Display for VarName {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            VarName::TupleIndex(ti) => write!(f, "{ti}"),
            _ => write!(f, "{}", &**self),
        }
    }
}

impl Debug for VarName {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl From<&str> for VarName {
    fn from(s: &str) -> Self { 
        Self::Other(Arc::from(s)) 
    }
}

impl From<String> for VarName {
    fn from(s: String) -> Self { Self::Other(Arc::from(&*s)) }
}

impl std::ops::Deref for VarName {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self {
            VarName::Other(o) => &**o,
            VarName::TupleIndex(0) => "0",
            VarName::TupleIndex(1) => "1",
            VarName::TupleIndex(2) => "2",
            VarName::TupleIndex(3) => "3",
            VarName::TupleIndex(4) => "4",
            VarName::TupleIndex(5) => "5",
            VarName::TupleIndex(6) => "6",
            VarName::TupleIndex(7) => "7",
            VarName::TupleIndex(8) => "8",
            VarName::TupleIndex(9) => "9",
            VarName::TupleIndex(_) => "#",
            VarName::None => "$none",
            VarName::Error => "$error",
            VarName::Sret => "$sret",
        }
    }
}

#[derive(PartialEq, Default, Eq, Clone, Hash, PartialOrd, Ord, XcReflect)]
#[xc_opaque]
pub enum TypeName {
    Other(Arc<str>),
    I(u32),
    U(u32),
    F64,
    F32,
    F16,
    Bool,
    Me,
    #[default]
    Anonymous,
}

impl TypeName {
    fn from_tok(t: &mut Lexer<Tok>) -> Self {
        match t.slice().chars().next() {
            Some('i') => match &t.slice()[1..] {
                "size" => Self::I(64),
                num => {
                    Self::I(num.parse().expect("improper int type"))
                }
            },
            Some('u') => match &t.slice()[1..] {
                "size" => Self::U(64),
                num => Self::U(num.parse().expect("improper int type")),
            },
            _ => match t.slice() {
                "f16" => Self::F16,
                "f32" => Self::F32,
                "f64" => Self::F64,
                "bool" => Self::Bool,
                "Me" => Self::Me,
                _ => Self::Other(Arc::from(t.slice())),
            },
        }
    }

    pub fn error() -> Self { Self::from("Error") }
}

impl Display for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Debug for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let out = match self {
            Self::Other(name) => name,
            Self::I(size) => return write!(f, "i{size}"),
            Self::U(size) => return write!(f, "u{size}"),
            Self::F64 => "f64",
            Self::F32 => "f32",
            Self::F16 => "f16",
            Self::Bool => "bool",
            Self::Me => "Me",
            Self::Anonymous => "unnamed",
        };

        write!(f, "{}", out)
    }
}

impl From<&str> for TypeName {
    fn from(s: &str) -> Self { Self::Other(Arc::from(s)) }
}

impl TypeName {
    pub fn to_string_zero_if_anonymous(&self) -> String {
        match self {
            TypeName::Anonymous => String::new(),
            other => other.to_string(),
        }
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
    Slash,
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
    #[token("~")]
    Tilde,

    #[token("++")]
    DoublePlus,
    #[token("--")]
    DoubleMinus,

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
    #[token("for")]
    For,

    #[token("with")]
    With,
    #[token("as")]
    As,

    #[token(":.")]
    ColonDot,
    #[token("::", priority = 3)]
    DoubleColon,

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LCurly,
    #[token("}")]
    RCurly,
    #[token("[")]
    LBrack,
    #[token("]")]
    RBrack,
    #[token("<")]
    LAngle,
    #[token(">")]
    RAngle,
    #[token("<-")]
    LArrow,
    #[token("->")]
    RArrow,

    #[token("---")]
    TripleMinus,

    #[token("=")]
    Assignment,

    #[regex(
        ":([a-z_][a-zA-Z0-9_]+|[a-z_])",
        |t| VarName::Other(Arc::from(&t.slice()[1..]))
    )]

    Label(VarName),

    #[regex(
        "[a-z_][a-zA-Z0-9_]+|\
        [a-z_]",
        |t| VarName::Other(Arc::from(t.slice()))
    )]
    VarName(VarName),

    #[regex(
        "[A-Z][A-Za-z0-9]+|\
        [A-Z]|\
        u[0-9]+|i[0-9]+|usize|isize|f16|f32|f64|f128|bool|str",
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
    Int(u64),

    // could be a float (e.g. .1), but could also be a tuple member (e.g. the ".1" in x.1)
    #[regex(r"[0-9_]*\.[0-9_]+", parse_dotted_int)]
    DottedNum((u128, u128)),

    // this is definitely a float, because it uses e for scientific notation
    #[regex(r"[0-9_]*\.[0-9_]+e[+-]?[0-9_]+", parse_float)]
    Float(ParsedFloat),

    #[regex("true|false", parse_bool)]
    Bool(bool),

    #[regex(r#""[^"]*""#, parse_string)]
    Strin(Arc<str>),

    #[error]
    Error,

    #[regex(r"(#.*\n)+")]
    Comment,

    // Called it line feed instead of return in order to avoid getting it confused with
    // actual returns
    #[regex(r"(\r\n?|\n)+")]
    LineFeed, 

    #[regex(r"\t")]
    Tab,

    #[regex(r"[ \t\f]+")]
    Space,
}

impl Tok {
    pub fn get_un_opcode(&self) -> ParseResult<Opcode> {
        match self {
            Tok::AsterickSet(_) => Ok(Opcode::Deref),
            Tok::AmpersandSet(_) => Ok(Opcode::Ref),
            Tok::Bang => Ok(Opcode::Not),
            Tok::Plus => Ok(Opcode::Transform),
            Tok::Tilde => Ok(Opcode::Destroy),
            _ => Err(ParseError::UnexpectedTok {
                got: self.clone(),
                expected: vec![TokName::UnaryOperator],
            }),
        }
    }

    pub fn is_unary_op(&self) -> bool { self.get_un_opcode().is_ok() }

    pub fn get_bin_opcode(&self) -> Result<Opcode, ParseError> {
        match self {
            Tok::Plus => Ok(Opcode::Plus),
            Tok::Minus => Ok(Opcode::Minus),
            Tok::AsterickSet(1) => Ok(Opcode::Multiplier),
            Tok::Slash => Ok(Opcode::Divider),
            Tok::Modulus => Ok(Opcode::Modulus),
            Tok::AmpersandSet(1) => Ok(Opcode::BitAND),
            Tok::BitOR => Ok(Opcode::BitOR),
            Tok::BitXOR => Ok(Opcode::BitXOR),
            Tok::BitShiftR => Ok(Opcode::BitShiftR),
            Tok::BitShiftL => Ok(Opcode::BitShiftL),
            Tok::AmpersandSet(2) => Ok(Opcode::And),
            Tok::Or => Ok(Opcode::Or),
            Tok::LAngle => Ok(Opcode::LessThan),
            Tok::RAngle => Ok(Opcode::GrtrThan),
            Tok::LessOrEqual => Ok(Opcode::LessOrEqual),
            Tok::GreaterOrEqual => Ok(Opcode::GreaterOrEqual),
            Tok::Equal => Ok(Opcode::Equal),
            Tok::Inequal => Ok(Opcode::Inequal),
            Tok::Dot => Ok(Opcode::Dot),
            err => Err(ParseError::UnexpectedTok {
                got: err.clone(),
                expected: vec![TokName::BinaryOperator],
            }),
        }
    }

    pub fn is_bin_op(&self) -> bool { self.get_bin_opcode().is_ok() }

    pub fn var_name(&self) -> Result<VarName, ParseError> {
        match self {
            Tok::VarName(name) => Ok(name.clone()),
            got => ParseError::unexpected(got, vec![TokName::VarName]),
        }
    }

    pub fn type_name(&self) -> Result<TypeName, ParseError> {
        match self {
            Tok::TypeName(name) => Ok(name.clone()),
            got => ParseError::unexpected(got, vec![TokName::TypeName]),
        }
    }

    pub fn int_value(&self) -> Result<u64, ParseError> {
        match self {
            Tok::Int(val) => Ok(*val),
            got => ParseError::unexpected(got, vec![TokName::Int]),
        }
    }

    pub fn is_whitespace(&self) -> bool {
        use Tok::*;
        matches!(self, Space | Comment | LineFeed | Tab)
    }

    pub fn parens() -> (TokWithName, TokWithName) { 
        ((Tok::LParen, TokName::LParen), (Tok::RParen, TokName::RParen)) 
    }
    pub fn bracks() -> (TokWithName, TokWithName) { 
        ((Tok::LBrack, TokName::LBrack), (Tok::RBrack, TokName::RBrack)) 
    }
    pub fn curlys() -> (TokWithName, TokWithName) { 
        ((Tok::LCurly, TokName::LCurly), (Tok::RCurly, TokName::RCurly)) 
    }
    pub fn angles() -> (TokWithName, TokWithName) { 
        ((Tok::LAngle, TokName::LAngle), (Tok::RAngle, TokName::RAngle)) 
    }
}

impl ToString for Tok {
    fn to_string(&self) -> String {
        use Tok::*;

        match self {
            Comma => ",",
            Semicolon => ";",
            Bang => "!",
            Plus => "+",
            Minus => "-",
            AsterickSet(count) => return "*".repeat(*count as _),
            Slash => "/",
            Modulus => "%",
            BitOR => "|",
            BitXOR => "^",
            BitShiftR => ">>",
            BitShiftL => "<<",
            Dot => ".",
            Tilde => "~",
            DoublePlus => "++",
            DoubleMinus => "--",
            TripleMinus => "---",
            AmpersandSet(count) => return "&".repeat(*count as _),
            Or => "||",
            LessOrEqual => "<=",
            GreaterOrEqual => ">=",
            Equal => "==",
            Inequal => "!=",
            Question => "?",
            Colon => ":",
            At => "@",
            With => "with",
            As => "as",
            For => "for",
            ColonDot => ":.",
            DoubleColon => "::",
            LParen => "(",
            RParen => ")",
            LCurly => "{",
            RCurly => "}",
            LBrack => "[",
            RBrack => "]",
            LAngle => "<",
            RAngle => ">",
            LArrow => "<-",
            RArrow => "->",
            Assignment => "=",
            VarName(name) => return name.to_string(),
            Label(name) => return format!(":{name}"),
            TypeName(name) => return name.to_string(),
            Int(value) => return value.to_string(),
            DottedNum((left, right)) => return format!("{left}.{right}"),
            Float(value) => return value.to_string(),
            Bool(value) => return value.to_string(),
            Strin(value) => value,
            Error => "!error!",
            Comment => "#...\n",
            LineFeed => "\n",
            Space => " ",
            Tab => "\t",
        }
        .into()
    }
}
