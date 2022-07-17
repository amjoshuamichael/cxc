use super::parse_num;
use crate::parse::*;
use logos::Logos;
use syn::token::{And, Or};

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token("!")]
    Bang,

    #[token("**")]
    Exponential,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Multiplier,
    #[token("/")]
    Divider,
    #[token("%")]
    Modulus,
    #[token("&")]
    BitAND,
    #[token("|")]
    BitOR,
    #[token("^")]
    BitXOR,
    #[token(">>")]
    BitShiftR,
    #[token("<<")]
    BitShiftL,

    #[token("&&")]
    And,
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

    #[regex("[A-Za-z:_][A-Za-z0-9:_]*", |lex| String::from(lex.slice()))]
    Ident(String),

    #[regex(
        "0b[01][01_]*|0o[0-8][0-8_]*|0x[0-9abcdef][0-9abcdef_]*|[0-9][0-9_]*",
        parse_num::parse_int
    )]
    Int(u128),

    #[regex(r"[0-9_]*\.[0-9_]*(e[+-]?[0-9_]+)?", parse_num::parse_float)]
    Float(f64),

    #[error]
    Error,

    #[regex(r"(#.*\n)|[ \t\n\f]+", logos::skip)]
    Whitespace,
}

impl Token {
    pub fn get_opcode(&self) -> Option<Opcode> {
        match self {
            Plus => Some(Opcode::Plus),
            Minus => Some(Opcode::Minus),
            Multiplier => Some(Opcode::Multiplier),
            Divider => Some(Opcode::Divider),
            Modulus => Some(Opcode::Modulus),
            BitAND => Some(Opcode::BitAND),
            BitOR => Some(Opcode::BitOR),
            BitXOR => Some(Opcode::BitXOR),
            BitShiftR => Some(Opcode::BitShiftR),
            BitShiftL => Some(Opcode::BitShiftL),
            And => Some(Opcode::And),
            Or => Some(Opcode::Or),
            LessOrEqual => Some(Opcode::LessOrEqual),
            GreaterOrEqual => Some(Opcode::GreaterOrEqual),
            _ => None,
        }
    }
}
