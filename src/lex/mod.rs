use crate::parse::Opcode;
use logos::{Lexer, Logos};
use syn::token::{And, Or};

pub mod parse_num;
mod token;

pub fn lex(input: &str) -> Lexer<Token> {
    Token::lexer(input)
}

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
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
    pub fn get_un_opcode(&self) -> Option<Opcode> {
        match self {
            Token::AsterickSet(count) => Some(Opcode::Deref(*count)),
            Token::AmpersandSet(count) => Some(Opcode::Ref(*count)),
            _ => None,
        }
    }

    pub fn get_bin_opcode(&self) -> Option<Opcode> {
        match self {
            Token::AsterickSet(2) => Some(Opcode::Exponential),
            Token::Plus => Some(Opcode::Plus),
            Token::Minus => Some(Opcode::Minus),
            Token::AsterickSet(1) => Some(Opcode::Multiplier),
            Token::Divider => Some(Opcode::Divider),
            Token::Modulus => Some(Opcode::Modulus),
            Token::AmpersandSet(1) => Some(Opcode::BitAND),
            Token::BitOR => Some(Opcode::BitOR),
            Token::BitXOR => Some(Opcode::BitXOR),
            Token::BitShiftR => Some(Opcode::BitShiftR),
            Token::BitShiftL => Some(Opcode::BitShiftL),
            Token::AmpersandSet(2) => Some(Opcode::BitAND),
            Token::Or => Some(Opcode::Or),
            Token::LeftAngle => Some(Opcode::LessThan),
            Token::RghtAngle => Some(Opcode::GrtrThan),
            Token::LessOrEqual => Some(Opcode::LessOrEqual),
            Token::GreaterOrEqual => Some(Opcode::GreaterOrEqual),
            Token::Equal => Some(Opcode::Equal),
            Token::Dot => Some(Opcode::Dot),
            _ => None,
        }
    }
}
