use super::parse_num;
use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    #[token(";")]
    Semicolon,

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

    #[regex("[A-Za-z_][A-Za-z0-9_]*", |lex| String::from(lex.slice()))]
    Ident(String),

    #[regex(
        "0b[01][01_]*|0o[0-8][0-8_]*|0x[0-9abcdef][0-9abcdef_]*|[0-9][0-9_]*",
        parse_num::parse_int
    )]
    Int(u128),

    #[regex(r"[0-9_]*\.[0-9_]*(e[0-9_]+)?", parse_num::parse_float)]
    Float(String),

    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}
