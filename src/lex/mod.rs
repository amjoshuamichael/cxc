use crate::parse::Opcode;
use logos::{Lexer as LogosLexer, Logos};
use syn::token::{And, Or};

pub mod parse_num;

pub struct Lexer {
    inner: Vec<Token>,
    current_ptr: usize,
}

impl Lexer {
    pub fn next(&mut self) -> Option<Token> {
        let out = self.get(self.current_ptr, true);
        self.current_ptr += 1;
        out
    }

    pub fn peek(&self) -> Option<Token> {
        self.get(self.current_ptr, false)
    }

    pub fn peek_by(&self, offset: usize) -> Option<Token> {
        self.get(self.current_ptr + offset, false)
    }

    fn get(&self, at: usize, log: bool) -> Option<Token> {
        let token = self.inner.get(at).map(|t| t.clone());

        if crate::DEBUG && log && let Some(token) = token.clone() {
            println!("lexing: {:?}", token);
        }

        token
    }
}

impl From<LogosLexer<'_, Token>> for Lexer {
    fn from(og: LogosLexer<Token>) -> Self {
        Lexer {
            inner: og.collect(),
            current_ptr: 0,
        }
    }
}

pub fn lex(input: &str) -> Lexer {
    Lexer::from(Token::lexer(input))
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

    #[regex(r"[A-Za-z_][A-Za-z0-9_]+(:[A-Za-z0-9_]+)*|[A-za-z_]", |lex| String::from(lex.slice()))]
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
            Token::Inequal => Some(Opcode::Inequal),
            Token::Dot => Some(Opcode::Dot),
            _ => None,
        }
    }
}
