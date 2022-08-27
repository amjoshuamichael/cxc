use crate::parse::Opcode;
use logos::{Lexer as LogosLexer, Logos};

pub mod parse_num;

pub struct Lexer {
    inner: Vec<Tok>,
    current_ptr: usize,
}

impl Lexer {
    pub fn next(&mut self) -> Option<Tok> {
        let out = self.get(self.current_ptr, true);
        self.current_ptr += 1;
        out
    }

    pub fn next_if<F: Fn(Tok) -> bool>(&mut self, cond: F) -> Option<Tok> {
        let next = self.peek()?;

        if cond(next.clone()) {
            self.next();
            Some(next)
        } else {
            None
        }
    }

    pub fn peek(&self) -> Option<Tok> { self.get(self.current_ptr, false) }

    pub fn peek_by(&self, offset: usize) -> Option<Tok> {
        self.get(self.current_ptr + offset, false)
    }

    fn get(&self, at: usize, log: bool) -> Option<Tok> {
        let token = self.inner.get(at).map(|t| t.clone());

        if crate::DEBUG && log && let Some(token) = token.clone() {
            println!("lexing: {:?}", token);
        }

        token
    }
}

impl From<LogosLexer<'_, Tok>> for Lexer {
    fn from(og: LogosLexer<Tok>) -> Self {
        Lexer {
            inner: og.collect(),
            current_ptr: 0,
        }
    }
}

pub fn lex(input: &str) -> Lexer { Lexer::from(Tok::lexer(input)) }

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

    pub fn ident_name(self) -> Option<String> {
        match self {
            Tok::Ident(name) => Some(name),
            _ => None,
        }
    }
}
