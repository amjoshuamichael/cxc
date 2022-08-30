use crate::parse::{GenFuncDependency, GenericLabels, Opcode, TypeAlias};
use logos::{Lexer as LogosLexer, Logos};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    iter::Peekable,
    ops::Deref,
    rc::Rc,
};
use syn::token::Token;

#[derive(Clone)]
struct TokPos(Rc<RefCell<usize>>);

impl TokPos {
    pub fn inc(&self) { *self.0.borrow_mut() += 1; }
    pub fn zero() -> Self { TokPos(Rc::new(RefCell::new(0))) }
    pub fn val(&self) -> usize { *self.0.borrow() }
}

pub trait TokenStream {
    fn next_tok(&mut self) -> Option<Tok>;
    fn peek_tok(&mut self) -> Option<Tok>;
}

pub mod parse_num;

pub struct Lexer {
    inner: Vec<Tok>,
    tok_pos: TokPos,
}

impl TokenStream for Lexer {
    fn next_tok(&mut self) -> Option<Tok> {
        let out = self.inner.get(self.tok_pos.val());

        self.tok_pos.inc();

        if crate::DEBUG {
            println!("pre-lexing: {:?}", out?);
        }

        out.cloned()
    }

    fn peek_tok(&mut self) -> Option<Tok> {
        self.inner.get(self.tok_pos.val()).cloned()
    }
}

impl Lexer {
    pub fn from(lexer: LogosLexer<'_, Tok>) -> Lexer {
        Lexer {
            inner: lexer.collect(),
            tok_pos: TokPos::zero(),
        }
    }

    pub fn split(
        &mut self,
        name: String,
        generic_labels: GenericLabels,
    ) -> ParseContext {
        if crate::DEBUG {
            println!("splitting the lexer to parse {name}");
        }

        let output = ParseContext {
            inner: self.inner.clone(),
            tok_pos: self.tok_pos.clone(),
            name,
            generic_labels,
            func_dependencies: Vec::new(),
            type_dependencies: HashSet::new(),
        };

        output
    }
}

pub struct ParseContext {
    inner: Vec<Tok>,
    tok_pos: TokPos,
    name: String,
    generic_labels: GenericLabels,
    func_dependencies: Vec<GenFuncDependency>,
    type_dependencies: HashSet<String>,
}

impl TokenStream for ParseContext {
    fn next_tok(&mut self) -> Option<Tok> {
        let out = self.get(self.tok_pos.val(), true);
        self.tok_pos.inc();
        out
    }

    fn peek_tok(&mut self) -> Option<Tok> { self.get(self.tok_pos.val(), false) }
}

impl ParseContext {
    pub fn next_if<F: Fn(Tok) -> bool>(&mut self, cond: F) -> Option<Tok> {
        let next = self.peek_tok()?;

        if cond(next.clone()) {
            self.next_tok();
            Some(next)
        } else {
            None
        }
    }

    pub fn peek_by(&self, offset: usize) -> Option<Tok> {
        self.get(self.tok_pos.val() + offset, false)
    }

    fn get(&self, at: usize, log: bool) -> Option<Tok> {
        let token = self.inner.get(at).cloned();

        if crate::DEBUG && log && let Some(token) = token.clone() {
            println!("lexing: {:?}", token);
        }

        token
    }

    pub fn push_func_dependency(&mut self, dep: GenFuncDependency) {
        self.func_dependencies.push(dep);
    }

    pub fn push_type_dependency(&mut self, name: String) {
        self.type_dependencies.insert(name);
    }

    pub fn get_generic_label(&self, label: &String) -> Option<u8> {
        self.generic_labels.get(label).copied()
    }

    pub fn has_generics(&self) -> bool { self.generic_labels.len() > 0 }

    pub fn name_of_this(&self) -> String { self.name.clone() }

    pub fn return_info(self) -> (String, Vec<GenFuncDependency>, HashSet<String>) {
        (self.name, self.func_dependencies, self.type_dependencies)
    }

    pub fn create_new_with_name(&self, name: String) -> Self {
        Self {
            inner: self.inner.clone(),
            name,
            tok_pos: self.tok_pos.clone(),
            generic_labels: self.generic_labels.clone(),
            func_dependencies: Vec::new(),
            type_dependencies: HashSet::new(),
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
