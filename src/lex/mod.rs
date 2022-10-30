use crate::parse::{GenericLabels, ParseError};
use logos::{Lexer as LogosLexer, Logos};
use std::{cell::RefCell, collections::HashSet, fmt::Display, rc::Rc};

pub use tok::{Tok, TypeName, VarName};

use tok::Ident;

mod parse_num;
mod tok;

#[derive(Debug, Clone)]
struct TokPos(Rc<RefCell<usize>>);

impl TokPos {
    pub fn inc(&self) { *self.0.borrow_mut() += 1; }
    pub fn zero() -> Self { TokPos(Rc::new(RefCell::new(0))) }
    pub fn val(&self) -> usize { *self.0.borrow() }
}

pub trait TokenStream {
    fn next_tok(&mut self) -> Result<Tok, ParseError>;
    fn peek_tok(&mut self) -> Result<Tok, ParseError>;
}

pub struct Lexer {
    inner: Vec<Tok>,
    tok_pos: TokPos,
}

impl TokenStream for Lexer {
    fn next_tok(&mut self) -> Result<Tok, ParseError> {
        let out = self.inner.get(self.tok_pos.val());

        self.tok_pos.inc();

        match out {
            Some(tok) => {
                if crate::DEBUG {
                    println!("pre-lexing: {:?}", tok);
                }

                Ok(tok.clone())
            },
            None => Err(ParseError::UnexpectedEndOfFile),
        }
    }

    fn peek_tok(&mut self) -> Result<Tok, ParseError> {
        self.inner
            .get(self.tok_pos.val())
            .map_or(Err(ParseError::UnexpectedEndOfFile), |t| Ok(t.clone()))
    }
}

impl Lexer {
    pub fn from(lexer: LogosLexer<'_, Tok>) -> Lexer {
        Lexer {
            inner: lexer.collect(),
            tok_pos: TokPos::zero(),
        }
    }

    pub fn split<N: Ident + Display>(
        &mut self,
        name: N,
        generic_labels: GenericLabels,
    ) -> ParseContext<N> {
        if crate::DEBUG {
            println!(
                "splitting the lexer at index {:?} to parse {name}",
                self.tok_pos.val()
            );
        }

        ParseContext {
            inner: self.inner.clone(),
            tok_pos: self.tok_pos.clone(),
            name,
            generic_labels,
            type_dependencies: HashSet::new(),
        }
    }
}

pub struct ParseContext<N: Ident> {
    inner: Vec<Tok>,
    tok_pos: TokPos,
    name: N,
    generic_labels: GenericLabels,
    type_dependencies: HashSet<TypeName>,
}

impl<N: Ident> TokenStream for ParseContext<N> {
    fn next_tok(&mut self) -> Result<Tok, ParseError> {
        let out = self.get(self.tok_pos.val(), true);
        self.tok_pos.inc();
        out
    }

    fn peek_tok(&mut self) -> Result<Tok, ParseError> {
        self.get(self.tok_pos.val(), false)
    }
}

impl<N: Ident> ParseContext<N> {
    pub fn peek_by(&self, offset: usize) -> Result<Tok, ParseError> {
        self.get(self.tok_pos.val() + offset, false)
    }

    fn get(&self, at: usize, log: bool) -> Result<Tok, ParseError> {
        let token = self.inner.get(at);

        if crate::DEBUG && log && let Some(token) = token {
            println!("lexing: {:?}", token);
        }

        token.map_or(Err(ParseError::UnexpectedEndOfFile), |t| Ok(t.clone()))
    }

    pub fn push_type_dependency(&mut self, name: TypeName) {
        self.type_dependencies.insert(name);
    }

    pub fn get_generic_label(&self, label: &TypeName) -> Option<u8> {
        self.generic_labels.get(label).copied()
    }

    pub fn has_generics(&self) -> bool { !self.generic_labels.is_empty() }

    pub fn name_of_this(&self) -> &N { &self.name }

    pub fn return_info(self) -> (N, HashSet<TypeName>) {
        (self.name, self.type_dependencies)
    }

    pub fn create_new_with_name<T: Ident>(&self, name: T) -> ParseContext<T> {
        ParseContext::<T> {
            inner: self.inner.clone(),
            name,
            tok_pos: self.tok_pos.clone(),
            generic_labels: self.generic_labels.clone(),
            type_dependencies: HashSet::new(),
        }
    }

    pub fn generic_count(&self) -> usize { self.generic_labels.len() }
}

pub fn lex(input: &str) -> Lexer { Lexer::from(Tok::lexer(input)) }
