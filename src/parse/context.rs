use crate::parse::{Errable, GenericLabels, ParseError, ParseResult, TokName};
use std::{cell::RefCell, rc::Rc};

use crate::lex::{Tok, TypeName, VarName};
use passable::Pass;

use super::{ParseErrorSpanned, TokenSpan};

#[derive(Debug, Clone, Default)]
struct SharedNum(Rc<RefCell<usize>>);

impl SharedNum {
    pub fn inc(&self) { *self.0.borrow_mut() += 1 }
    pub fn dec(&self) { *self.0.borrow_mut() -= 1 }
    pub fn val(&self) -> usize { *self.0.borrow() }
}

pub type GlobalParseContext = ParseContext<()>;
pub type FuncParseContext = ParseContext<VarName>;
pub type TypeParseContext = ParseContext<TypeName>;

#[derive(Default)]
pub struct ParseContext<N> {
    pub inner: Rc<Vec<Tok>>,
    tok_pos: SharedNum,
    scope: SharedNum,
    pub name: N,
    pub generic_labels: GenericLabels,
    pub errors: Pass<Vec<ParseErrorSpanned>>,
}

impl<N: Default> ParseContext<N> {
    pub fn new(tokens: Vec<Tok>) -> Self {
        Self {
            inner: Rc::new(tokens),
            ..Default::default()
        }
    }

    pub fn get_generic_label(&self, label: &TypeName) -> Option<u8> {
        self.generic_labels.get(label).copied()
    }

    pub fn has_generics(&self) -> bool { !self.generic_labels.is_empty() }

    pub fn split<T: std::fmt::Debug>(
        &mut self,
        name: T,
        generic_labels: GenericLabels,
    ) -> ParseContext<T> {
        ParseContext {
            name,
            generic_labels,
            inner: self.inner.clone(),
            tok_pos: self.tok_pos.clone(),
            scope: self.scope.clone(),
            errors: self.errors.pass().unwrap(),
        }
    }

    pub fn generic_count(&self) -> usize { self.generic_labels.len() }

    pub fn peek_by(&self, offset: usize) -> ParseResult<Tok> {
        let mut current = self.tok_pos.val();
        let mut token = &Tok::Space;

        for _ in 0..=offset {
            token = &Tok::Space;
            while token.is_whitespace() {
                token = self
                    .inner
                    .get(current)
                    .ok_or(ParseError::UnexpectedEndOfFile)?;
                current += 1;
            }
        }

        Ok(token.clone())
    }

    pub fn peek_tok(&mut self) -> ParseResult<Tok> { self.peek_by(0) }

    pub fn next_tok(&mut self) -> ParseResult<Tok> {
        let mut next = &Tok::Space;

        while next.is_whitespace() {
            next = self
                .inner
                .get(self.tok_pos.val())
                .ok_or(ParseError::UnexpectedEndOfFile)?;

            Self::check_scope(&self.scope, &next);
            self.tok_pos.inc();
        }

        if crate::XC_DEBUG {
            print!("{}", next.to_string());
        }

        Ok(next.clone())
    }

    pub fn assert_next_tok_is(&mut self, tok: Tok) -> Result<(), ParseError> {
        let next = self.next_tok()?;

        if next != tok {
            return Err(ParseError::UnexpectedTok {
                got: next,
                expected: vec![TokName::from(tok)],
            });
        }

        Ok(())
    }

    pub fn recover<O: Errable>(
        &mut self,
        parser: impl Fn(&mut Self) -> ParseResult<O>,
        skip_until: Vec<Tok>,
    ) -> ParseResult<O> {
        let start = self.tok_pos.val();
        let beginning_scope = self.scope.val();

        match parser(self) {
            Ok(expr) => Ok(expr),
            Err(error) => {
                if crate::XC_DEBUG {
                    println!("...recovering...");
                }

                loop {
                    let next_token = self
                        .inner
                        .get(self.tok_pos.val())
                        .ok_or(ParseError::UnexpectedEndOfFile)?;

                    if next_token == &skip_until[0] && self.scope.val() == beginning_scope {
                        break;
                    }

                    Self::check_scope(&self.scope, &next_token);
                    self.tok_pos.inc();
                }

                let end = self.tok_pos.val();
                let spanned = ParseErrorSpanned {
                    error,
                    start,
                    end,
                    tokens_between: TokenSpan::new(&self.inner, start, end),
                };

                self.errors.deref_mut().unwrap().push(spanned);

                Ok(O::err())
            },
        }
    }

    // increase the scope number if tok is < ( [ { and decrease if tok is > ) ] }
    fn check_scope(scope: &SharedNum, tok: &Tok) {
        match tok {
            Tok::LParen | Tok::LBrack | Tok::LCurly => scope.inc(),
            Tok::RParen | Tok::RBrack | Tok::RCurly => scope.dec(),
            _ => {},
        }
    }
}
