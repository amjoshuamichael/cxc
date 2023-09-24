use crate::parse::{Errable, GenericLabels, ParseError, ParseResult, TokName};
use std::{cell::RefCell, rc::Rc};

use crate::lex::{Tok, TypeName, VarName};
use passable::Pass;

use super::{ParseErrorSpanned, TokenSpan};

pub type GlobalParseContext = ParseContext<()>;
pub type FuncParseContext = ParseContext<VarName>;
pub type TypeParseContext = ParseContext<TypeName>;

#[derive(Debug, Clone, Default)]
struct SharedNum(Rc<RefCell<usize>>);

impl SharedNum {
    pub fn inc(&self) { *self.0.borrow_mut() += 1 }
    pub fn dec(&self) { *self.0.borrow_mut() -= 1; }
    pub fn val(&self) -> usize { *self.0.borrow() }
    pub fn detach(&self) -> Self {
        let num = (*self.0).clone();
        Self(Rc::new(num))
    }
}

#[derive(Default)]
pub struct ParseContext<N> {
    pub tokens: Rc<Vec<Tok>>,
    pub spans: Rc<Vec<(usize, usize)>>,
    tok_pos: SharedNum,
    scope: SharedNum,
    pub name: N,
    pub generic_labels: GenericLabels,
    pub errors: Pass<Vec<ParseErrorSpanned>>,
}

impl<N: Default + Clone> ParseContext<N> {
    pub fn new(tokens: Vec<Tok>, spans: Vec<(usize, usize)>) -> Self {
        Self {
            tokens: Rc::new(tokens),
            spans: Rc::new(spans),
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
            tokens: self.tokens.clone(),
            spans: self.spans.clone(),
            tok_pos: self.tok_pos.clone(),
            scope: self.scope.clone(),
            errors: self.errors.pass().unwrap(),
        }
    }

    pub fn detach(&mut self) -> Self {
        Self {
            name: self.name.clone(),
            generic_labels: self.generic_labels.clone(),
            tokens: self.tokens.clone(),
            spans: self.spans.clone(),
            tok_pos: self.tok_pos.detach(),
            scope: self.scope.clone(),
            errors: Pass::default(),
        }
    }

    pub fn generic_count(&self) -> usize { self.generic_labels.len() }

    pub fn peek_by(&self, offset: usize) -> ParseResult<&Tok> {
        let mut current = self.tok_pos.val();
        let mut token = &Tok::Space;

        for _ in 0..=offset {
            token = &Tok::Space;
            while token.is_whitespace() {
                token = self
                    .tokens
                    .get(current)
                    .ok_or(ParseError::UnexpectedEndOfFile)?;
                current += 1;
            }
        }

        Ok(&token)
    }

    pub fn peek_tok(&self) -> ParseResult<&Tok> { self.peek_by(0) }

    pub fn next_is_whitespace(&mut self) -> bool {
        match self.tokens.get(self.tok_pos.val()) {
            Some(tok) => tok.is_whitespace(),
            None => false
        }
    }

    pub fn next_tok(&mut self) -> ParseResult<&Tok> {
        let mut next = &Tok::Space;

        while next.is_whitespace() {
            next = self
                .tokens
                .get(self.tok_pos.val())
                .ok_or(ParseError::UnexpectedEndOfFile)?;

            Self::check_scope(&self.scope, next);
            self.tok_pos.inc();
        }

        #[cfg(feature = "xc-debug")]
        print!("{}", next.to_string());

        Ok(&next)
    }

    pub fn move_on(&mut self, tok: Tok) -> bool {
        if self.peek_tok() == Ok(&tok) {
            self.next_tok().unwrap();
            true
        } else {
            false
        }
    }

    pub fn assert_next_tok_is(&mut self, tok: Tok, name: TokName) -> Result<(), ParseError> {
        let next = self.next_tok()?;

        if next != &tok {
            return Err(ParseError::UnexpectedTok {
                got: next.clone(),
                expected: vec![TokName::from(name)],
            });
        }

        Ok(())
    }

    pub fn recover<O: Errable>(
        &mut self,
        parser: impl FnMut(&mut Self) -> ParseResult<O>,
    ) -> ParseResult<O> {
        self.recover_with(parser, vec![&Tok::Return])
    }

    pub fn recover_with<O: Errable>(
        &mut self,
        mut parser: impl FnMut(&mut Self) -> ParseResult<O>,
        skip_until: Vec<&Tok>,
    ) -> ParseResult<O> {
        let next_non_whitespace_index = self
            .tokens
            .iter()
            .skip(self.tok_pos.val())
            .position(|tok| !tok.is_whitespace())
            .unwrap_or_default()
            + self.tok_pos.val();
        let tok_start = next_non_whitespace_index;
        let char_start = self.spans[next_non_whitespace_index].0;

        let beginning_scope = self.scope.val();

        match parser(self) {
            Ok(expr) => Ok(expr),
            Err(error) => {
                #[cfg(feature = "xc-debug")]
                println!("...recovering...");

                let mut has_increased_once = false;

                // move backwards, in case the "skip until" token was the one that caused the
                // error
                self.tok_pos.dec();

                loop {
                    let next_token = &self
                        .tokens
                        .get(self.tok_pos.val())
                        .ok_or(ParseError::UnexpectedEndOfFile)?;

                    if skip_until.contains(&next_token) && self.scope.val() == beginning_scope {
                        break;
                    }

                    if has_increased_once {
                        Self::check_scope(&self.scope, next_token);
                    }
                    has_increased_once = true;
                    self.tok_pos.inc();
                }

                // helps break out of infinite loops.
                if !has_increased_once {
                    self.tok_pos.inc();
                }

                let tok_end = self.tok_pos.val();
                let char_end = self.spans[tok_end].1 - 1;

                // sometimes, the parser can get stuck with whitespace tokens, causing
                // char_end to be less than char_start. this is a hacky fix, but it keeps
                // an invalid range from indexing into the token list.
                let tok_start = if tok_start > tok_end {
                    tok_end
                } else {
                    tok_start
                };

                let spanned = ParseErrorSpanned {
                    error,
                    start: char_start,
                    end: char_end,
                    tokens_between: TokenSpan::new(&self.tokens, tok_start, tok_end),
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
