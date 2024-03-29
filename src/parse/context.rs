use crate::parse::{Errable, GenericLabels, ParseError, ParseResult, TokName};
use std::{cell::RefCell, rc::Rc};

use crate::lex::{Tok, TypeName, VarName};
use passable::Pass;

use super::ParseErrorSpanned;

#[derive(Default, Debug, Clone)]
pub struct GlobalParseData {
    pub code: Box<str>,
}

#[derive(Default, Debug, Clone)]
pub struct FuncParseData {
    pub name: VarName,
    pub has_return: bool,
}

pub type GlobalParseContext = ParseContext<GlobalParseData>;
pub type FuncParseContext = ParseContext<FuncParseData>;
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
    pub inner_data: N,
    pub generic_labels: GenericLabels,
    pub errors: Pass<Vec<ParseErrorSpanned>>,
}

impl GlobalParseContext {
    pub fn new(tokens: Vec<Tok>, spans: Vec<(usize, usize)>, code: Box<str>) -> Self {
        Self {
            tokens: Rc::new(tokens),
            spans: Rc::new(spans),
            inner_data: GlobalParseData { code },
            ..Default::default()
        }
    }
}

impl<N: Clone> ParseContext<N> {
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
            inner_data: name,
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
            inner_data: self.inner_data.clone(),
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
        {
            use std::io::Write;
            print!("{}", next.to_string());
            // sometimes the parser gets into loops, so to debug this we need to make
            // sure the most recently lexed token is always visible
            std::io::stdout().flush().unwrap();
        }

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
    ) -> O {
        self.recover_with(vec![&Tok::LineFeed], parser)
    }

    pub fn recover_with<O: Errable>(
        &mut self,
        skip_until: Vec<&Tok>,
        mut parser: impl FnMut(&mut Self) -> ParseResult<O>,
    ) -> O {
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
            Ok(expr) => expr,
            Err(error) => {
                #[cfg(feature = "xc-debug")]
                println!("...recovering...");

                let starting_pos = self.tok_pos.val();
                let mut has_increased_once = false;

                // move backwards, in case the "skip until" token was the one that caused the
                // error
                self.tok_pos.dec();

                loop {
                    let Some(next_token) = &self.tokens.get(self.tok_pos.val()) 
                        else { break };

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
                if starting_pos >= self.tok_pos.val() 
                    || self.tokens.len() <= self.tok_pos.val() {
                    // we just want to move forward, we don't care if we hit the end of
                    // the file
                    let _ = self.next_tok();
                }

                // TODO: throw an UnexpectedEndOfFile if we are at the end of the fil

                let tok_end = self.tok_pos.val().min(self.spans.len() - 1);
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
                    at: self.spans[starting_pos.min(self.spans.len() - 1)].0,
                };

                self.errors.deref_mut().unwrap().push(spanned);

                O::err()
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
