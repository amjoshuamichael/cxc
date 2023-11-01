use super::*;
use crate::lex::Tok;

pub const COMMAS: Option<TokWithName> = Some((Tok::Comma, TokName::Comma));

pub fn parse_list<T: Clone, O: Errable>(
    opener_and_closer: (TokWithName, TokWithName), // tuple used to make calls cleaner
    separator: Option<TokWithName>,
    parser: impl Fn(&mut ParseContext<T>) -> ParseResult<O>,
    lexer: &mut ParseContext<T>,
) -> Result<Vec<O>, ParseError> {
    let ((opener, opener_name), (closer, _)) = opener_and_closer;

    lexer.assert_next_tok_is(opener, opener_name)?;

    if lexer.peek_tok()? == &closer {
        lexer.next_tok()?;
        return Ok(Vec::new());
    }

    let mut list = vec![parser(lexer)?];

    if let Some((separator, separator_name)) = separator {
        // parse with separator

        loop {
            match lexer.next_tok()? {
                s if s == &separator => {
                    if lexer.peek_tok()? == &closer {
                        lexer.next_tok()?;
                        break;
                    }

                    list.push(lexer.recover_with(vec![&separator, &closer], &parser))
                },
                s if s == &closer => break,
                got => {
                    return ParseError::unexpected(got, vec![TokName::from(separator_name)]);
                },
            }
        }
    } else {
        // parse without separator
        loop {
            match lexer.peek_tok()? {
                c if c == &closer => {
                    lexer.next_tok()?;
                    break;
                },
                _ => list.push(lexer.recover(&parser)),
            }
        }
    }

    Ok(list)
}

pub fn parse_one_or_list<T: Default + Clone, O: Errable>(
    opener_and_closer: (TokWithName, TokWithName), // tuple used to make calls cleaner
    separator: Option<TokWithName>,
    parser: impl Fn(&mut ParseContext<T>) -> ParseResult<O>,
    lexer: &mut ParseContext<T>,
) -> Result<Vec<O>, ParseError> {
    if lexer.peek_tok()? == &opener_and_closer.0.0 {
        parse_list(opener_and_closer, separator, parser, lexer)
    } else {
        Ok(vec![parser(lexer)?])
    }
}
