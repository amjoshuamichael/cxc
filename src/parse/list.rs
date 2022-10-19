use super::*;
use crate::lex::Tok;

pub fn parse_list<E, F, T: TokenStream>(
    opener_and_closer: (Tok, Tok), // tuple used to make calls cleaner
    separator: Option<Tok>,
    mut parser: F,
    lexer: &mut T,
) -> Result<Vec<E>, ParseError>
where
    F: FnMut(&mut T) -> Result<E, ParseError>,
{
    let (opener, closer) = opener_and_closer;

    assert_or_error(lexer.next_tok()?, opener)?;

    if lexer.peek_tok()? == closer.clone() {
        lexer.next_tok()?;
        return Ok(Vec::new());
    }

    let mut list = vec![parser(lexer)?];

    if let Some(separator) = separator {
        // parse with separator

        loop {
            match lexer.next_tok()? {
                s if s == separator => {
                    if lexer.peek_tok()? == closer.clone() {
                        lexer.next_tok()?;
                        break;
                    }

                    list.push(parser(lexer)?)
                },
                s if s == closer => break,
                _ => panic!(),
            }
        }
    } else {
        // parse without separator
        loop {
            match lexer.peek_tok()? {
                c if c == closer => {
                    lexer.next_tok()?;
                    break;
                },
                _ => list.push(parser(lexer)?),
            }
        }
    }

    Ok(list)
}
