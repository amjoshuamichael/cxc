use super::*;
use crate::lex::Tok;

pub fn parse_list<T, U>(
    opener_and_closer: (Tok, Tok), // tuple used to make calls cleaner
    separator: Option<Tok>,
    parser: U,
    lexer: &mut Lexer,
) -> Vec<T>
where
    U: Fn(&mut Lexer) -> T,
{
    let (opener, closer) = opener_and_closer;

    assert_eq!(lexer.next(), Some(opener));

    if lexer.next_if(|t| t == closer).is_some() {
        return Vec::new();
    }

    let mut list = vec![parser(lexer)];

    if let Some(separator) = separator {
        // parse with separator

        loop {
            match lexer.next() {
                Some(s) if s == separator => {
                    if lexer.next_if(|t| t == closer).is_some() {
                        break;
                    }

                    list.push(parser(lexer))
                },
                Some(s) if s == closer => break,
                _ => panic!(),
            }
        }
    } else {
        // parse without separator
        loop {
            match lexer.peek() {
                Some(c) if c == closer => {
                    lexer.next();
                    break;
                },
                Some(s) => list.push(parser(lexer)),
                _ => panic!(),
            }
        }
    }

    list
}
