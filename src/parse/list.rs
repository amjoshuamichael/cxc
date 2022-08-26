use super::*;
use crate::lex::Token;

pub fn parse_list<T, U>(
    opener: Token,
    separator: Option<Token>,
    closer: Token,
    parser: U,
    lexer: &mut Lexer,
) -> Vec<T>
where
    U: Fn(&mut Lexer) -> T,
{
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
