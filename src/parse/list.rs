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

    if lexer.peek() == Some(closer.clone()) {
        lexer.next();
        return Vec::new();
    }

    let mut list = vec![parser(lexer)];

    if let Some(separator) = separator {
        loop {
            match lexer.next() {
                Some(s) if s == separator => {
                    if lexer.peek() == Some(closer.clone()) {
                        // trailing separators
                        lexer.next();
                        break;
                    }

                    list.push(parser(lexer))
                },
                Some(s) if s == closer => break,
                _ => panic!(),
            }
        }
    } else {
        loop {
            match lexer.peek() {
                Some(s) if s == closer => {
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
