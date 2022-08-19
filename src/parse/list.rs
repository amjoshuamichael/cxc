use super::*;
use crate::lex::Token;

pub fn parse_list<T, U, I>(
    opener: Token,
    separator: Option<Token>,
    closer: Token,
    parser: U,
    lexer: &mut Peekable<I>,
) -> Vec<T>
where
    U: Fn(&mut Peekable<I>) -> T,
    I: Iterator<Item = Token>,
{
    assert_eq!(lexer.next(), Some(opener));

    if lexer.peek() == Some(&closer) {
        lexer.next();
        return Vec::new();
    }

    let mut list = vec![parser(lexer)];

    if let Some(separator) = separator {
        loop {
            match lexer.next() {
                Some(s) if s == separator => {
                    if lexer.peek() == Some(&closer) {
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
                Some(s) if *s == closer => {
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
