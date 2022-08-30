use super::*;
use crate::lex::Tok;

pub fn parse_list<El, Fu, Ts: TokenStream>(
    opener_and_closer: (Tok, Tok), // tuple used to make calls cleaner
    separator: Option<Tok>,
    mut parser: Fu,
    lexer: &mut Ts,
) -> Vec<El>
where
    Fu: FnMut(&mut Ts) -> El,
{
    let (opener, closer) = opener_and_closer;

    assert_eq!(lexer.next_tok(), Some(opener));

    if lexer.peek_tok() == Some(closer.clone()) {
        lexer.next_tok();
        return Vec::new();
    }

    let mut list = vec![parser(lexer)];

    if let Some(separator) = separator {
        // parse with separator

        loop {
            match lexer.next_tok() {
                Some(s) if s == separator => {
                    if lexer.peek_tok() == Some(closer.clone()) {
                        lexer.next_tok();
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
            match lexer.peek_tok() {
                Some(c) if c == closer => {
                    lexer.next_tok();
                    break;
                },
                Some(_) => list.push(parser(lexer)),
                _ => panic!(),
            }
        }
    }

    list
}
