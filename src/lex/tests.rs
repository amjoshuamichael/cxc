use super::{lex, Token::*};

macro_rules! lex_check {
    {$($n:expr),+ ; $lexed:ident} => {
        let mut $lexed = $lexed.0.map(|token| token.1);

        $(assert_eq!($lexed.next(), Some($n));)+
        assert!($lexed.next().is_none());
    };
    ($count:expr => $n:expr; $lexed:ident) => {
        let mut $lexed = $lexed.0.map(|token| token.1);

        for _ in 0..($count) {
            assert_eq!($lexed.next(), Some($n));
        }

        assert!($lexed.next().is_none());
    }
}

#[test]
fn basic_lexing() {
    let lexed = lex(r#"
        an_int = 42
        a_float = 42.9
    "#);

    lex_check! {
        Ident("an_int".into()),
        Assignment,
        Int(42),
        Ident("a_float".into()),
        Assignment,
        Float("42.9".into());
        lexed
    };
}

#[test]
fn number_literals() {
    let lexed = lex("42 0b101010 0o52 0x2a 0b101_0_10 4_2");
    lex_check!(6 => Int(42); lexed);
}

#[test]
fn float_literals() {
    let lexed = lex(".42 0.42 0.4_2");
    lex_check!(3 => Float("0.42".into()); lexed);
}

#[test]
fn stuck_together() {
    let lexed = lex("some_var=4?<<()&&&");
    lex_check! {
        Ident("some_var".into()),
        Assignment,
        Int(4),
        Question,
        BitShiftL,
        LeftParen,
        RghtParen,
        And,
        BitAND;
        lexed
    }
}
