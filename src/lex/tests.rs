use super::{lex, Token::*};

macro_rules! lex_check {
    {$($n:expr),+ ; $lexed:ident} => {
        let mut $lexed = $lexed.0.map(|token| token.1);

        $(assert_eq!($lexed.next(), Some($n));)+
        assert!($lexed.next().is_none());
    };
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
        Float(42.9);
        lexed
    };
}

#[test]
fn int_literals() {
    let lexed = lex("4 35 0b101010 0o35 0x35 0b111000 923_734");
    lex_check! {
        Int(4),
        Int(35),
        Int(42),
        Int(29),
        Int(53),
        Int(56),
        Int(923734);
        lexed
    };
}

#[test]
fn float_literals() {
    let lexed = lex("7.8 21.42 0.4_2");
    lex_check! {
        Float(7.8), Float(21.42), Float(0.42); lexed
    };
}

#[test]
fn scientific_notation() {
    let lexed = lex("7.8e+1 21.42e-4 0.4_2e+1");
    lex_check! {
        Float(78.0), Float(0.002142), Float(4.2); lexed
    };
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
