use crate::lex::Tok;

#[derive(Debug)]
pub enum ParseError {
    IncorrectBeginningOfDeclaration,
    UnexpectedEndOfFile,
    UnexpectedTok { got: Tok, expected: Vec<TokName> },
}

#[derive(Debug)]
pub enum TokName {
    Comma,
    Semicolon,
    Bang,

    Plus,
    Minus,
    Multiply,
    Divider,
    Modulus,

    BitOR,
    BitXOR,
    BitAND,
    BitShiftR,
    BitShiftL,

    Ref,
    Deref,

    And,
    Or,

    Dot,

    LessThan,
    GreaterThan,
    LessOrEqual,
    GreaterOrEqual,

    Question,
    Colon,
    At,

    LeftParen,
    RightParen,
    LeftCurly,
    RightCurly,
    LeftBrack,
    RightBrack,
    LeftAngle,
    RightAngle,
    Assignment,

    VarName,
    TypeName,

    Int,
    Float,

    Error,
    Whitespace,

    UnaryOperator,
    BinaryOperator,
}
