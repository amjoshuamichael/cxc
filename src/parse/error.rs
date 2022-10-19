use crate::lex::Tok;

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    IncorrectBeginningOfDeclaration,
    UnexpectedEndOfFile,
    UnexpectedTok { got: Tok, expected: Vec<TokName> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

impl From<Tok> for TokName {
    fn from(tok: Tok) -> Self {
        use Tok::*;
        match tok {
            Comma => TokName::Comma,
            Semicolon => TokName::Semicolon,
            Bang => TokName::Bang,
            Plus => TokName::Plus,
            Minus => TokName::Minus,
            AsterickSet(_) => todo!(),
            Divider => TokName::Divider,
            Modulus => TokName::Modulus,
            BitOR => TokName::BitOR,
            BitXOR => TokName::BitXOR,
            BitShiftR => TokName::BitShiftR,
            BitShiftL => TokName::BitShiftL,
            Dot => TokName::Dot,
            AmpersandSet(_) => todo!(),
            Or => TokName::Or,
            LessOrEqual => TokName::LessOrEqual,
            GreaterOrEqual => TokName::GreaterOrEqual,
            Equal => todo!(),
            Inequal => todo!(),
            Question => TokName::Question,
            Colon => TokName::Colon,
            At => TokName::At,
            LeftParen => TokName::LeftParen,
            RghtParen => TokName::RightParen,
            LeftCurly => TokName::LeftCurly,
            RghtCurly => TokName::RightCurly,
            LeftBrack => TokName::LeftBrack,
            RghtBrack => TokName::RightBrack,
            LeftAngle => TokName::LeftAngle,
            RghtAngle => TokName::RightAngle,
            Assignment => TokName::Assignment,
            VarName(_) => TokName::VarName,
            TypeName(_) => TokName::TypeName,
            Int(_) => TokName::Int,
            Float(_) => TokName::Float,
            Error => TokName::Error,
            Whitespace => TokName::Whitespace,
        }
    }
}

pub fn assert_or_error(check: Tok, against: Tok) -> Result<(), ParseError> {
    if check == against {
        Ok(())
    } else {
        Err(ParseError::UnexpectedTok {
            got: check,
            expected: vec![against.into()],
        })
    }
}
