use crate::lex::SerfLex;
use std::fmt::{Debug, Formatter};

pub mod prelude {
    pub use super::{parse, Expr, Opcode, Program};
}

#[derive(Debug, PartialEq, Eq)]
pub enum Program {
    OneFunc(Vec<Expr>),
}

#[derive(PartialEq, Eq)]
pub enum Expr {
    Number(u128),
    Ident(String),
    BinOp(Box<Expr>, Opcode, Box<Expr>),
    IfThen(Box<Expr>, Box<Expr>),
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),
    ForWhile(Box<Expr>, Box<Expr>),
    GotoMarker(String),
    Goto(String),
    Block(Vec<Expr>),
}

impl Debug for Expr {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        use self::Expr::*;

        match self {
            Number(n) => write!(fmt, "NUM: {n:?}"),
            Ident(i) => write!(fmt, "IDENT: {i}"),
            BinOp(ref l, op, ref r) => write!(fmt, "BINOP: ({l:?} {op:?} {r:?})"),
            IfThen(ref l, ref r) => write!(fmt, "(IF {l:?} THEN {r:?})"),
            IfThenElse(ref i, ref t, ref e) => write!(fmt, "(IF {i:?} THEN {t:?} ELSE {e:?})"),
            ForWhile(ref f, ref d) => write!(fmt, "(FOR {f:?} DO {d:?}"),
            GotoMarker(g) => write!(fmt, "GOTOMARK {g}"),
            Goto(g) => write!(fmt, "GOTO {g}"),
            Block(statements) => {
                let mut output = String::from("BLOCK {");

                for s in statements {
                    output += &*format!("{:?}", s);
                }

                write!(fmt, "{}}}", output)
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Opcode {
    Exponential,
    Plus,
    Minus,
    Multiplier,
    Divider,
    BitAND,
    BitOR,
    BitXOR,
    BitShiftL,
    BitShiftR,
    Or,
    And,
    LessThan,
    GrtrThan,
    LessOrEqual,
    GreaterOrEqual,
    Equal,
    Inequal,
    TernaryQuestion,
    TernaryColon,
    Assignment,
}

pub fn parse(input: SerfLex) -> Program {
    use crate::serf_parser;

    serf_parser::OneFuncProgramParser::new()
        .parse(input)
        .expect("unable to parse: ")
}

#[cfg(test)]
mod tests {
    use crate::{lex::*, parse::prelude::*};

    #[allow(dead_code)]
    fn space_separated<'a>(input: Program) -> Vec<String> {
        let unseparated = format!("{input:?}");
        unseparated
            .clone()
            .split_whitespace()
            .collect::<Vec<&str>>()
            .iter()
            .map(|str| (*str).into())
            .collect()
    }

    // TODO: write proper tests
    #[test]
    fn basic_parsing() {
        let parsed = parse(lex("{ x + 42 }"));

        println!("{parsed:?}");
        assert_eq!(
            Program::OneFunc(vec![Expr::BinOp(
                Box::new(Expr::Ident("x".into())),
                Opcode::Plus,
                Box::new(Expr::Number(42)),
            )]),
            parsed
        );
    }
}
