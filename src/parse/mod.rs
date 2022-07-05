use crate::lex::SerfLex;
use std::fmt::{Debug, Formatter};

pub mod prelude {
    pub use super::{parse, Declaration, Expr, Opcode, Script};
}

#[derive(Debug)]
pub struct Script(pub Vec<Declaration>);

#[derive(Debug)]
pub enum Declaration {
    Function {
        name: (String, Option<(u8, String)>),
        args: Vec<(String, Option<(u8, String)>)>,
        code: Expr,
    },
}

#[derive(Clone)]
pub enum Expr {
    Number(u128),
    Float(f64),
    Ident(String),
    VarDecl(Option<(u8, String)>, String, Box<Expr>),
    UnarOp(Opcode, Box<Expr>),
    BinOp(Box<Expr>, Opcode, Box<Expr>),
    IfThen(Box<Expr>, Box<Expr>),
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),
    ForWhile(Box<Expr>, Box<Expr>),
    Block(Vec<Expr>),
}

impl Debug for Expr {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        use self::Expr::*;

        match self {
            Number(n) => write!(fmt, "NUM: {n:?}"),
            Float(f) => write!(fmt, "FLOAT: {f:?}"),
            Ident(i) => write!(fmt, "IDENT: {i}"),
            VarDecl(t, n, e) => write!(fmt, "DECLR: {n}: {t:?} = ({e:?})"),
            UnarOp(op, ref r) => write!(fmt, "UNOP: ({op:?}, {r:?})"),
            BinOp(ref l, op, ref r) => write!(fmt, "BINOP: ({l:?} {op:?} {r:?})"),
            IfThen(ref l, ref r) => write!(fmt, "(IF {l:?} THEN {r:?})"),
            IfThenElse(ref i, ref t, ref e) => write!(fmt, "(IF {i:?} THEN {t:?} ELSE {e:?})"),
            ForWhile(ref f, ref d) => write!(fmt, "(FOR {f:?} DO {d:?}"),
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
    Modulus,
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
    Ref,
    Deref,
}

pub fn parse(input: SerfLex) -> Script {
    use crate::serf_parser;

    let parsed = serf_parser::RootParser::new().parse(input).expect("unable to parse: ");

    if crate::DEBUG {
        let parse_data = crate::indent_parens::indent_parens(format!("{parsed:?}"));
        println!("--------PARSE DATA--------");
        println!("{parse_data}");
    }

    parsed
}

#[cfg(test)]
mod tests {
    use crate::{lex::*, parse::prelude::*};

    #[allow(dead_code)]
    fn space_separated<'a>(input: Script) -> Vec<String> {
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
            Script::Function(vec![Expr::BinOp(
                Box::new(Expr::Ident("x".into())),
                Opcode::Plus,
                Box::new(Expr::Number(42)),
            )]),
            parsed
        );
    }
}
