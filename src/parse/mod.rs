use crate::indent_parens::indent_parens;
use crate::lex::Lexer;
use std::fmt::{Debug, Formatter};

pub mod prelude {
    pub use super::{parse, Declaration, Expr, Opcode, Script, TypeSpec, VarDecl};
}

#[derive(Debug)]
pub struct Script(pub Vec<Declaration>);

#[derive(Clone, Debug)]
pub struct TypeSpec {
    pub ref_count: u8,
    pub name: String,
}

impl TypeSpec {
    pub fn new(name: &str, ref_count: u8) -> Self {
        Self {
            ref_count,
            name: String::from(name),
        }
    }

    pub fn reference(self) -> Self {
        Self {
            ref_count: self.ref_count + 1,
            name: self.name,
        }
    }
}

#[derive(Clone, Debug)]
pub struct VarDecl {
    pub var_name: String,
    pub type_spec: Option<TypeSpec>,
}

#[derive(Debug)]
pub enum Declaration {
    Function {
        name: VarDecl,
        args: Vec<VarDecl>,
        code: Expr,
    },
    Struct {
        name: String,
        fields: Vec<VarDecl>,
    },
}

#[derive(Clone)]
pub enum Expr {
    Number(u128),
    Float(f64),
    Ident(String),
    SetVar(VarDecl, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
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
            Number(n) => print!("NUM: {n:?}"),
            Float(f) => print!("FLOAT: {f:?}"),
            Ident(i) => print!("IDENT: {i}"),
            SetVar(d, e) => {
                print!("SET: {}: {:?} = ({e:?})", d.var_name, d.type_spec)
            },
            Call(f, a) => print!("CALL: {f:?} WITH {a:?}"),
            UnarOp(op, ref r) => print!("UNOP: ({op:?}, {r:?})"),
            BinOp(ref l, op, ref r) => print!("BINOP: ({l:?} {op:?} {r:?})"),
            IfThen(ref l, ref r) => print!("(IF {l:?} THEN {r:?})"),
            IfThenElse(ref i, ref t, ref e) => {
                print!("(IF {i:?} THEN {t:?} ELSE {e:?})")
            },
            ForWhile(ref f, ref d) => print!("(FOR {f:?} DO {d:?}"),
            Block(statements) => {
                for s in statements {
                    print!("{s:?}");
                }
            },
        };

        Ok(())
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

pub fn parse(input: Lexer) -> Script {
    use crate::serf_parser;

    let parsed = serf_parser::RootParser::new()
        .parse(input)
        .expect("unable to parse: ");

    if crate::DEBUG {
        println!("--------PARSE DATA--------");
        println!("{}", indent_parens(format!("{parsed:?}")));
    }

    parsed
}
