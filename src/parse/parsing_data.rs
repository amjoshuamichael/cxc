use super::*;

#[derive(Debug, Clone)]
pub enum Expr {
    Number(u128),
    Float(f64),
    Ident(String),
    SetVar(VarDecl, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    UnarOp(Opcode, Box<Expr>),
    BinOp(Opcode, Box<Expr>, Box<Expr>),
    IfThen(Box<Expr>, Box<Expr>),
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),
    ForWhile(Box<Expr>, Box<Expr>),
    Block(Vec<Expr>),
    Op(Opcode),
}

/// The data about a type given by the programmer. (e.g. &u32)
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
pub struct Script(pub Vec<Declaration>);

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
