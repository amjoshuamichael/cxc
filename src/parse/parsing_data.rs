use super::*;

#[derive(Debug, Clone)]
pub enum Expr {
    Number(u128),
    Float(f64),
    Ident(String),
    Struct(TypeAlias, Vec<(String, Expr)>),
    Array(Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    MakeVar(VarDecl, Box<Expr>),
    SetVar(Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
    UnarOp(Opcode, Box<Expr>),
    BinOp(Opcode, Box<Expr>, Box<Expr>),
    Member(Box<Expr>, String),
    IfThen(Box<Expr>, Box<Expr>),
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),
    ForWhile(Box<Expr>, Box<Expr>),
    Block(Vec<Expr>),
    Return(Box<Expr>),

    // These are used during to make parsing easier, but are not outputted after
    // parsing.
    ArgList(Vec<Expr>),
    Op(Opcode),
}

#[derive(Clone, Debug)]
pub struct VarDecl {
    pub var_name: String,
    pub type_spec: Option<TypeAlias>,
}

#[derive(Debug)]
pub struct Script(pub Vec<Declaration>);

impl Script {
    pub fn get_type(&self, name: String) -> Option<&Declaration> {
        self.0
            .iter()
            .filter(|d| matches!(d, Declaration::Type { .. }))
            .find(|d| d.name() == name)
    }
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Function {
        name: String,
        ret_type: TypeAlias,
        args: Vec<VarDecl>,
        code: Expr,
    },
    Type {
        name: String,
        typ: TypeAlias,
        contains_generics: bool,
        dependencies: HashSet<String>,
    },
}

impl Declaration {
    pub fn name(&self) -> String {
        match self {
            Declaration::Function { name, .. } => name.clone(),
            Declaration::Type { name, .. } => name.clone(),
        }
    }
}
