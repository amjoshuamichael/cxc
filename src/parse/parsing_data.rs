use std::hash::{Hash, Hasher};

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
    Call(String, Vec<Expr>, bool),
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VarDecl {
    pub var_name: String,
    pub type_spec: Option<TypeAlias>,
}

#[derive(Debug)]
pub struct Script(pub Vec<Declaration>);

impl Script {
    pub fn get_type(&self, name: String) -> Option<&TypeDecl> {
        self.0
            .iter()
            .filter(|d| matches!(d, Declaration::Type { .. }))
            .find(|d| d.name() == name)?
            .as_type()
    }

    pub fn types_iter(&self) -> impl Iterator<Item = &TypeDecl> {
        self.0
            .iter()
            .filter(|d| matches!(d, Declaration::Type { .. }))
            .map(|d| d.as_type().unwrap())
    }

    pub fn funcs_iter(&self) -> impl Iterator<Item = &FuncDecl> {
        self.0
            .iter()
            .filter(|d| matches!(d, Declaration::Func { .. }))
            .map(|d| d.as_func().unwrap())
    }

    pub fn remove_generic_funcs(&mut self) {
        self.0.retain(|d| match d {
            Declaration::Func(d) => !d.contains_generics,
            _ => true,
        })
    }
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Func(FuncDecl),
    Type(TypeDecl),
}

impl Declaration {
    pub fn as_func(&self) -> Option<&FuncDecl> {
        match self {
            Declaration::Func(d) => Some(d),
            _ => None,
        }
    }

    pub fn as_type(&self) -> Option<&TypeDecl> {
        match self {
            Declaration::Type(d) => Some(d),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub name: String,
    pub ret_type: TypeAlias,
    pub args: Vec<VarDecl>,
    pub code: Expr,
    pub is_method: bool,
    pub contains_generics: bool,
    pub dependencies: Vec<(String, Vec<TypeAlias>)>,
    pub generics: Vec<TypeAlias>,
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub name: String,
    pub typ: TypeAlias,
    pub contains_generics: bool,
    pub dependencies: HashSet<String>,
}

impl Declaration {
    pub fn name(&self) -> String {
        match self {
            Declaration::Func(FuncDecl { name, .. }) => name.clone(),
            Declaration::Type(TypeDecl { name, .. }) => name.clone(),
        }
    }
}
