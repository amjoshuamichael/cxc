use crate::{
    unit::{CompData, FuncDeclInfo, UniqueFuncInfo},
    Type,
};

use super::*;

#[derive(Debug, Clone)]
pub enum Expr {
    Number(u128),
    Float(f64),
    Bool(bool),
    Ident(VarName),
    Struct(TypeAlias, Vec<(VarName, Expr)>),
    Array(Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    MakeVar(VarDecl, Box<Expr>),
    SetVar(Box<Expr>, Box<Expr>),
    Call(VarName, Vec<TypeAlias>, Vec<Expr>, bool),
    UnarOp(Opcode, Box<Expr>),
    BinOp(Opcode, Box<Expr>, Box<Expr>),
    Member(Box<Expr>, VarName),
    IfThen(Box<Expr>, Box<Expr>),
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),
    ForWhile(Box<Expr>, Box<Expr>),
    Block(Vec<Expr>),
    Return(Box<Expr>),

    // These are used during to make parsing easier, but are not outputted after
    // parsing.
    ArgList(Vec<TypeAlias>, Vec<Expr>),
    Op(Opcode),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VarDecl {
    pub name: VarName,
    pub typ: Option<TypeAlias>,
}

#[derive(Debug)]
pub struct Script(pub Vec<Decl>);

impl Script {
    pub fn get_type(&self, find_name: TypeName) -> Option<&TypeDecl> {
        self.0
            .iter()
            .find(|d| match d {
                Decl::Type(TypeDecl { name, .. }) => name == &find_name,
                _ => false,
            })?
            .as_type()
    }

    pub fn types_iter(&self) -> impl Iterator<Item = &TypeDecl> {
        self.0
            .iter()
            .filter(|d| matches!(d, Decl::Type { .. }))
            .map(|d| d.as_type().unwrap())
    }

    pub fn funcs_iter(&self) -> impl Iterator<Item = &FuncCode> {
        self.0
            .iter()
            .filter(|d| matches!(d, Decl::Func { .. }))
            .map(|d| d.as_func().unwrap())
    }
}

#[derive(Debug, Clone)]
pub enum Decl {
    Func(FuncCode),
    Type(TypeDecl),
}

impl Decl {
    pub fn as_func(&self) -> Option<&FuncCode> {
        match self {
            Decl::Func(d) => Some(d),
            _ => None,
        }
    }

    pub fn as_type(&self) -> Option<&TypeDecl> {
        match self {
            Decl::Type(d) => Some(d),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FuncCode {
    pub name: VarName,
    pub ret_type: TypeAlias,
    pub args: Vec<VarDecl>,
    pub generic_count: u32,
    pub code: Expr,
    pub method_of: Option<TypeName>,
    pub dependencies: Vec<FuncDependency>,
}

impl FuncCode {
    pub fn decl_info(&self) -> FuncDeclInfo {
        FuncDeclInfo {
            name: self.name.clone(),
            method_of: self.method_of.clone(),
        }
    }

    pub fn from_expr(code: Expr) -> Self {
        Self {
            name: VarName::temp(),
            ret_type: TypeAlias::Int(0),
            args: Vec::new(),
            generic_count: 0,
            code,
            method_of: None,
            dependencies: Vec::new(),
        }
    }

    pub fn is_generic(&self) -> bool { self.generic_count > 0 }
}

impl Into<FuncDependency> for FuncCode {
    fn into(self) -> FuncDependency {
        FuncDependency {
            name: self.name,
            method_of: self.method_of,
            generics: vec![],
        }
    }
}

impl FuncCode {
    pub fn to_unique_func_info(&self) -> UniqueFuncInfo {
        UniqueFuncInfo {
            name: self.name.clone(),
            method_of: self.method_of.clone(),
            generics: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub name: TypeName,
    pub typ: TypeAlias,
    pub contains_generics: bool,
    pub dependencies: HashSet<TypeName>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FuncDependency {
    pub name: VarName,
    pub method_of: Option<TypeName>,
    pub generics: Vec<TypeAlias>,
}
