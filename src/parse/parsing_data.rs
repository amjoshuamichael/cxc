use std::hash::Hash;

use super::*;

#[derive(Debug, Clone)]
pub enum Expr {
    Number(u128),
    Float(f64),
    Ident(VarName),
    Struct(TypeAlias, Vec<(VarName, Expr)>),
    Array(Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    MakeVar(VarDecl, Box<Expr>),
    SetVar(Box<Expr>, Box<Expr>),
    Call(VarName, Vec<Expr>, bool),
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
    ArgList(Vec<Expr>),
    Op(Opcode),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VarDecl {
    pub var_name: VarName,
    pub type_spec: Option<TypeAlias>,
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

    pub fn funcs_iter(&self) -> impl Iterator<Item = &FuncDecl> {
        self.0
            .iter()
            .filter(|d| matches!(d, Decl::Func { .. }))
            .map(|d| d.as_func().unwrap())
    }

    pub fn gen_funcs_iter(&self) -> impl Iterator<Item = &GenFuncDecl> {
        self.0
            .iter()
            .filter(|d| matches!(d, Decl::GenFunc { .. }))
            .map(|d| d.as_gen_func().unwrap())
    }
}

#[derive(Debug, Clone)]
pub enum Decl {
    GenFunc(GenFuncDecl),
    Func(FuncDecl),
    Type(TypeDecl),
}

impl Decl {
    pub fn as_func(&self) -> Option<&FuncDecl> {
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

    pub fn as_gen_func(&self) -> Option<&GenFuncDecl> {
        match self {
            Decl::GenFunc(d) => Some(d),
            _ => None,
        }
    }
}

pub enum SomeFuncDecl {
    Gen(GenFuncDecl),
    Func(FuncDecl),
}

impl SomeFuncDecl {
    pub fn args(&self) -> &Vec<VarDecl> {
        match self {
            SomeFuncDecl::Gen(GenFuncDecl { args, .. }) => args,
            SomeFuncDecl::Func(FuncDecl { args, .. }) => args,
        }
    }

    pub fn args_mut(&mut self) -> &mut Vec<VarDecl> {
        match self {
            SomeFuncDecl::Gen(GenFuncDecl { ref mut args, .. }) => args,
            SomeFuncDecl::Func(FuncDecl { ref mut args, .. }) => args,
        }
    }

    pub fn name(&self) -> &VarName {
        match self {
            SomeFuncDecl::Gen(GenFuncDecl { name, .. }) => name,
            SomeFuncDecl::Func(FuncDecl { name, .. }) => name,
        }
    }
}

impl Into<Decl> for SomeFuncDecl {
    fn into(self) -> Decl {
        match self {
            SomeFuncDecl::Gen(g) => Decl::GenFunc(g),
            SomeFuncDecl::Func(g) => Decl::Func(g),
        }
    }
}

#[derive(Debug, Clone)]
pub struct GenFuncDecl {
    pub name: VarName,
    pub ret_type: TypeAlias,
    pub args: Vec<VarDecl>,
    pub code: Expr,
    pub is_method: bool,
    pub dependencies: Vec<GenFuncDependency>,
}

impl GenFuncDecl {
    pub fn realize(self, types: Vec<TypeAlias>) -> FuncDecl {
        FuncDecl {
            name: self.name,
            ret_type: self.ret_type,
            args: self.args,
            code: self.code,
            is_method: self.is_method,
            dependencies: self.dependencies,
            generics: types,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub name: VarName,
    pub ret_type: TypeAlias,
    pub args: Vec<VarDecl>,
    pub code: Expr,
    pub is_method: bool,
    pub dependencies: Vec<GenFuncDependency>,
    pub generics: Vec<TypeAlias>,
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub name: TypeName,
    pub typ: TypeAlias,
    pub contains_generics: bool,
    pub dependencies: HashSet<TypeName>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenFuncDependency {
    pub name: VarName,
    pub types: Vec<TypeAlias>,
}

impl Hash for GenFuncDependency {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        format!("{self:?}").hash(state);
    }
}
