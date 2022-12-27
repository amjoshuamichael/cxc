use std::{
    fmt::Debug,
    hash::Hash,
    iter::{empty, once},
};

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
    Strin(String),
    Ident(VarName),
    StaticMethodPath(TypeSpec, VarName),
    Struct(TypeSpec, Vec<(VarName, Expr)>, bool),
    Array(Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    MakeVar(VarDecl, Box<Expr>),
    SetVar(Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<TypeSpec>, Vec<Expr>, bool),
    UnarOp(Opcode, Box<Expr>),
    BinOp(Opcode, Box<Expr>, Box<Expr>),
    Member(Box<Expr>, VarName),
    IfThen(Box<Expr>, Box<Expr>),
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),
    ForWhile(Box<Expr>, Box<Expr>),
    Block(Vec<Expr>),
    Return(Box<Expr>),
    Enclosed(Box<Expr>),

    // These are used during to make parsing easier, but are not outputted after
    // parsing.
    ArgList(Vec<TypeSpec>, Vec<Expr>),
    Op(Opcode),
}

impl Expr {
    pub fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = &Expr> + 'a> {
        use Expr::*;

        match &self {
            Number(_) | Float(_) | Bool(_) | Strin(_) | Ident(_) | StaticMethodPath(..) => {
                box once(self)
            },
            Struct(_, fields, _) => box fields.iter().map(|(_, expr)| expr),
            Array(exprs) => box exprs.iter(),
            Index(a, i) => box [&**a, &**i].into_iter(),
            MakeVar(_, rhs) => box once(&**rhs),
            SetVar(lhs, rhs) => box [&**lhs, &**rhs].into_iter(),
            Call(f, _, a, _) => box once(&**f).chain(a.iter()),
            UnarOp(_, hs) => box once(&**hs),
            BinOp(_, lhs, rhs) => box [&**lhs, &**rhs].into_iter(),
            Member(o, _) => box once(&**o),
            IfThen(i, t) => box [&**i, &**t].into_iter(),
            IfThenElse(i, t, e) => box [&**i, &**t, &**e].into_iter(),
            ForWhile(f, w) => box [&**f, &**w].into_iter(),
            Block(stmts) => box stmts.iter(),
            Return(r) => box once(&**r),
            ArgList(_, args) => box args.iter(),
            Op(_) => box empty(),
            Enclosed(expr) => box once(&**expr),
        }
    }

    pub fn get_ref(&self) -> Expr { Expr::UnarOp(Opcode::Ref(1), box self.clone()) }

    pub fn wrap_in_block(self) -> Expr {
        match self {
            Expr::Block(_) => self,
            _ => Expr::Block(vec![Expr::Return(box self)]),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VarDecl {
    pub name: VarName,
    pub type_spec: Option<TypeSpec>,
}

impl VarDecl {
    pub fn no_name(typ: Option<TypeSpec>) -> Self {
        VarDecl {
            name: VarName::temp(),
            type_spec: typ,
        }
    }
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

    pub fn decl_count(&self) -> usize { self.0.len() }
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeRelationGeneric<T> {
    Static(T),
    MethodOf(T),
    Unrelated,
}

pub type TypeSpecRelation = TypeRelationGeneric<TypeSpec>;
pub type TypeRelation = TypeRelationGeneric<Type>;

impl<T: Clone> TypeRelationGeneric<T> {
    pub fn map<U>(self, closure: impl FnOnce(T) -> U) -> TypeRelationGeneric<U> {
        match self {
            Self::Static(inner) => TypeRelationGeneric::<U>::Static(closure(inner)),
            Self::MethodOf(inner) => TypeRelationGeneric::<U>::MethodOf(closure(inner)),
            Self::Unrelated => TypeRelationGeneric::<U>::Unrelated,
        }
    }

    pub fn inner(&self) -> Option<T> {
        match self {
            Self::Static(inner) | Self::MethodOf(inner) => Some(inner.clone()),
            Self::Unrelated => None,
        }
    }
}

impl<T> Default for TypeRelationGeneric<T> {
    fn default() -> Self { Self::Unrelated }
}

#[derive(Debug, Clone)]
pub struct FuncCode {
    pub name: VarName,
    pub ret_type: TypeSpec,
    pub args: Vec<VarDecl>,
    pub generic_count: u32,
    pub code: Expr,
    pub relation: TypeSpecRelation,
}

impl FuncCode {
    pub fn decl_info(&self) -> FuncDeclInfo {
        FuncDeclInfo {
            name: self.name.clone(),
            relation: self.relation.clone(),
        }
    }

    pub fn to_unique_func_info(&self, comp_data: &CompData) -> UniqueFuncInfo {
        let relation = self
            .relation
            .clone()
            .map(|spec| comp_data.get_spec(&spec, &Vec::new()).unwrap());

        UniqueFuncInfo {
            name: self.name.clone(),
            relation,
            generics: Vec::new(),
        }
    }

    pub fn from_expr(code: Expr) -> Self {
        Self {
            name: VarName::temp(),
            ret_type: Type::void().into(),
            args: Vec::new(),
            generic_count: 0,
            code,
            relation: TypeSpecRelation::Unrelated,
        }
    }

    pub fn has_generics(&self) -> bool { self.generic_count > 0 }
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub name: TypeName,
    pub typ: TypeSpec,
    pub contains_generics: bool,
    pub dependencies: HashSet<TypeName>,
}

pub type GenericLabels = HashMap<TypeName, u8>;

pub fn merge(one: &GenericLabels, another: &GenericLabels) -> GenericLabels {
    one.iter()
        .chain(another.iter())
        .map(|(n, i)| (n.clone(), i.clone()))
        .collect()
}
