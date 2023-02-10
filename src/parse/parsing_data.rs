use std::{fmt::Debug, hash::Hash, iter::once, sync::Arc};

use crate::{
    unit::{CompData, FuncDeclInfo, UniqueFuncInfo},
    Type,
};

use super::*;

#[derive(Debug, Copy, Clone, Default, PartialEq, Eq)]
pub enum StructFill {
    Default,
    Uninit,

    #[default]
    NoFill,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(u64),
    Float(f64),
    Bool(bool),
    Strin(Arc<str>),
    Ident(VarName),
    TypedValue(TypeSpec, Box<Expr>),
    Struct(Vec<(VarName, Expr)>, StructFill),
    StaticMethodPath(TypeSpec, VarName),
    Tuple(Vec<Expr>, StructFill),
    Array(Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    SetVar(VarDecl, Box<Expr>),
    Set(Box<Expr>, Box<Expr>),
    Call {
        func: Box<Expr>,
        generics: Vec<TypeSpec>,
        args: Vec<Expr>,
        is_method: bool,
    },
    UnarOp(Opcode, Box<Expr>),
    BinOp(Opcode, Box<Expr>, Box<Expr>),
    Member(Box<Expr>, VarName),
    IfThen(Box<Expr>, Box<Expr>),
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),
    ForWhile(Box<Expr>, Box<Expr>),
    Block(Vec<Expr>),
    Return(Box<Expr>),
    Enclosed(Box<Expr>),

    Error,
}

impl Default for Expr {
    fn default() -> Self { Self::Block(Vec::new()) }
}

impl Expr {
    pub fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = &Expr> + 'a> {
        use Expr::*;

        match self {
            Number(_)
            | Float(_)
            | Bool(_)
            | Strin(_)
            | Ident(_)
            | StaticMethodPath(..)
            | Error { .. } => box once(self),
            Struct(fields, _) => box fields.iter().map(|(_, expr)| expr),
            Tuple(exprs, _) => box exprs.iter(),
            Array(exprs) => box exprs.iter(),
            Index(a, i) => box [&**a, &**i].into_iter(),
            SetVar(_, rhs) => box once(&**rhs),
            Set(lhs, rhs) => box [&**lhs, &**rhs].into_iter(),
            Call {
                func: name, args, ..
            } => box once(&**name).chain(args.iter()),
            UnarOp(_, hs) => box once(&**hs),
            BinOp(_, lhs, rhs) => box [&**lhs, &**rhs].into_iter(),
            Member(o, _) => box once(&**o),
            IfThen(i, t) => box [&**i, &**t].into_iter(),
            IfThenElse(i, t, e) => box [&**i, &**t, &**e].into_iter(),
            ForWhile(f, w) => box [&**f, &**w].into_iter(),
            Block(stmts) => box stmts.iter(),
            Return(r) => box once(&**r),
            Enclosed(expr) => box once(&**expr),
            TypedValue(_, expr) => box once(&**expr),
        }
    }

    pub fn get_ref(&self) -> Expr { Expr::UnarOp(Opcode::Ref, box self.clone()) }

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
    pub type_spec: TypeSpec,
}

impl VarDecl {
    pub fn no_name(type_spec: TypeSpec) -> Self {
        VarDecl {
            name: VarName::temp(),
            type_spec,
        }
    }
}

#[derive(Default, Debug)]
pub struct Script(pub Vec<Decl>);

impl Script {
    pub fn types_iter(&self) -> impl Iterator<Item = &TypeDecl> {
        self.0.iter().filter_map(|decl| decl.as_type())
    }

    pub fn funcs_iter(&self) -> impl Iterator<Item = &FuncCode> {
        self.0.iter().filter_map(|decl| decl.as_func())
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum TypeRelationGeneric<T> {
    Static(T),
    MethodOf(T),
    Unrelated,
}

pub type TypeSpecRelation = TypeRelationGeneric<TypeSpec>;
pub type TypeRelation = TypeRelationGeneric<Type>;

impl<T: Clone> TypeRelationGeneric<T> {
    pub fn map_inner_type<U>(self, closure: impl FnOnce(T) -> U) -> TypeRelationGeneric<U> {
        match self {
            Self::Static(inner) => TypeRelationGeneric::<U>::Static(closure(inner)),
            Self::MethodOf(inner) => TypeRelationGeneric::<U>::MethodOf(closure(inner)),
            Self::Unrelated => TypeRelationGeneric::<U>::Unrelated,
        }
    }

    pub fn inner_type(&self) -> Option<T> {
        match self {
            Self::Static(inner) | Self::MethodOf(inner) => Some(inner.clone()),
            Self::Unrelated => None,
        }
    }

    pub fn inner_type_mut(&mut self) -> Option<&mut T> {
        match self {
            Self::Static(ref mut inner) | Self::MethodOf(ref mut inner) => Some(inner),
            Self::Unrelated => None,
        }
    }
}

impl<T> Default for TypeRelationGeneric<T> {
    fn default() -> Self { Self::Unrelated }
}

#[derive(Default, Debug, Clone)]
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
            .map_inner_type(|spec| comp_data.get_spec(&spec, &Vec::new()).unwrap());

        UniqueFuncInfo {
            name: self.name.clone(),
            relation,
            own_generics: Vec::new(),
        }
    }

    pub fn from_expr(code: Expr) -> Self {
        Self {
            name: VarName::temp(),
            ret_type: Type::unknown().into(),
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
}

pub type GenericLabels = HashMap<TypeName, u8>;

pub fn merge(one: &GenericLabels, another: &GenericLabels) -> GenericLabels {
    one.iter()
        .chain(another.iter())
        .map(|(n, i)| (n.clone(), i.clone()))
        .collect()
}
