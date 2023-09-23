use std::{fmt::Debug, hash::Hash, iter::{once, empty}, sync::Arc};

use crate::{
    unit::{CompData, FuncQuery},
    Type, XcReflect,
};

use super::*;

#[derive(Debug, Copy, Clone, Default, PartialEq, Eq)]
pub enum InitOpts {
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
    String(Arc<str>),
    Ident(VarName),
    TypedValue(TypeSpec, Box<Expr>),
    Struct(Vec<(VarName, Expr)>, InitOpts),
    StaticMethodPath(TypeSpec, VarName),
    Tuple(Vec<Expr>, InitOpts),
    Array(Vec<Expr>, InitOpts),
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
    While(Box<Expr>, Box<Expr>),
    Block(Vec<Expr>),
    Return(Box<Expr>),
    Enclosed(Box<Expr>),

    Error,
}

impl Default for Expr {
    fn default() -> Self { Self::Block(Vec::new()) }
}

impl<'a> IntoIterator for &'a Expr {
    type Item = &'a Expr;

    type IntoIter = Box<dyn Iterator<Item = &'a Expr> + 'a>;

    fn into_iter(self) -> Self::IntoIter {
        use Expr::*;

        let me = once(self);

        let rest: Box<dyn Iterator<Item = &Expr>> = match self {
            Number(_)
            | Float(_)
            | Bool(_)
            | String(_)
            | Ident(_)
            | StaticMethodPath(..)
            | Error { .. } => Box::new(empty()),
            Struct(fields, _) 
                => Box::new(fields.iter().map(|(_, expr)| expr).flatten()),
            Tuple(exprs, _) => Box::new(exprs.iter().flatten()),
            Array(exprs, _) => Box::new(exprs.iter().flatten()),
            Index(a, i) => Box::new([&**a, &**i].into_iter().flatten()),
            SetVar(_, rhs) => Box::new(once(&**rhs).flatten()),
            Set(lhs, rhs) => Box::new([&**lhs, &**rhs].into_iter().flatten()),
            Call {
                func: name, args, ..
            } => Box::new(once(&**name).chain(args.iter())),
            UnarOp(_, hs) => Box::new(once(&**hs).flatten()),
            BinOp(_, lhs, rhs) => Box::new([&**lhs, &**rhs].into_iter().flatten()),
            Member(o, _) => Box::new(o.into_iter()),
            IfThen(i, t) => Box::new([&**i, &**t].into_iter().flatten()),
            IfThenElse(i, t, e) => Box::new([&**i, &**t, &**e].into_iter().flatten()),
            While(f, w) => Box::new([&**f, &**w].into_iter().flatten()),
            Block(stmts) => Box::new(stmts.iter().flatten()),
            Return(r) => Box::new(once(&**r).flatten()),
            Enclosed(expr) => Box::new(once(&**expr).flatten()),
            TypedValue(_, expr) => Box::new(once(&**expr).flatten()),
        };

        Box::new(me.chain(rest))
    }
}
impl Expr {
    pub fn get_ref(&self) -> Expr { Expr::UnarOp(Opcode::Ref, Box::new(self.clone())) }

    pub fn wrap_in_block(self) -> Expr {
        match self {
            Expr::Block(_) => self,
            _ => Expr::Block(vec![Expr::Return(Box::new(self))]),
        }
    }

    pub fn ident(from: &str) -> Box<Expr> {
        Box::new(Expr::Ident(VarName::from(from)))
    }

    pub fn string(from: &str) -> Box<Expr> {
        Box::new(Expr::String(Arc::from(from)))
    }

    pub fn empty_block() -> Expr {
        Expr::Block(Vec::new())
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
            name: VarName::None,
            type_spec,
        }
    }
}

#[derive(Default, Debug)]
pub struct Script {
    pub types: Vec<TypeDecl>,
    pub funcs: Vec<FuncCode>,
    pub comp_script: Option<FuncCode>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum TypeRelationGeneric<T> {
    Static(T),
    MethodOf(T),
    Unrelated,
}

pub type TypeSpecRelation = TypeRelationGeneric<TypeSpec>;
pub type TypeRelation = TypeRelationGeneric<Type>;

impl XcReflect for TypeRelationGeneric<Type> {
    fn alias_code() -> String {
        "TypeRelation = { Static: { Type } / MethodOf: { Type } / Unrelated: {} }".into()
    }
}

impl<T: Clone> TypeRelationGeneric<T> {
    pub fn map_inner_type<U>(self, closure: impl FnOnce(T) -> U) -> TypeRelationGeneric<U> {
        match self {
            Self::Static(inner) => TypeRelationGeneric::<U>::Static(closure(inner)),
            Self::MethodOf(inner) => TypeRelationGeneric::<U>::MethodOf(closure(inner)),
            Self::Unrelated => TypeRelationGeneric::<U>::Unrelated,
        }
    }

    // TODO: return pointer here
    pub fn inner_type(&self) -> Option<&T> {
        match self {
            Self::Static(inner) | Self::MethodOf(inner) => Some(&inner),
            Self::Unrelated => None,
        }
    }

    pub fn inner_type_mut(&mut self) -> Option<&mut T> {
        match self {
            Self::Static(ref mut inner) | Self::MethodOf(ref mut inner) => Some(inner),
            Self::Unrelated => None,
        }
    }

    pub fn is_method(&self) -> bool { matches!(self, Self::MethodOf(_)) }
}

impl<T> Default for TypeRelationGeneric<T> {
    fn default() -> Self { Self::Unrelated }
}

#[derive(Debug, Clone)]
pub struct FuncCode {
    pub name: VarName,
    pub ret_type: TypeSpec,
    pub args: Vec<VarDecl>,
    pub generic_count: usize,
    pub code: Expr,
    pub relation: TypeSpecRelation,
    pub is_external: bool,
}

impl FuncCode {
    pub fn to_unique_func_info(&self, comp_data: &CompData) -> FuncQuery {
        let relation = self
            .relation
            .clone()
            .map_inner_type(|spec| comp_data.get_spec(&spec, &Vec::new()).unwrap());

        FuncQuery {
            name: self.name.clone(),
            relation,
            generics: Vec::new(),
        }
    }

    pub fn from_expr(code: Expr) -> Self {
        Self {
            name: VarName::None,
            ret_type: TypeSpec::Unknown,
            args: Vec::new(),
            generic_count: 0,
            code,
            relation: TypeSpecRelation::Unrelated,
            is_external: false,
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
        .map(|(n, i)| (n.clone(), *i))
        .collect()
}
