use std::{hash::Hash, sync::Arc, rc::Rc};

use crate::{
    unit::{CompData, FuncQuery},
    Type, XcReflect, typ::ABI,
};

use super::*;

#[derive(Hash, Debug, Copy, Clone, Default, PartialEq, Eq)]
pub enum InitOpts {
    Default,
    Uninit,

    #[default]
    NoFill,
}

#[derive(Hash, Copy, Debug, Clone, PartialEq, Eq)]
pub struct ParsedFloat {
    pub l: u128,
    pub r: u128,
    pub exp: Option<i128>,
}

impl std::fmt::Display for ParsedFloat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParsedFloat { l, r, exp: None } => write!(f, "{l}.{r}"),
            ParsedFloat { l, r, exp: Some(exp) } => write!(f, "{l}.{r}e{exp}"),
        }
    }
}

impl Into<f64> for ParsedFloat {
    fn into(self) -> f64 {
        // TODO: make this faster by not converting to string 
        self.to_string().parse().unwrap()
    }
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Number(u64),
    Float(ParsedFloat),
    Bool(bool),
    String(Arc<str>),
    Ident(VarName),
    Label(VarName),
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
    Return(Option<Box<Expr>>),
    Enclosed(Box<Expr>),
    With(Box<Expr>),
    WithAs(Box<Expr>, VarName),
    For(Box<Expr>, Option<VarName>, Box<Expr>),

    Error,
}

impl Default for Expr {
    fn default() -> Self { Self::Block(Vec::new()) }
}

impl Expr {
    pub fn get_ref(&self) -> Expr { Expr::UnarOp(Opcode::Ref, Box::new(self.clone())) }

    /// wraps self in an Rc, and in a block if self is not already a block.
    pub fn wrap(self) -> Arc<Expr> {
        match self {
            Expr::Block(_) => Arc::new(self),
            _ => Arc::new(Expr::Block(vec![Expr::Return(Some(Box::new(self)))])),
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

#[derive(Hash, Clone, Debug, PartialEq, Eq)]
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
    fn spec_code() -> String {
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
    pub code: Arc<Expr>,
    pub relation: TypeSpecRelation,
    pub is_external: bool,
    pub abi: ABI,
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

    pub fn from_expr(code: Arc<Expr>) -> Self {
        Self {
            name: VarName::None,
            ret_type: TypeSpec::Unknown,
            args: Vec::new(),
            generic_count: 0,
            code,
            relation: TypeSpecRelation::Unrelated,
            is_external: false,
            abi: ABI::C,
        }
    }

    pub fn has_generics(&self) -> bool { self.generic_count > 0 }

    pub fn empty() -> FuncCode {
        FuncCode {
            name: VarName::None,
            ret_type: TypeSpec::Void,
            args: Vec::new(),
            generic_count: 0,
            code: Expr::empty_block().into(),
            relation: TypeSpecRelation::Unrelated,
            is_external: false,
            abi: ABI::C,
        }
    }
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
