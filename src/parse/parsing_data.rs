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
    Tuple(TypeSpec, Vec<Expr>, bool),
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
            Tuple(_, exprs, _) => box exprs.iter(),
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

    // returns the token that starts this expression. for example, if the expression
    // is IfThen, then this function outputs TokName::Question, because that's
    // the token that starts an if. this is used for error reporting.
    pub fn first_tok(&self) -> Tok {
        use Expr::*;

        match self {
            Number(num) => Tok::Int(*num),
            Float(float) => Tok::Float(*float),
            Bool(b) => Tok::Bool(*b),
            Strin(s) => Tok::Strin(s.clone()),
            Ident(name) => Tok::VarName(name.clone()),
            StaticMethodPath(..) => todo!(),
            Struct(..) => Tok::LCurly,
            Tuple(..) => Tok::LCurly,
            Array(..) => Tok::LBrack,
            Index(left, _) => left.first_tok(),
            MakeVar(var_decl, _) => Tok::VarName(var_decl.name.clone()),
            SetVar(left, _) => left.first_tok(),
            Call(left, ..) => left.first_tok(),
            UnarOp(..) => todo!(),
            BinOp(_, left, _) => left.first_tok(),
            Member(left, _) => left.first_tok(),
            IfThen(..) => Tok::Question,
            IfThenElse(..) => Tok::Question,
            ForWhile(..) => Tok::At,
            Block(..) => Tok::LCurly,
            Return(..) => Tok::Semicolon,
            ArgList(..) => todo!(),
            Op(..) => todo!(),
            Enclosed(..) => Tok::LParen,
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
    pub fn types_iter(&self) -> impl Iterator<Item = &TypeDecl> {
        self.0.iter().filter_map(|decl| decl.as_type())
    }

    pub fn funcs_iter(&self) -> impl Iterator<Item = &FuncCode> {
        self.0.iter().filter_map(|decl| decl.as_func())
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

    pub fn inner_type(&self) -> Option<T> {
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
            own_generics: Vec::new(),
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
