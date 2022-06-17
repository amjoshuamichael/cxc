mod expr_tree;
pub mod hlr_data;
mod maybe_type;
pub mod program_info;
#[cfg(test)]
mod tests;
pub mod type_group;

use crate::core_lib::CORE_LIB;
use crate::parse::prelude as p;
use std::collections::HashMap;
use std::sync::Arc;

pub mod prelude {
    pub use super::{
        program_info::ProgramInfo, type_group::TypeGroup, Expr, ExprKind, Field,
        GeneralReturnType::*, LiteralKind, Type, HLR,
    };
}

use prelude::*;

#[derive(Default, Clone)]
pub struct HLR {
    pub expressions: Vec<Expr>,
    types: TypeGroup,
    pub identifiers: HashMap<String, VarType>,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum VarType {
    Variable,
    Goto,
}

pub fn hlr(prog: p::Program) -> HLR {
    let mut output = HLR::default();

    match prog {
        p::Program::OneFunc(p_exprs) => {
            for p_expr in p_exprs {
                output.push_expr(p_expr);
            }
        }
    }

    output
}

impl HLR {
    pub fn with_core_lib() -> Self {
        let types = TypeGroup::with_core_lib();

        HLR {
            expressions: Vec::new(),
            types,
            identifiers: HashMap::new(),
        }
    }

    pub fn push_expr(&mut self, expr: p::Expr) {
        let new_expr = self.expr_to_hlr_expr(expr);
        self.expressions.push(new_expr);
    }

    pub fn names(&self, var_type: VarType) -> Vec<String> {
        let mut output = Vec::new();

        for (k, v) in &self.identifiers {
            if *v == var_type {
                output.push(k.clone());
            }
        }

        output
    }

    pub fn expr_to_hlr_expr(&mut self, expr: p::Expr) -> Expr {
        match expr {
            p::Expr::Number(num) => Expr {
                return_type: CORE_LIB.force_get(&"#prim::u32".into()),
                kind: ExprKind::Literal(LiteralKind::Number(num as u64)),
            },
            p::Expr::Ident(name) => match self.identifiers.get(&name) {
                Some(_) => Expr {
                    return_type: CORE_LIB.force_get(&"#prim::u32".into()),
                    kind: ExprKind::Ident(name),
                },
                None => {
                    self.identifiers.insert(name.clone(), VarType::Variable);
                    Expr {
                        return_type: CORE_LIB.force_get(&"#prim::u32".into()),
                        kind: ExprKind::VarDecl(name),
                    }
                }
            },
            p::Expr::BinOp(lhs, op, rhs) => {
                let lhs = Box::new(self.expr_to_hlr_expr(*lhs));
                let rhs = Box::new(self.expr_to_hlr_expr(*rhs));

                Expr {
                    return_type: CORE_LIB.force_get(&"#prim::u32".into()),
                    kind: ExprKind::BinOp(lhs, op, rhs),
                }
            }
            p::Expr::IfThen(i, t) => {
                let i = Box::new(self.expr_to_hlr_expr(*i));
                let t = Box::new(self.expr_to_hlr_expr(*t));

                Expr {
                    return_type: CORE_LIB.force_get(&"#prim::u32".into()),
                    kind: ExprKind::IfThen(i, t),
                }
            }
            p::Expr::IfThenElse(i, t, e) => {
                let i = Box::new(self.expr_to_hlr_expr(*i));
                let t = Box::new(self.expr_to_hlr_expr(*t));
                let e = Box::new(self.expr_to_hlr_expr(*e));

                Expr {
                    return_type: CORE_LIB.force_get(&"#prim::u32".into()),
                    kind: ExprKind::IfThenElse(i, t, e),
                }
            }
            p::Expr::ForWhile(f, d) => {
                let f = Box::new(self.expr_to_hlr_expr(*f));
                let d = Box::new(self.expr_to_hlr_expr(*d));

                match f.gen_ret_type() {
                    PrimInt => Expr {
                        return_type: CORE_LIB.force_get(&"#prim::u32".into()),
                        kind: ExprKind::While(f, d),
                    },
                    _ => todo!(),
                }
            }
            p::Expr::Block(p_exprs) => {
                let mut exprs = Vec::new();

                for p_expr in p_exprs {
                    exprs.push(self.expr_to_hlr_expr(p_expr));
                }

                Expr {
                    return_type: CORE_LIB.force_get(&"#prim::u32".into()),
                    kind: ExprKind::Block(exprs),
                }
            }
            p::Expr::GotoMarker(name) => match self.identifiers.get(&name) {
                Some(_) => Expr {
                    return_type: CORE_LIB.force_get(&"#_::marker".into()),
                    kind: ExprKind::GotoMarker(name),
                },
                None => {
                    self.identifiers.insert(name.clone(), VarType::Goto);

                    Expr {
                        return_type: CORE_LIB.force_get(&"#_::gotomarker".into()),
                        kind: ExprKind::GotoMarker(name),
                    }
                }
            },
            p::Expr::Goto(name) => Expr {
                return_type: CORE_LIB.force_get(&"#_::goto".into()),
                kind: ExprKind::Goto(name),
            },
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Type {
    pub name: String,
    fields: Option<Vec<Field>>, // Primitive types do not have fields
}

impl Type {
    pub fn new_prim(name: &str) -> Self {
        Type {
            name: String::from("#prim::") + name.into(),
            fields: None,
        }
    }

    pub fn new_under(name: &str) -> Self {
        Type {
            name: String::from("#_::") + name.into(),
            fields: None,
        }
    }

    fn unscoped_name<'a>(&'a self) -> &'a str {
        match self.name.rfind(":") {
            Some(char_pos) => &self.name[(char_pos + 1)..self.name.len()],
            None => &*self.name,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Field {
    name: String,
    datatype: Arc<Type>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expr {
    pub return_type: Arc<Type>,
    pub kind: ExprKind,
}

// General categories of return type. Helpful when trying to differentiate between Adding ints or
// floats, or other things like that.
pub enum GeneralReturnType {
    PrimInt,
    PrimFloat,
    Struct,
}

impl Expr {
    pub fn gen_ret_type(&self) -> GeneralReturnType {
        if self.returns_primitive_int() {
            return PrimInt;
        } else if self.returns_primitive_float() {
            return PrimFloat;
        } else {
            todo!()
        }
    }

    fn returns_primitive_int(&self) -> bool {
        match &*self.return_type.name {
            "#prim::u8" | "#prim::u16" | "#prim::u32" | "#prim::u64" => true,
            _ => false,
        }
    }

    fn returns_primitive_float(&self) -> bool {
        todo!();
    }

    fn is_primitive(&self) -> bool {
        &self.return_type.name[0..5] == "#prim"
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind {
    Literal(LiteralKind),
    Ident(String),
    VarDecl(String),
    BinOp(Box<Expr>, p::Opcode, Box<Expr>),
    IfThen(Box<Expr>, Box<Expr>),
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),
    While(Box<Expr>, Box<Expr>),
    Block(Vec<Expr>),
    GotoMarker(String),
    Goto(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiteralKind {
    Number(u64),
    Strin(String),
}

impl LiteralKind {
    pub fn expect_num(&self) -> u64 {
        match self {
            LiteralKind::Number(n) => *n,
            _ => panic!(
                "A literal, {:?}, was unwrapped as a number, even though it isn't a number.",
                self
            ),
        }
    }
}
