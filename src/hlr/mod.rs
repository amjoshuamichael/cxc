pub mod expr_tree;
pub mod hlr_data;
mod maybe_type;
pub mod program_info;
#[cfg(test)]
mod tests;
pub mod type_group;
mod type_inference;
mod type_precedence;

use crate::parse::prelude as p;
use num_bigint::BigInt;
use std::collections::HashSet;
use std::sync::Arc;

pub mod prelude {
    pub use super::{
        expr_tree::ExprID, expr_tree::GeneralReturnType::*, expr_tree::NodeData, hlr, hlr_data::HLR,
        program_info::ProgramInfo, type_group::TypeGroup, type_inference::Constraint, type_inference::Constraints,
        type_precedence::get_precedence, Field, Impl, Type,
    };
}

use prelude::*;
use type_inference::do_type_inference;

pub fn hlr(prog: p::Program) -> HLR {
    match prog {
        p::Program::OneFunc(p_exprs) => {
            let func_as_expr = p::Expr::Block(p_exprs);

            let mut output = HLR::from(func_as_expr);

            output
        }
    }
}

#[derive(Clone, Debug)]
pub struct Type {
    pub name: String,
    impls: Impls,
}

#[derive(Debug, Clone, Default)]
pub struct Impls(HashSet<Impl>);

macro_rules! ret_if {
    ($cond:expr) => {
        if $cond {
            return false;
        }
    };
}

impl Impls {
    fn fits_constraints(&self, constraints: &Constraints) -> bool {
        for constraint in constraints.0.iter() {
            match constraint {
                Constraint::FromIntLiteral(n) => {
                    let implem = self
                        .0
                        .iter()
                        .find(|implem| matches!(implem, Impl::FromIntLiteral { .. }));
                    ret_if!(implem.is_none());

                    match implem {
                        Some(Impl::FromIntLiteral { min, max }) => ret_if!(!(min < n && n < max)),
                        _ => unreachable!(),
                    }
                }
                Constraint::AddToIntLiteral => {
                    let implem = self
                        .0
                        .iter()
                        .find(|implem| matches!(implem, Impl::AddToIntLiteral { .. }));
                    ret_if!(implem.is_none());
                }
                Constraint::AddToType(_) => {}
                _ => todo!(),
            }
        }

        true
    }
}

impl Type {
    pub fn new_prim(name: &str) -> Self {
        Type {
            name: String::from("#prim::") + name.into(),
            impls: Impls::default(),
        }
    }

    pub fn new_under(name: &str) -> Self {
        Type {
            name: String::from("#_::") + name.into(),
            impls: Impls::default(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Field {
    name: String,
    datatype: Arc<Type>,
}
