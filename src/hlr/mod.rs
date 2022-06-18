pub mod expr_tree;
pub mod hlr_data;
mod maybe_type;
pub mod program_info;
#[cfg(test)]
mod tests;
pub mod type_group;

use crate::parse::prelude as p;
use std::sync::Arc;

pub mod prelude {
    pub use super::{
        expr_tree::ExprID, expr_tree::GeneralReturnType::*, expr_tree::NodeData, hlr,
        hlr_data::HLR, program_info::ProgramInfo, type_group::TypeGroup, Field, Type,
    };
}

use prelude::*;

pub fn hlr(prog: p::Program) -> HLR {
    match prog {
        p::Program::OneFunc(p_exprs) => {
            let func_as_expr = p::Expr::Block(p_exprs);

            HLR::from(func_as_expr)
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
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Field {
    name: String,
    datatype: Arc<Type>,
}
