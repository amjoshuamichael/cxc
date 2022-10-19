pub mod expr_tree;
pub mod hlr_data;
pub mod type_group;
mod type_inference;

use crate::parse::*;
use crate::unit::Functions;

pub mod prelude {
    pub use super::{
        expr_tree::ExprID, expr_tree::ExprTree, expr_tree::NodeData, hlr,
        hlr_data::FuncRep, type_group::TypeGroup, type_inference::*,
    };
}

use prelude::*;

pub fn hlr(
    args: Vec<VarDecl>,
    code: Expr,
    globals: &Functions,
    types: &TypeGroup,
    generics: Vec<TypeAlias>,
) -> FuncRep {
    let mut output = FuncRep::from(args, code, types, generics);
    infer_types(&mut output, globals);

    if crate::DEBUG {
        dbg!(&output);
    }

    output
}
