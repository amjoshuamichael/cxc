pub mod expr_tree;
mod handle_active_initialization;
mod handle_arg_type_reflection;
mod handle_struct_literals;
pub mod hlr_data;
pub mod hlr_data_output;
mod large_returns;
pub mod type_group;
mod type_inference;

use std::rc::Rc;

use crate::parse::*;
use crate::unit::{CompData, UniqueFuncInfo};

pub mod prelude {
    pub use super::{
        expr_tree::ExprID, expr_tree::ExprTree, expr_tree::NodeData, hlr,
        hlr_data::FuncRep, type_inference::*,
    };
}

use prelude::*;

use self::handle_active_initialization::handle_active_initialization;
use self::handle_arg_type_reflection::handle_arg_type_reflection;
use self::handle_struct_literals::handle_struct_literals;
use self::hlr_data_output::FuncOutput;
use self::large_returns::handle_large_returns;

pub fn hlr(
    info: UniqueFuncInfo,
    comp_data: Rc<CompData>,
    code: FuncCode,
) -> FuncOutput {
    if crate::DEBUG {
        println!();
        println!("====HLR of {}====", info.name.to_string());
    }

    let code = block_around_expr(code);
    let mut output = FuncRep::from(code, comp_data.clone(), info);

    infer_types(&mut output);
    handle_active_initialization(&mut output);
    handle_arg_type_reflection(&mut output);
    handle_large_returns(&mut output);
    handle_struct_literals(&mut output);

    if crate::DEBUG {
        println!("{}", &output.tree.to_string());
    }

    output.output()
}

fn block_around_expr(mut code: FuncCode) -> FuncCode {
    if !matches!(code.code, Expr::Block(_)) {
        code.code = Expr::Block(vec![Expr::Return(box code.code)])
    }

    code
}
