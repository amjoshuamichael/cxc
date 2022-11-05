pub mod expr_tree;
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

use self::hlr_data_output::FuncOutput;
use self::large_returns::handle_large_returns;

pub fn hlr(info: UniqueFuncInfo, comp_data: Rc<CompData>) -> FuncOutput {
    if crate::DEBUG {
        println!();
        println!("====HLR of {}====", info.name.to_string());
    }

    let code = comp_data.get_code(info.clone()).unwrap();
    let code = block_around_expr(code);
    let mut output = FuncRep::from(code, comp_data.clone(), info);
    infer_types(&mut output);
    handle_large_returns(&mut output);

    if crate::DEBUG {
        println!("{}", &output.tree.to_string());
    }

    output.output()
}

pub fn hlr_anonymous(
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
    handle_large_returns(&mut output);

    if crate::DEBUG {
        println!("{}", &output.tree.to_string());
    }

    output.output()
}

pub fn block_around_expr(mut code: FuncCode) -> FuncCode {
    if !matches!(code.code, Expr::Block(_)) {
        code.code = Expr::Block(vec![Expr::Return(box code.code)])
    }

    code
}
