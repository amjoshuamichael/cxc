pub mod expr_tree;
pub mod hlr_data;
pub mod hlr_data_output;
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

pub fn hlr(info: UniqueFuncInfo, comp_data: Rc<CompData>) -> FuncOutput {
    let code = comp_data.get_code(info.clone()).unwrap();
    let mut output = FuncRep::from(code, comp_data.clone(), info);
    infer_types(&mut output, comp_data);

    if crate::DEBUG {
        dbg!(&output);
    }

    output.output()
}

pub fn hlr_anonymous(
    info: UniqueFuncInfo,
    comp_data: Rc<CompData>,
    code: FuncCode,
) -> FuncOutput {
    let mut output = FuncRep::from(code, comp_data.clone(), info);
    infer_types(&mut output, comp_data);

    if crate::DEBUG {
        dbg!(&output);
    }

    output.output()
}
