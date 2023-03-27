mod add_void_return;
pub mod expr_tree;
mod active_initialization;
mod auto_deref;
mod op_overloading;
mod struct_literals;
mod variant_literals;
pub mod hlr_data;
pub mod hlr_data_output;
mod large_returns;
mod type_inference;

use crate::errors::CResultMany;
use crate::hlr::add_void_return::add_void_return_if_ret_type_is_void;
use crate::parse::*;
use crate::unit::{CompData, UniqueFuncInfo};
use type_inference::infer_types;

pub mod prelude {
    pub use super::{
        expr_tree::ExprID, expr_tree::ExprTree, expr_tree::HNodeData, hlr, hlr_data::FuncRep,
    };
}

use prelude::*;

use self::active_initialization::active_initialization;
use self::auto_deref::auto_deref;
use self::op_overloading::op_overloading;
use self::struct_literals::struct_literals;
use self::variant_literals::variant_literals;
use self::hlr_data_output::FuncOutput;
use self::large_returns::large_returns;

pub fn hlr(
    info: UniqueFuncInfo,
    comp_data: &CompData,
    code: FuncCode,
) -> CResultMany<FuncOutput> {
    if crate::XC_DEBUG {
        println!();
        println!("====HLR of {}====", info.name);
    }

    assert!(matches!(code.code, Expr::Block(_)), "hlr input must be a block");

    let mut output = FuncRep::from_code(code, comp_data, info)?;

    if crate::XC_DEBUG {
        println!("{}", &output.tree.to_string());
    }

    infer_types(&mut output);
    auto_deref(&mut output)?;
    op_overloading(&mut output);
    variant_literals(&mut output);
    active_initialization(&mut output);
    struct_literals(&mut output)?;
    large_returns(&mut output);
    add_void_return_if_ret_type_is_void(&mut output)?;

    if crate::XC_DEBUG {
        println!("{}", &output.tree.to_string());
    }

    Ok(output.output())
}
