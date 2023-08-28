mod add_void_return;
mod active_initialization;
mod auto_deref;
mod remove_redundant_derefs;
mod op_overloading;
mod struct_literals;
mod array_literals;
mod large_returns;
mod large_args;
mod type_inference;
mod add_drops;
mod large_set_to_memcpy;
pub mod hlr_data;
pub mod hlr_data_output;
pub mod expr_tree;

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
use self::array_literals::array_literals;
use self::hlr_data_output::FuncOutput;
use self::large_returns::large_returns;
use self::large_set_to_memcpy::large_set_to_memcpy;
use self::large_args::large_args;
use self::add_drops::add_drops;
use self::remove_redundant_derefs::remove_redundant_derefs;


pub fn hlr(
    info: UniqueFuncInfo,
    comp_data: &CompData,
    code: FuncCode,
) -> CResultMany<FuncOutput> {
    #[cfg(feature = "xc-debug")]
    {
        println!();
        println!("====HLR of {}====", info.name);
    }

    assert!(matches!(code.code, Expr::Block(_)), "hlr input must be a block");

    let mut output = FuncRep::from_code(code, comp_data, info)?;

    #[cfg(feature = "xc-debug")]
    println!("{}", &output.tree.to_string());

    // the following hlr passes are nescessary, not just optimization
    //
    // each hlr pass is marked with #[cfg_attr(debug_assertions, inline(never))] so we can 
    // see the individual perf impact of each pass when using a flamegraph

    infer_types(&mut output);
    auto_deref(&mut output)?;
    op_overloading(&mut output);
    active_initialization(&mut output);
    struct_literals(&mut output);
    array_literals(&mut output);
    large_returns(&mut output);
    add_void_return_if_ret_type_is_void(&mut output)?;
    add_drops(&mut output);
    large_args(&mut output);
    large_set_to_memcpy(&mut output);
    remove_redundant_derefs(&mut output);

    #[cfg(feature = "xc-debug")]
    {
        println!("{}", &output.tree.to_string());
        println!("{:?}", &output.variables);
    }
  
    Ok(output.output())
}
