mod add_void_return;
pub mod expr_tree;
mod handle_active_initialization;
mod handle_arg_type_reflection;
mod handle_struct_literals;
mod handle_variant_literals;
pub mod hlr_data;
pub mod hlr_data_output;
mod large_returns;
pub mod type_group;
mod type_inference;

use std::rc::Rc;

use crate::hlr::add_void_return::add_void_return_if_ret_type_is_void;
use crate::unit::{CompData, UniqueFuncInfo};
use crate::{parse::*, Kind, Type};

pub mod prelude {
    pub use super::{
        expr_tree::ExprID, expr_tree::ExprTree, expr_tree::NodeData, hlr, hlr_data::FuncRep,
        type_inference::*,
    };
}

use prelude::*;

use self::handle_active_initialization::handle_active_initialization;
use self::handle_arg_type_reflection::handle_arg_type_reflection;
use self::handle_struct_literals::handle_struct_literals;
use self::handle_variant_literals::handle_variant_literals;
use self::hlr_data_output::FuncOutput;
use self::large_returns::handle_large_returns;

pub fn hlr(info: UniqueFuncInfo, comp_data: Rc<CompData>, code: FuncCode) -> FuncOutput {
    if crate::DEBUG {
        println!();
        println!("====HLR of {}====", info.name.to_string());
    }

    assert!(matches!(code.code, Expr::Block(_)), "hlr input must be a block");

    let mut output = FuncRep::from_code(code, comp_data.clone(), info);

    let context = inkwell::context::Context::create();
    dbg!(&Type::new_tuple(vec![Type::i(32), Type::i(32).get_ref()]).to_any_type(&context));

    infer_types(&mut output);
    handle_variant_literals(&mut output);
    println!("{}", &output.tree.to_string());
    handle_active_initialization(&mut output);
    handle_arg_type_reflection(&mut output);
    handle_struct_literals(&mut output);
    handle_large_returns(&mut output);
    add_void_return_if_ret_type_is_void(&mut output);

    if crate::DEBUG {
        println!("{}", &output.tree.to_string());
    }

    output.output()
}
