use crate::Type;

use super::{expr_tree::NodeData, hlr_data::FuncRep};

pub fn add_void_return_if_ret_type_is_void(hlr: &mut FuncRep) {
    if hlr.ret_type.is_void() {
        let new_return = hlr.tree.insert(
            hlr.tree.root,
            NodeData::Return {
                to_return: None,
                ret_type: Type::void(),
            },
        );
        let main_block = hlr.tree.get_mut(hlr.tree.root);

        match main_block {
            NodeData::Block { ref mut stmts, .. } => {
                stmts.push(new_return);
            },
            _ => unreachable!(),
        }
    }
}
