use crate::{Type, errors::{UErr, CResult}};

use super::{expr_tree::HNodeData, hlr_data::FuncRep};

pub fn add_void_return_if_ret_type_is_void(hlr: &mut FuncRep) -> CResult<()> {
    if hlr.ret_type.is_void() {
        let new_return = hlr.tree.insert(
            hlr.tree.root,
            HNodeData::Return {
                to_return: None,
                ret_type: Type::void(),
            },
        );
        let main_block = hlr.tree.get_mut(hlr.tree.root);

        match main_block {
            HNodeData::Block { ref mut stmts, .. } => {
                stmts.push(new_return);
            },
            _ => unreachable!(),
        }
    }

    if !hlr.tree.iter().any(|node| matches!(node.1, HNodeData::Return { .. })) {
        Err(UErr::NoReturn)?;
    }

    Ok(())
}
