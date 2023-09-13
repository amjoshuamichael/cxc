use crate::parse::Opcode;

use super::{expr_tree::HNodeData, hlr_data::FuncRep};

#[cfg_attr(debug_assertions, inline(never))]
pub fn remove_redundant_derefs(hlr: &mut FuncRep) {
    hlr.modify_many_infallible(
        |_, ref_data, hlr| {
            use Opcode::*;

            let HNodeData::UnarOp { op: first_op, hs: first_hs, .. } = 
                &ref_data else { return };
            let HNodeData::UnarOp { op: second_op, hs: second_hs, .. } = 
                &hlr.tree.get(*first_hs) else { return };

            if matches!((*second_op, *first_op), (Deref, Ref) | (Ref, Deref)) {
                *ref_data = hlr.tree.get(*second_hs);
            }
        }
    );
}
