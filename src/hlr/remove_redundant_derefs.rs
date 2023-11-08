use crate::parse::Opcode;

use super::{expr_tree::HNodeData, hlr_data::FuncRep};

pub fn remove_redundant_derefs(hlr: &mut FuncRep) {
    hlr.modify_many_infallible(
        |ref_id, hlr| {
            use Opcode::*;

            let HNodeData::UnarOp { op: first_op, hs: first_hs, .. } = 
                hlr.tree.get_ref(ref_id) else { return };
            let HNodeData::UnarOp { op: second_op, hs: second_hs, .. } = 
                &hlr.tree.get(*first_hs) else { return };

            if matches!((*second_op, *first_op), (Deref, Ref) | (Ref, Deref)) {
                let new_parent = hlr.tree.parent(ref_id);
                hlr.replace_quick(ref_id, *second_hs);
            }
        }
    );
}
