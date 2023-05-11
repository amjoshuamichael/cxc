use crate::parse::Opcode;

use super::{expr_tree::HNodeData, hlr_data::FuncRep};

pub fn remove_redundant_derefs(hlr: &mut FuncRep) {
    hlr.modify_many_infallible(
        |ref_id, ref_data, hlr| {
            let HNodeData::UnarOp { op, hs, .. } = &ref_data else { return };
            match op {
                Opcode::Deref => {
                    let HNodeData::UnarOp { op, hs, .. } = &hlr.tree.get(*hs) else { return };

                    if *op == Opcode::Ref {
                        *ref_data = hlr.tree.get(*hs);
                    };
                }
                Opcode::Ref => {
                    let HNodeData::UnarOp { op, hs, .. } = &hlr.tree.get(*hs) else { return };

                    if *op == Opcode::Deref {
                        *ref_data = hlr.tree.get(*hs);
                    };
                }
                _ => {}
            }
        }
    );
}
