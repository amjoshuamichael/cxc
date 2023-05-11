use crate::parse::Opcode;

use super::{hlr_data::FuncRep, expr_tree::{HNodeData, DerefGen}};


pub fn deref_members(hlr: &mut FuncRep) {
    hlr.modify_many_infallible_rev(
        |member_id, member_lit, hlr| {
            if !matches!(member_lit, HNodeData::Member { .. }) {
                return;
            }

            *member_lit.ret_type_mut().unwrap() = member_lit.ret_type().get_ref();

            let parent_op = hlr.tree.parent(member_id);

            match hlr.tree.get(parent_op) {
                HNodeData::Member { .. } => return,
                HNodeData::UnarOp { op, .. } if op == Opcode::Ref => {
                    hlr.tree.replace(
                        parent_op,
                        member_lit.clone(),
                    );
                }
                _ => {
                    let dereffed_member = hlr.insert_quick(
                        parent_op,
                        DerefGen {
                            object: member_lit.clone(),
                        }
                    );

                    *member_lit = hlr.tree.get(dereffed_member);
                }
            }
        }
    );
}
