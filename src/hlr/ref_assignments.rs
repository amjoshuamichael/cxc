use super::{expr_tree::{HNodeData, RefGen}, hlr_data::FuncRep};

pub fn ref_assignments(hlr: &mut FuncRep) {
    hlr.modify_many_infallible(
        |set_id, set_data, hlr| {
            let HNodeData::Set { lhs, .. } = set_data else { return };
            hlr.replace_quick(*lhs, RefGen { object: box hlr.tree.get(*lhs) });
        }
    );
}
