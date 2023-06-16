use crate::Type;

use super::{hlr_data::FuncRep, expr_tree::{HNodeData, get_ref, MemCpyGen}};

pub fn large_set_to_memcpy(hlr: &mut FuncRep) {
    hlr.modify_many_infallible(
        |set_id, data, hlr| {
            let HNodeData::Set { lhs, rhs } = data else { return };
            
            let type_of_set = hlr.tree.get_ref(*lhs).ret_type();

            if type_of_set.size() <= 8 { return; }

            let new_data = hlr.insert_quick(
                hlr.tree.parent(set_id),
                MemCpyGen {
                    from: get_ref(hlr.tree.get(*rhs)),
                    to: get_ref(hlr.tree.get(*lhs)),
                    size: HNodeData::Number {
                        lit_type: Type::i(64),
                        value: type_of_set.size() as u64,
                    },
                },
            );

            *data = hlr.tree.get(new_data);
        }
    )
}
