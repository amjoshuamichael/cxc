use crate::Type;

use super::{hlr_data::FuncRep, expr_tree::{HNodeData, get_ref, MemCpyGen}};

pub fn large_set_to_memcpy(hlr: &mut FuncRep) {
    hlr.modify_many_infallible(
        |set_id, data, hlr| {
            let HNodeData::Set { lhs, rhs } = data else { return };
            
            let lhs_type = hlr.tree.get_ref(*lhs).ret_type();
            let rhs_type = hlr.tree.get_ref(*rhs).ret_type();

            let lhs_size = lhs_type.size();
            let rhs_size = rhs_type.size();

            let min_set_size = lhs_size.min(rhs_size);
            let max_set_size = lhs_size.max(rhs_size);

            if max_set_size <= 8 && 
                rhs_type.fields_iter().count() <= 1 && 
                lhs_type.fields_iter().count() <= 1 { return; }

            if let HNodeData::Ident { name, .. } = hlr.tree.get_ref(*rhs) {
                hlr.variables[name].do_not_drop = true;
            }

            let new_data = hlr.insert_quick(
                hlr.tree.parent(set_id),
                MemCpyGen {
                    from: get_ref(*rhs),
                    to: get_ref(*lhs),
                    size: HNodeData::Number {
                        lit_type: Type::i(64),
                        value: min_set_size as u64,
                    },
                },
            );

            *data = hlr.tree.get(new_data);
        }
    )
}
