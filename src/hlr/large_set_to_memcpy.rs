use crate::{Type, parse::Opcode};

use super::{hlr_data::FuncRep, expr_tree::{HNodeData, MemCpyGen, RefGen, HLit}};

pub fn large_set_to_memcpy(hlr: &mut FuncRep) {
    hlr.modify_many_infallible(
        |set_id, hlr| {
            let HNodeData::Set { lhs, rhs } = hlr.tree.get_ref(set_id) else { return };
            
            let lhs_type = hlr.tree.get_ref(*lhs).ret_type();
            let rhs_type = hlr.tree.get_ref(*rhs).ret_type();

            let lhs_size = lhs_type.size();
            let rhs_size = rhs_type.size();

            let min_set_size = lhs_size.min(rhs_size);
            let max_set_size = lhs_size.max(rhs_size);

            if max_set_size <= 8 && 
                rhs_type.primitive_fields_iter().skip(1).next().is_none() && 
                lhs_type.primitive_fields_iter().skip(1).next().is_none() { return; }

            hlr.replace_quick(
                set_id,
                MemCpyGen {
                    from: RefGen(*rhs),
                    to: RefGen(*lhs),
                    size: HNodeData::Lit {
                        lit: HLit::Int(min_set_size as u64), 
                        var_type: Type::u(64),
                    },
                },
            );
        }
    );
}
