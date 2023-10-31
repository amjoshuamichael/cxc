use crate::{Type, parse::Opcode};

use super::{hlr_data::FuncRep, expr_tree::{HNodeData, MemCpyGen, RefGen}};

#[cfg_attr(debug_assertions, inline(never))]
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
                rhs_type.primitive_fields_iter().skip(1).next().is_none() && 
                lhs_type.primitive_fields_iter().skip(1).next().is_none() { return; }

            hlr.tree.replace(set_id, HNodeData::zero());

            let new_data = hlr.insert_quick(
                hlr.tree.parent(set_id),
                MemCpyGen {
                    from: RefGen(*rhs),
                    to: RefGen(*lhs),
                    size: HNodeData::Number {
                        lit_type: Type::i(64),
                        value: min_set_size as u64,
                    },
                },
            );

            *data = hlr.tree.get(new_data);
        }
    );

    //hlr.modify_many_infallible(
    //    move |unar_id, data, hlr| {
    //        let HNodeData::UnarOp { op: Opcode::Deref, hs, .. } = hlr.tree.get(unar_id)
    //            else { return };
    //        let ret_type = hlr.tree.get_ref(unar_id).ret_type().clone();

    //        let return_var = hlr.add_variable(&ret_type);

    //        hlr.insert_statement_before(
    //            unar_id,
    //            MemCpyGen {
    //                from: hs,
    //                to: RefGen(return_var),
    //                size: HNodeData::Number {
    //                    lit_type: Type::u(64),
    //                    value: ret_type.size() as u64,
    //                }
    //            }
    //        );

    //        hlr.replace_quick(unar_id, return_var);
    //    }
    //);
}
