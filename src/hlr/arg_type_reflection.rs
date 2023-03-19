use crate::Type;

use super::{expr_tree::NodeData, hlr_data::FuncRep};

pub fn arg_type_reflection(hlr: &mut FuncRep) {
    hlr.modify_many(
        |data| matches!(data, NodeData::Call { .. }),
        |call_id, mut call_data, hlr| {
            let info = hlr.tree.unique_func_info_of_call(call_data);
            let Some(type_mask) = hlr.comp_data.get_reflect_type_masks(&info) else { return };

            let NodeData::Call { ref mut a, .. } = &mut call_data else { panic!() };
            let mut added_so_far = 0;

            for (index, should_reflect) in { type_mask }.drain(..).enumerate() {
                let arg_id = a[index + added_so_far];
                let arg_data = hlr.tree.get(arg_id);
                let arg_type = arg_data.ret_type();

                let reflected_type_id = hlr.tree.insert(
                    call_id,
                    NodeData::Number {
                        lit_type: Type::i(64),
                        value: arg_type.get_deref().unwrap().as_u64(),
                    },
                );

                if should_reflect {
                    a.insert(index + added_so_far, reflected_type_id);
                    added_so_far += 1;
                }
            }
        },
    );
}
