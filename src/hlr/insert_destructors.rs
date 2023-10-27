use std::collections::HashMap;

use slotmap::SecondaryMap;

use crate::{parse::Opcode, errors::CResultMany, typ::DestructorType, TypeEnum};

use super::{hlr_data::{FuncRep, ArgIndex}, expr_tree::HNodeData, add_implicit_drops, inline::inline};

#[cfg_attr(debug_assertions, inline(never))]
pub fn insert_destructors(hlr: &mut FuncRep) -> CResultMany<()> {
    hlr.modify_many_infallible(
        |destructor_id, destructor_data, hlr| {
            let HNodeData::UnarOp { op: Opcode::Destroy, hs, .. } = destructor_data
                else { return };

            let destructor_type = hlr.tree.get_ref(*hs).ret_type();
            let destructor_paths = add_implicit_drops::destructor_paths(&destructor_type);

            if let TypeEnum::Destructor(DestructorType { destructor, .. }) = 
                destructor_type.as_type_enum() {
                let mut inline_arguments = HashMap::new();
                inline_arguments.insert(ArgIndex::Some(0), *hs);
                *destructor_data = inline(destructor, hlr, inline_arguments);
            } else {
                hlr.tree.remove_node(destructor_id);
            }
        }
    );

    Ok(())
}
