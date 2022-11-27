use crate::{TypeEnum, Type};

use super::{expr_tree::NodeData, hlr_data::FuncRep};

pub fn assign_struct(hlr: &mut FuncRep) {
    for setvar_id in hlr.tree.ids() {
        let data = hlr.tree.get(setvar_id);

        if let NodeData::SetVar { lhs: lhs_id, rhs, .. } = data {
            let lhs_data = hlr.tree.get(lhs_id);

            let lhs_type = lhs_data.ret_type();
            let lhs_type = lhs_type.complete_deref();
            let TypeEnum::Struct(lhs_type) = lhs_type.as_type_enum()
                else { continue };

            let (_, block_id) = hlr.tree.statement_and_block(setvar_id);
            let NodeData::Block { mut stmts, ret_type: block_ret_type } =
                hlr.tree.get(block_id)
                    else { continue };
                    
            let id_in_block = stmts.iter().position(|id| *id == setvar_id).unwrap();

            let NodeData::StructLit { fields: field_stmts, .. } = 
                hlr.tree.get(rhs) else { continue; };

            stmts.remove(id_in_block);

            for (field_index, (field_name, field_value)) 
                in { field_stmts }.drain(..).enumerate().rev() {
                let field_type = lhs_type.get_field_type(&field_name).unwrap();

                let set_space = hlr.tree.make_one_space(block_id);

                if field_index != 0 {
                    let member_space = hlr.tree.make_one_space(set_space);
                    let object = hlr.tree.insert(member_space, lhs_data.clone());
                    hlr.tree.replace(member_space, NodeData::Member {
                        ret_type: field_type,
                        object,
                        field: field_name,
                    });

                    hlr.tree.replace(set_space, NodeData::SetVar { 
                        ret_type: Type::never(), 
                        lhs: member_space, 
                        rhs: field_value 
                    });
                } else {
                    // if we are in the first field, we can set the object directly, because the
                    // first field is always aligned to the first byte of the object.
                    let object_to_set = hlr.tree.insert(set_space, lhs_data.clone());
                    hlr.tree.replace(
                        set_space, 
                        NodeData::SetVar { 
                            ret_type: Type::never(), 
                            lhs: object_to_set, 
                            rhs: field_value 
                        }
                    );
                }

                stmts.insert(id_in_block, set_space);
            }

            hlr.tree.replace(
                block_id, 
                NodeData::Block { stmts, ret_type: block_ret_type }
            );

            hlr.tree.replace(
                setvar_id, 
                lhs_data.clone()
            );
        }
    }
}
