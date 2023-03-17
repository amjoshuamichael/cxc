use crate::parse::{Opcode, TypeSpec};

use super::{expr_tree::NodeData, hlr_data::FuncRep};

pub fn handle_auto_deref(hlr: &mut FuncRep) {
    hlr.modify_many(
        |data| matches!(data, NodeData::Member{..}),
        |memberlit, member_data, hlr| {
            let NodeData::Member { ref mut object, field, .. } = member_data else { unreachable!() };
            let object_type = hlr.tree.get(*object).ret_type();

            let report = hlr.comp_data.get_spec_report(
                &TypeSpec::StructMember(box object_type.clone().into(), field.clone()), 
                &()
            ).unwrap();

            match report.deref_count {
                1.. => {
                    for _ in 0..report.deref_count {
                        *object = hlr.insert_quick(
                            memberlit,
                            NodeData::UnarOp {
                                ret_type: object_type.get_auto_deref(&hlr.comp_data).unwrap().clone(),
                                op: Opcode::Deref,
                                hs: *object,
                            },
                        );
                    }
                },
                _ => {},
            }
        },
    );
}
