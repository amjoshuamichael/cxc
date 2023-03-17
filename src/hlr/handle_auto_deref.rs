use crate::{
    parse::{Opcode, TypeSpec},
    TypeRelation, UniqueFuncInfo,
};

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

    hlr.modify_many(
        |data| matches!(data, NodeData::Call { relation, .. } if relation.is_method()),
        |callid, call_data, hlr| {
            let unique_func_info = hlr.tree.unique_func_info_of_call(&call_data);
            let NodeData::Call { ref mut a, .. } = call_data else { unreachable!() };

            let method_of = hlr.tree.get(*a.first().unwrap()).ret_type();
            let deref_chain = method_of.deref_chain(&*hlr.comp_data);

            let deref_level_of_call = deref_chain
                .iter()
                .position(|deref| {
                    let dereffed_func_info = UniqueFuncInfo {
                        relation: TypeRelation::MethodOf(deref.clone()),
                        ..unique_func_info.clone()
                    };

                    hlr.comp_data.get_func_type(&dereffed_func_info).is_ok()
                })
                .unwrap();

            for d in 0..deref_level_of_call {
                let last_arg = a.last_mut().unwrap();

                *last_arg = hlr.insert_quick(
                    callid,
                    NodeData::UnarOp {
                        ret_type: deref_chain[d + 1].clone(),
                        op: Opcode::Deref,
                        hs: *last_arg,
                    },
                );
            }
        },
    );
}
