use crate::{
    parse::{Opcode, TypeSpec},
    TypeRelation, FuncQuery, errors::CResultMany,
};

use super::{expr_tree::HNodeData, hlr_data::FuncRep};

#[cfg_attr(debug_assertions, inline(never))]
pub fn auto_deref(hlr: &mut FuncRep) -> CResultMany<()> {
    hlr.modify_many(
        |memberlit, member_data, hlr| {
            let HNodeData::Member { ref mut object, field, .. } = member_data 
                else { return  Ok(()) };
            let object_type = hlr.tree.get(*object).ret_type();

            let report = hlr.comp_data.get_spec_report(
                &TypeSpec::StructMember(Box::new(object_type.into()), field.clone()), 
                &()
            )?;

            let deref_count = report.deref_count - 1;

            for _ in 0..deref_count {
                let object_type = hlr.tree.get(*object).ret_type();

                *object = hlr.insert_quick(
                    memberlit,
                    HNodeData::UnarOp {
                        ret_type: object_type.get_auto_deref(&hlr.comp_data)?.clone(),
                        op: Opcode::Deref,
                        hs: *object,
                    },
                );
            }

            Ok(())
        },
    ).unwrap();

    hlr.modify_many_infallible(
        |callid, call_data, hlr| {
            let HNodeData::Call { ref mut a, .. } = call_data else { return };
            let unique_func_info = hlr.tree.func_query_of_call(&hlr.tree.get(callid));

            if !unique_func_info.relation.is_method() {
                return 
            }

            let method_of = hlr.tree.get(*a.first().unwrap()).ret_type();
            let deref_chain = method_of.deref_chain(&*hlr.comp_data);

            let deref_level_of_call = deref_chain
                .iter()
                .position(|deref| {
                    let dereffed_func_info = FuncQuery {
                        relation: TypeRelation::MethodOf(deref.clone()),
                        ..unique_func_info.clone()
                    };

                    hlr.comp_data.get_func_type(&dereffed_func_info).is_ok()
                })
                .unwrap_or_else(|| todo!()) as i32 - 1;

            if deref_level_of_call == 0 {
                let first_arg = a.first_mut().unwrap();

                *first_arg = hlr.insert_quick(
                    callid,
                    HNodeData::UnarOp {
                        ret_type: method_of.get_ref(),
                        op: Opcode::Ref,
                        hs: *first_arg,
                    },
                );
            } else {
                for d in 0..deref_level_of_call {
                    let first_arg = a.first_mut().unwrap();

                    *first_arg = hlr.insert_quick(
                        callid,
                        HNodeData::UnarOp {
                            ret_type: deref_chain[(d + 1) as usize].clone(),
                            op: Opcode::Deref,
                            hs: *first_arg,
                        },
                    );
                }
            }
        },
    );

    Ok(())
}
