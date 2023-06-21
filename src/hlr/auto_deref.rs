use crate::{
    parse::{Opcode, TypeSpec},
    TypeRelation, UniqueFuncInfo, errors::CResultMany,
};

use super::{expr_tree::HNodeData, hlr_data::FuncRep};

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

            for _ in 0..report.deref_count {
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
            let unique_func_info = hlr.tree.unique_func_info_of_call(&hlr.tree.get(callid));

            if !unique_func_info.relation.is_method() {
                return 
            }

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
                .unwrap_or_else(|| todo!());

            for d in 0..deref_level_of_call {
                let last_arg = a.first_mut().unwrap();

                *last_arg = hlr.insert_quick(
                    callid,
                    HNodeData::UnarOp {
                        ret_type: deref_chain[d + 1].clone(),
                        op: Opcode::Deref,
                        hs: *last_arg,
                    },
                );
            }
        },
    );

    Ok(())
}
