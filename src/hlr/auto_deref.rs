use crate::{
    parse::{Opcode, TypeSpec},
    errors::CResultMany, typ::could_come_from::TransformationStep,
};

use super::{expr_tree::{HNodeData, MemberGen, RefGen, DerefGen}, hlr_data::FuncRep};

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
        |transform_id, transform_data, hlr| {
            let HNodeData::Transform { 
                ref mut hs, 
                steps,
                ..
            } = transform_data else { return };

            let steps = steps.clone().to_vec();
            let transform_parent = hlr.tree.parent(transform_id);

            for step in steps {
                match step {
                    TransformationStep::Ref(count) => {
                        if count > 0 {
                            for _ in 0..count {
                                *hs = hlr.insert_quick(
                                    transform_parent,
                                    RefGen { object: *hs },
                                );
                            }
                        } else {
                            for _ in 0..(-count) {
                                *hs = hlr.insert_quick(
                                    transform_parent,
                                    DerefGen { object: *hs },
                                );
                            }
                        }
                    },
                    TransformationStep::Field(field) => {
                        *hs = hlr.insert_quick(
                            transform_parent,
                            MemberGen {
                                object: *hs,
                                field,
                            },
                        );
                    }
                }
            }

            *transform_data = hlr.tree.get(*hs);
        },
    );

    Ok(())
}
