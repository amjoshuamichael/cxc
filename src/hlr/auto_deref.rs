use crate::{
    parse::{Opcode, TypeSpec, InitOpts},
    errors::CResultMany, typ::{could_come_from::TransformationStep, spec_from_type::type_to_type_spec}, VarName, Type, Field, TypeEnum, ArrayType,
};

use super::{expr_tree::{HNodeData, MemberGen, RefGen, DerefGen, StructLitGen, NodeDataGen, get_ref}, hlr_data::FuncRep};

#[cfg_attr(debug_assertions, inline(never))]
pub fn auto_deref(hlr: &mut FuncRep) -> CResultMany<()> {
    hlr.modify_many(
        |memberlit, member_data, hlr| {
            let HNodeData::Member { ref mut object, field, .. } = member_data 
                else { return  Ok(()) };
            let object_type = hlr.tree.get(*object).ret_type();

            let report = hlr.comp_data.get_spec_report(
                &TypeSpec::StructMember(
                    Box::new(type_to_type_spec(object_type)),
                    field.clone(),
                ), 
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
                    },
                    TransformationStep::Fields(fields) => {
                        let struct_type = hlr.tree.get_ref(*hs).ret_type();
                        let TypeEnum::Struct(struct_type) = struct_type.as_type_enum() 
                            else { unreachable!() };

                        let mut field_nodes = Vec::<(VarName, Box<dyn NodeDataGen>)>::new();
                        let mut field_types = Vec::<Field>::new();

                        for field in fields.into_iter() {
                            field_nodes.push((
                                field.clone(),
                                Box::new(MemberGen {
                                    object: *hs,
                                    field: field.clone(),
                                }),
                            ));

                            field_types.push(Field {
                                inherited: false,
                                typ: struct_type.get_field_type(field).unwrap(),
                                name: field.clone(),
                            });
                        }

                        *hs = hlr.insert_quick(
                            transform_parent,
                            StructLitGen {
                                var_type: Type::new_struct(field_types),
                                fields: field_nodes,
                                initialize: InitOpts::NoFill,
                            }
                        );

                    },
                    TransformationStep::ArrayToSlice => {
                        let array_type = hlr.tree.get_ref(*hs).ret_type();
                        let TypeEnum::Array(ArrayType { base, count }) = 
                            array_type.as_type_enum() else { unreachable!() };

                        *hs = hlr.insert_quick(
                            transform_parent,
                            StructLitGen {
                                var_type: Type::new_struct(vec![
                                    Field { inherited: true, name: "ptr".into(), typ: base.get_ref() },
                                    Field { inherited: true, name: "len".into(), typ: Type::u(64) },
                                ]),
                                fields: vec![
                                    ("ptr".into(), get_ref(hlr.tree.get(*hs))),
                                    (
                                        "len".into(), 
                                        Box::new(HNodeData::Number { 
                                            lit_type: Type::u(64),
                                            value: *count as u64,
                                        })
                                    ),
                                ],
                                initialize: InitOpts::NoFill,
                            }
                        );
                    },
                }
            }

            *transform_data = hlr.tree.get(*hs);
        },
    );

    Ok(())
}
