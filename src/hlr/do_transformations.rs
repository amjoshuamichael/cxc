use std::rc::Rc;

use crate::{
    parse::{InitOpts, Opcode},
    errors::CResultMany, typ::{can_transform::{TransformationStep, TransformationList}, spec_from_type::type_to_type_spec, DestructorType}, VarName, Type, Field, TypeEnum, ArrayType, hlr::expr_tree::GenSlot, FuncQuery, TypeRelation,
};

use super::{expr_tree::{HNodeData, MemberGen, RefGen, DerefGen, StructLitGen, NodeDataGen, TransformationGen, ExprID, SetGenSlot, CastGen, With, UnarOpGen}, hlr_data::FuncRep};

pub fn do_transformations(hlr: &mut FuncRep) -> CResultMany<()> {
    hlr.modify_many_infallible_rev(
        |member_id, member_data, hlr| {
            let HNodeData::Member { object, field, .. } = member_data
                else { return };
            let object_type = hlr.tree.get_ref(*object).ret_type();

            if let Some((steps, _)) = object_type.route_to(field.clone()) {
                if steps == TransformationList::Nil { return }

                *object = hlr.insert_quick(
                    member_id,
                    TransformationGen {
                        object: *object,
                        steps,
                    }
                );
            }
        }
    );

    hlr.modify_many_infallible_rev(desugar_transformation);

    Ok(())
}

pub fn desugar_transformation(
    transform_id: ExprID, 
    transform_data: &mut HNodeData, 
    hlr: &mut FuncRep,
) {
    let HNodeData::Transform { ref mut hs, steps, ret_type, } = transform_data 
        else { return };

    let steps = if let Some(existing_stpes) = steps {
        existing_stpes.clone().to_vec()
    } else {
        let transforms_to = type_to_type_spec(ret_type.clone());
        let from = hlr.tree.get(*hs).ret_type();
        from.can_transform_to(transforms_to).unwrap().steps.to_vec()
    };

    let mut gen: Box::<dyn NodeDataGen> = Box::new(GenSlot);
    let transform_parent = hlr.tree.parent(transform_id);

    for step in steps {
        match step {
            TransformationStep::Ref(count) => {
                if count > 0 {
                    for _ in 0..count {
                        gen = Box::new(RefGen(gen));
                    }
                } else {
                    for _ in 0..(-count) {
                        gen = Box::new(DerefGen(gen));
                    }
                }
            },
            TransformationStep::Field(field) => {
                gen = Box::new(MemberGen {
                    object: gen,
                    field,
                });
            }
            TransformationStep::Fields(fields) => {
                let mut field_nodes = Vec::<(VarName, Box<dyn NodeDataGen>)>::new();
                let mut field_types = Vec::<Field>::new();

                let gen_rc: Rc<dyn NodeDataGen> = Rc::new(gen);

                for field in fields.into_iter() {
                    field_nodes.push((
                        field.0.clone(),
                        Box::new(TransformationGen {
                            object: gen_rc.clone(),
                            steps: field.1.clone(),
                        }),
                    ));
                }

                gen = Box::new(StructLitGen {
                    var_type: Type::unknown(),
                    fields: field_nodes,
                    initialize: InitOpts::NoFill,
                });
            },
            TransformationStep::ArrayToSlice => {
                gen = Box::new(With {
                    generate: gen,
                    then: Box::new(|array_id, typ| {
                        let TypeEnum::Array(ArrayType { base, count }) = typ.as_type_enum()
                            else { unreachable!() };
                        StructLitGen {
                            var_type: Type::unknown(),
                            fields: vec![
                                ("ptr".into(), Box::new(RefGen(array_id))),
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
                    })
                });
            },
            TransformationStep::RemoveDestructor => { 
                gen = Box::new(With {
                    generate: gen,
                    then: Box::new(|object_id, typ| {
                        let TypeEnum::Destructor(DestructorType { base, .. }) =
                            typ.as_type_enum() else { unreachable!() };
                        UnarOpGen {
                            hs: object_id,
                            ret_type: base.clone(),
                            op: Opcode::RemoveTypeWrapper,
                        }
                    }),
                });
            }
        }
    }

    gen = Box::new(SetGenSlot {
        set_to: *hs,
        then: gen,
    });
    hlr.replace_quick(transform_id, gen);
    *transform_data = hlr.tree.get(transform_id);
}
