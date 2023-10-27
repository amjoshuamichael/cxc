use std::rc::Rc;

use crate::{
    parse::InitOpts,
    errors::CResultMany, typ::{can_transform::{TransformationStep, TransformationList}, spec_from_type::type_to_type_spec, DestructorType}, VarName, Type, Field, TypeEnum, ArrayType, hlr::expr_tree::GenSlot,
};

use super::{expr_tree::{HNodeData, MemberGen, RefGen, DerefGen, StructLitGen, NodeDataGen, TransformationGen, ExprID, SetGenSlot, CastGen}, hlr_data::FuncRep};

#[cfg_attr(debug_assertions, inline(never))]
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
                        ret_type: Type::unknown(),
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
    let mut typ: Type = hlr.tree.get_ref(*hs).ret_type();
    let transform_parent = hlr.tree.parent(transform_id);

    for step in steps {
        match step {
            TransformationStep::Ref(count) => {
                if count > 0 {
                    for _ in 0..count {
                        gen = Box::new(RefGen(gen));
                        typ = typ.get_ref();
                    }
                } else {
                    for _ in 0..(-count) {
                        gen = Box::new(DerefGen(gen));
                        typ = typ.get_deref().unwrap();
                    }
                }
            },
            TransformationStep::Field(field) => {
                let TypeEnum::Struct(struct_type) = typ.as_type_enum()
                    else { unreachable!() };
                typ = struct_type.get_field_type(&field).unwrap();
                gen = Box::new(MemberGen {
                    object: gen,
                    field,
                });
            }
            TransformationStep::Fields(fields) => {
                let struct_type = hlr.tree.get_ref(*hs).ret_type();
                let TypeEnum::Struct(struct_type) = struct_type.as_type_enum() 
                    else { unreachable!() };

                let mut field_nodes = Vec::<(VarName, Box<dyn NodeDataGen>)>::new();
                let mut field_types = Vec::<Field>::new();

                let gen_rc: Rc<dyn NodeDataGen> = Rc::new(gen);

                for field in fields.into_iter() {
                    let field_type = struct_type.get_field_type(&field.0).unwrap();

                    field_nodes.push((
                        field.0.clone(),
                        Box::new(TransformationGen {
                            object: MemberGen {
                                object: gen_rc.clone(),
                                field: field.0.clone(),
                            },
                            steps: field.1.steps.clone(),
                            ret_type: field_type.clone(),
                        }),
                    ));

                    field_types.push(Field {
                        inherited: false,
                        typ: field_type,
                        name: field.0.clone(),
                    });
                }

                typ = Type::new_struct(field_types);
                gen = Box::new(StructLitGen {
                    var_type: typ.clone(),
                    fields: field_nodes,
                    initialize: InitOpts::NoFill,
                });
            },
            TransformationStep::ArrayToSlice => {
                let TypeEnum::Array(ArrayType { base, count }) = 
                    typ.as_type_enum() else { unreachable!() };
                let count = *count;

                typ = Type::new_struct(vec![
                    Field { inherited: true, name: "ptr".into(), typ: base.get_ref() },
                    Field { inherited: true, name: "len".into(), typ: Type::u(64) },
                ]);
                gen = Box::new(StructLitGen {
                    var_type: typ.clone(),
                    fields: vec![
                        ("ptr".into(), Box::new(RefGen(gen))),
                        (
                            "len".into(), 
                            Box::new(HNodeData::Number { 
                                lit_type: Type::u(64),
                                value: count as u64,
                            })
                        ),
                    ],
                    initialize: InitOpts::NoFill,
                })
            },
            TransformationStep::RemoveDestructor => { 
                let TypeEnum::Destructor(DestructorType { base, .. }) = typ.as_type_enum()
                    else { unreachable!() };
                gen = Box::new(CastGen {
                    cast: gen,
                    to: base.clone(),
                });
                typ = base.clone();
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
