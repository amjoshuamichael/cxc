use crate::{Type, TypeEnum, UniqueFuncInfo, VarName};

use super::{
    expr_tree::{CallGen, NodeData, StructLitGen},
    hlr_data::FuncRep,
};

pub fn variant_literals(hlr: &mut FuncRep) {
    hlr.modify_many_rev(
        |data| matches!(data, NodeData::StructLit { .. }),
        |struct_id, mut struct_data, hlr| {
            let mut struct_data_clone = struct_data.clone();

            let NodeData::StructLit { ref mut var_type, ref mut fields, ref mut initialize } = 
                &mut struct_data else { unreachable!() };

            let TypeEnum::Variant(variant_type) = var_type.as_type_enum() else { return };
            let TypeEnum::Sum(sum_type) = variant_type.parent.as_type_enum() else { panic!() };
            let struct_parent = hlr.tree.parent(struct_id);

            if sum_type.has_internal_discriminant() {
                let variant_data = if variant_type.variant_type == Type::empty() {
                    let invalid_state_index =
                        if sum_type.largest_variant_index() as u32 > variant_type.tag {
                            variant_type.tag
                        } else {
                            variant_type.tag - 1
                        };

                    hlr.insert_quick(struct_parent, sum_type.tag_data(invalid_state_index))
                } else {
                    let castable_data = if fields.len() == 1 {
                        hlr.tree.get(fields[0].1)
                    } else {
                        let struct_data_type = struct_data_clone.ret_type_mut().unwrap();
                        *struct_data_type = variant_type.as_struct();
                        struct_data_clone
                    };

                    hlr.insert_quick(
                        hlr.tree.parent(struct_id),
                        CallGen {
                            info: UniqueFuncInfo {
                                name: "cast".into(),
                                generics: vec![
                                    variant_type.variant_type.clone(),
                                    variant_type.parent.clone(),
                                ],
                                ..Default::default()
                            },
                            args: vec![box castable_data],
                        },
                    )
                };

                *struct_data = hlr.tree.get(variant_data);
            } else {
                let new_struct = if fields.len() == 1 {
                    hlr.insert_quick(
                        struct_parent,
                        StructLitGen {
                            var_type: variant_type.as_struct(),
                            fields: vec![
                                (
                                    "tag".into(),
                                    box NodeData::Number {
                                        value: variant_type.tag as u64,
                                        lit_type: Type::i(32),
                                    },
                                ),
                                ("data".into(), box hlr.tree.get(fields[0].1)),
                            ],
                            ..Default::default()
                        },
                    )
                } else {
                    let tag = hlr.insert_quick(
                        struct_parent,
                        NodeData::Number {
                            lit_type: Type::i(8),
                            value: variant_type.tag as u64,
                        },
                    );

                    fields.insert(0, (VarName::from("tag"), tag));

                    hlr.tree.insert(
                        struct_parent,
                        NodeData::StructLit {
                            var_type: variant_type.as_struct(),
                            fields: fields.clone(),
                            initialize: *initialize,
                        },
                    )
                };

                let new_struct_casted = hlr.insert_quick(
                    struct_parent,
                    CallGen {
                        info: crate::UniqueFuncInfo {
                            name: VarName::from("cast"),
                            generics: vec![
                                variant_type.as_struct(),
                                variant_type.parent.clone(),
                            ],
                            ..Default::default()
                        },
                        args: vec![box hlr.tree.get(new_struct)],
                    },
                );

                *struct_data = hlr.tree.get(new_struct_casted);
            }
        },
    );
}
