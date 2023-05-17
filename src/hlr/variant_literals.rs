use crate::{Type, TypeEnum, UniqueFuncInfo, VarName, StructType};

use super::{
    expr_tree::{CallGen, HNodeData, StructLitGen},
    hlr_data::FuncRep,
};

pub fn variant_literals(hlr: &mut FuncRep) {
    hlr.modify_many_infallible_rev(
        |struct_id, mut struct_data, hlr| {
            let mut struct_data_clone = struct_data.clone();

            let HNodeData::StructLit { ref mut var_type, ref mut fields, ref mut initialize } = 
                &mut struct_data else { return };

            let TypeEnum::Variant(variant_type) = var_type.as_type_enum() else { return };
            let struct_parent = hlr.tree.parent(struct_id);

            if variant_type.parent.has_internal_discriminant() {
                let variant_data = if variant_type.variant_type == Type::empty() {
                    let largest_variant = variant_type.parent.largest_variant_index();

                    let invalid_state_index =
                        if largest_variant as u32 > variant_type.tag {
                            variant_type.tag
                        } else {
                            variant_type.tag - 1
                        };

                    let data = variant_type.parent.tag_data(invalid_state_index);
                    hlr.insert_quick(struct_parent, data)
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
                                    variant_type.parent_type(),
                                ],
                                ..Default::default()
                            },
                            args: vec![box castable_data],
                        },
                    )
                };

                *struct_data = hlr.tree.get(variant_data);
            } else {
                let variant_type_as_struct = variant_type.as_struct();
                let tag_type = {
                    let TypeEnum::Struct(StructType { ref fields, .. }) = 
                        variant_type_as_struct.as_type_enum() else { unreachable!() };

                    fields
                        .iter()
                        .find(|(name, _)| &*name.to_string() == "tag")
                        .unwrap()
                        .1
                        .clone()
                };

                let new_struct = if fields.len() == 1 {
                    hlr.insert_quick(
                        struct_parent,
                        StructLitGen {
                            var_type: variant_type_as_struct.clone(),
                            fields: vec![
                                (
                                    "tag".into(),
                                    box HNodeData::Number {
                                        value: variant_type.tag as u64,
                                        lit_type: tag_type,
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
                        HNodeData::Number {
                            lit_type: tag_type,
                            value: variant_type.tag as u64,
                        },
                    );

                    fields.insert(0, (VarName::from("tag"), tag));

                    hlr.tree.insert(
                        struct_parent,
                        HNodeData::StructLit {
                            var_type: variant_type_as_struct.clone(),
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
                                variant_type_as_struct,
                                variant_type.parent_type(),
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
