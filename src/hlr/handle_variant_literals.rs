use crate::{
    hlr::expr_tree::SetVarGen, hlr::hlr_data::DataFlowInfo,
    typ::fields_iter::PrimitiveFieldsIter, Type, TypeEnum, UniqueFuncInfo, VarName,
};

use super::{
    expr_tree::{CallGen, MakeVarGen, NodeData, StructLitGen},
    hlr_data::FuncRep,
};

pub fn handle_variant_literals(hlr: &mut FuncRep) {
    hlr.modify_many(
        |data| matches!(data, NodeData::StructLit { .. }),
        |struct_id, mut struct_data, hlr| {
            let mut struct_data_clone = struct_data.clone();

            let NodeData::StructLit { ref mut var_type, ref mut fields, ref mut initialize } = 
                &mut struct_data else { unreachable!() };

            let TypeEnum::Variant(variant_type) = var_type.as_type_enum() else { return };
            let TypeEnum::Sum(sum_type) = variant_type.parent.as_type_enum() else { panic!() };
            let struct_parent = hlr.tree.parent(struct_id);

            if sum_type.is_discriminant_nullref() {
                if variant_type.variant_type == Type::empty() {
                    // TODO: This all might be unnessesary???
                    let nullref_variant = hlr.uniqueify_varname("nullref_variant");

                    hlr.data_flow.insert(
                        nullref_variant.clone(),
                        DataFlowInfo {
                            typ: sum_type.largest_variant_data(),
                            arg_index: None,
                        },
                    );

                    let nullref_variant_node_data = NodeData::Ident {
                        var_type: Type::i(32),
                        name: nullref_variant.clone(),
                    };

                    let nullref_variant_type = sum_type.largest_variant_data().clone();

                    hlr.insert_statement_before(
                        struct_id,
                        MakeVarGen {
                            set: nullref_variant.clone(),
                            var_type: nullref_variant_type.clone(),
                            ..Default::default()
                        },
                    );

                    let nullref_access =
                        if matches!(nullref_variant_type.as_type_enum(), TypeEnum::Ref(_)) {
                            box nullref_variant_node_data.clone()
                        } else {
                            let mut prim_iter = PrimitiveFieldsIter::new(nullref_variant_type);
                            prim_iter
                                .find(|typ| matches!(typ.as_type_enum(), TypeEnum::Ref(_)));
                            prim_iter
                                .build_accessor_nodes(box nullref_variant_node_data.clone())
                        };

                    hlr.insert_statement_before(
                        struct_id,
                        SetVarGen {
                            lhs: nullref_access,
                            rhs: box NodeData::Number {
                                value: 0,
                                lit_type: Type::i(64),
                            },
                            ..Default::default()
                        },
                    );

                    *struct_data = nullref_variant_node_data;
                } else {
                    let castable_data = if fields.len() == 1 {
                        hlr.tree.get(fields[0].1.clone())
                    } else {
                        let struct_data_type = struct_data_clone.ret_type_mut().unwrap();
                        *struct_data_type = variant_type.as_struct();
                        struct_data_clone
                    };

                    let new_data_id = hlr.insert_quick(
                        hlr.tree.parent(struct_id),
                        CallGen {
                            info: UniqueFuncInfo {
                                name: "cast".into(),
                                own_generics: vec![
                                    variant_type.variant_type.clone(),
                                    variant_type.parent.clone(),
                                ],
                                ..Default::default()
                            },
                            args: vec![box castable_data],
                        },
                    );

                    *struct_data = hlr.tree.get(new_data_id);
                }
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
                            own_generics: vec![
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
