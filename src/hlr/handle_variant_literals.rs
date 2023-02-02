use crate::{
    hlr::expr_tree::SetVarGen, hlr::hlr_data::DataFlowInfo,
    typ::fields_iter::PrimitiveFieldsIter, Type, TypeEnum,
};

use super::{
    expr_tree::{MakeVarGen, NodeData, StructLitGen},
    hlr_data::FuncRep,
};

pub fn handle_variant_literals(hlr: &mut FuncRep) {
    hlr.modify_many(
        |data| matches!(data, NodeData::StructLit { .. }),
        |struct_id, mut struct_data, hlr| {
            let NodeData::StructLit { ref mut var_type, ref mut fields, ref mut initialize } = 
                &mut struct_data else { unreachable!() };

            let TypeEnum::Variant(variant_type) = var_type.as_type_enum() else { return };
            let TypeEnum::Sum(sum_type) = variant_type.parent.as_type_enum() else { panic!() };
            let struct_parent = hlr.tree.parent(struct_id);

            if sum_type.is_discriminant_nullref() {
                if variant_type.variant_type == Type::empty() {
                    let nullref_variant = hlr.uniqueify_varname("nullref_variant");

                    hlr.data_flow.insert(
                        nullref_variant.clone(),
                        DataFlowInfo {
                            typ: sum_type.largest_variant(),
                            arg_index: None,
                        },
                    );

                    let nullref_variant_node_data = NodeData::Ident {
                        var_type: Type::i(32),
                        name: nullref_variant.clone(),
                    };

                    let nullref_variant_type = sum_type.largest_variant().clone();

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
                            rhs: box NodeData::Number { value: 0, size: 64 },
                            ..Default::default()
                        },
                    );

                    *struct_data = nullref_variant_node_data;
                } else {
                    *struct_data = if fields.len() == 1 {
                        hlr.tree.get(fields[0].1)
                    } else {
                        NodeData::StructLit {
                            var_type: variant_type.variant_type.clone(),
                            fields: fields.clone(),
                            initialize: *initialize,
                        }
                    };
                }
            } else {
                let variant_data = if fields.len() == 1 {
                    hlr.tree.get(fields[0].1)
                } else {
                    NodeData::StructLit {
                        var_type: variant_type.variant_type.clone(),
                        fields: fields.clone(),
                        initialize: *initialize,
                    }
                };

                let new_struct = hlr.insert_quick(
                    struct_parent,
                    StructLitGen {
                        var_type: variant_type.as_struct(),
                        fields: vec![
                            (
                                "tag".into(),
                                box NodeData::Number {
                                    value: variant_type.tag as u64,
                                    size: 32,
                                },
                            ),
                            ("data".into(), box variant_data),
                        ],
                        ..Default::default()
                    },
                );

                *struct_data = hlr.tree.get(new_struct);
            }
        },
    );
}
