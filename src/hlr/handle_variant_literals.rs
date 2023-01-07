use crate::{Type, TypeEnum};

use super::{
    expr_tree::{NodeData, StructLitGen},
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
                if *var_type == Type::empty() {
                    panic!()
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
                                    value: variant_type.tag as u128,
                                    size: 32,
                                },
                            ),
                            ("data".into(), box variant_data),
                        ],
                        initialize: false,
                    },
                );

                *struct_data = hlr.tree.get(new_struct);
            }
        },
    );
}
