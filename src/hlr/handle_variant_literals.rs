use crate::TypeEnum;

use super::{
    expr_tree::{NodeData, StructLitGen},
    hlr_data::FuncRep,
};

pub fn handle_variant_literals(hlr: &mut FuncRep) {
    hlr.modify_many(
        |data| matches!(data, NodeData::StructLit { .. }),
        |struct_id, struct_data, hlr| {
            let NodeData::StructLit { var_type, fields, initialize } = 
                struct_data.clone() else { unreachable!() };

            let TypeEnum::Variant(variant_type) = var_type.as_type_enum() else { return };
            let struct_parent = hlr.tree.parent(struct_id);

            let variant_data = if fields.len() == 1 {
                hlr.tree.get(fields[0].1)
            } else {
                NodeData::StructLit {
                    var_type: variant_type.variant_type.clone(),
                    fields: fields.clone(),
                    initialize,
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
        },
    );
}
