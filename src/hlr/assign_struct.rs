use crate::{lex::VarName, typ::StructType, TypeEnum};

use super::{
    expr_tree::{MakeVarGen, MemberGen, NodeData, SetVarGen},
    hlr_data::FuncRep,
};

pub fn assign_struct(hlr: &mut FuncRep) {
    hlr.modify_many_rev(
        |data| matches!(data, NodeData::StructLit { .. }),
        |structlit, struct_data, hlr| {
            let new_struct_name = VarName::from(structlit.to_string() + "struct");
            let struct_type = struct_data.ret_type();
            let NodeData::StructLit { fields: fields_ids, .. } = struct_data 
                else { unreachable!() };

            let mut current_statement = hlr
                .insert_statement_before(
                    structlit,
                    MakeVarGen {
                        set: new_struct_name.clone(),
                        var_type: struct_type.clone(),
                        ..Default::default()
                    },
                )
                .inserted_id();

            let TypeEnum::Struct(StructType { fields }) = struct_type.as_type_enum()
                else { unreachable!() };

            for ((field_name, field_type), (_, field_id)) 
                in fields.iter().zip(fields_ids) {
                current_statement = hlr
                    .insert_statement_after(
                        current_statement,
                        SetVarGen {
                            lhs: box MemberGen {
                                object: box NodeData::Ident {
                                    var_type: struct_type.clone(),
                                    name: new_struct_name.clone(),
                                },
                                field: field_name.clone(),
                                ret_type: field_type.clone(),
                            },
                            rhs: box *field_id,
                            ..Default::default()
                        },
                    )
                    .inserted_id();
            }

            *struct_data = NodeData::Ident {
                var_type: struct_type.clone(),
                name: new_struct_name.clone(),
            };
        },
    )
}
