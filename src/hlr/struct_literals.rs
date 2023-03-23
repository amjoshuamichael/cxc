use crate::{typ::StructType, TypeEnum, errors::CResultMany};

use super::{
    expr_tree::{MakeVarGen, MemberGen, NodeData, SetVarGen},
    hlr_data::FuncRep,
};

pub fn struct_literals(hlr: &mut FuncRep) -> CResultMany<()> {
    hlr.modify_many_rev(
        |structlit, struct_data, hlr| {
            let new_struct_name = hlr.uniqueify_varname("struct");
            let struct_type = struct_data.ret_type();

            let NodeData::StructLit { fields: field_exprs, .. } = struct_data 
                else { return Ok(()) };

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

            // TODO: error
            let TypeEnum::Struct(StructType { fields: field_types, .. }) = 
                struct_type.as_type_enum() else { todo!() };

            for (field_name, field_expr) in field_exprs {
                let (_, field_type) = field_types
                    .iter()
                    .find(|(name, _)| name == field_name)
                    .unwrap();

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
                            rhs: box hlr.tree.get(*field_expr),
                        },
                    )
                    .inserted_id();
            }

            *struct_data = NodeData::Ident {
                var_type: struct_type.clone(),
                name: new_struct_name,
            };

            Ok(())
        },
    )
}
