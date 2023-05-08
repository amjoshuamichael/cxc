use crate::errors::CResultMany;

use super::{
    expr_tree::{MemberGen, HNodeData, SetVarGen},
    hlr_data::FuncRep,
};

pub fn struct_literals(hlr: &mut FuncRep) -> CResultMany<()> {
    hlr.modify_many_rev(
        |structlit, struct_data, hlr| {
            let struct_type = struct_data.ret_type();

            let HNodeData::StructLit { fields: field_exprs, .. } = struct_data 
                else { return Ok(()) };

            let (new_struct, make_new_struct) = 
                hlr.add_variable("struct", &struct_type);

            let mut current_statement = hlr
                .insert_statement_before(
                    structlit,
                    make_new_struct,
                )
                .inserted_id();

            for (field_name, field_expr) in field_exprs {
                current_statement = hlr
                    .insert_statement_after(
                        current_statement,
                        SetVarGen {
                            lhs: box MemberGen {
                                object: box new_struct.clone(),
                                field: field_name.clone(),
                            },
                            rhs: box hlr.tree.get(*field_expr),
                        },
                    )
                    .inserted_id();
            }

            *struct_data = HNodeData::Ident {
                var_type: struct_type.clone(),
                name: new_struct,
            };

            Ok(())
        },
    )
}
