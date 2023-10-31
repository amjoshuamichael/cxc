use crate::Type;

use super::{
    expr_tree::{MemberGen, HNodeData, SetGen},
    hlr_data::FuncRep,
};

pub fn struct_literals(hlr: &mut FuncRep) {
    hlr.modify_many_infallible_rev(
        |structlit, struct_data, hlr| {
            let HNodeData::StructLit { ref mut var_type, fields: field_exprs, .. } = struct_data 
                else { return };

            if field_exprs.len() == 0 {
                *struct_data = HNodeData::zero();
                return;
            }
            
            hlr.tree.replace(structlit, HNodeData::zero());
            let new_struct = hlr.add_variable(&var_type);

            let mut current_statement = structlit;

            for (field_name, field_expr) in field_exprs.iter().rev() {
                current_statement = hlr
                    .insert_statement_before(
                        current_statement,
                        SetGen {
                            lhs: MemberGen {
                                object: new_struct.clone(),
                                field: field_name.clone(),
                            },
                            rhs: *field_expr,
                        },
                    )
                    .inserted_id();
            }

            *struct_data = HNodeData::Ident {
                var_type: var_type.clone(),
                var_id: new_struct,
            };
        },
    )
}
