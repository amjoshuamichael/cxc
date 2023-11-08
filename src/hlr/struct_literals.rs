use crate::Type;

use super::{
    expr_tree::{MemberGen, HNodeData, SetGen},
    hlr_data::FuncRep,
};

pub fn struct_literals(hlr: &mut FuncRep) {
    hlr.modify_many_infallible_rev(
        |structlit_id, hlr| {
            let HNodeData::StructLit { var_type, fields: field_exprs, .. } = 
                hlr.tree.get(structlit_id) else { return };

            hlr.tree.replace(structlit_id, HNodeData::zero());

            if field_exprs.len() == 0 {
                return;
            }

            let new_struct = hlr.add_variable(&var_type);

            let mut current_statement = structlit_id;

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

            hlr.tree.replace(structlit_id, HNodeData::Ident {
                var_type,
                var_id: new_struct,
            });
        },
    )
}
