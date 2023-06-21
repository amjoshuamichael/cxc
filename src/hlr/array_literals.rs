use crate::{Type, TypeEnum, ArrayType};

use super::{
    expr_tree::{MemberGen, HNodeData, SetVarGen, IndexGen},
    hlr_data::FuncRep,
};

pub fn array_literals(hlr: &mut FuncRep) {
    hlr.modify_many_infallible_rev(
        |arr_id, arr_data, hlr| {
            let HNodeData::ArrayLit { ref mut var_type, parts: part_exprs, .. } = arr_data
                else { return };
            let TypeEnum::Array(ArrayType { base, .. }) = var_type.as_type_enum() 
                else { unreachable!() };

            let new_array = 
                hlr.add_variable("array", &var_type);

            let mut current_statement = arr_id;

            for (index, part_expr) in part_exprs.iter().enumerate().rev() {
                current_statement = hlr
                    .insert_statement_before(
                        current_statement,
                        SetVarGen {
                            lhs: box IndexGen {
                                object: box new_array.clone(),
                                index: box HNodeData::Number {
                                    lit_type: Type::i(64),
                                    value: index as u64,
                                },
                                ret_type: base.clone(),
                            },
                            rhs: box hlr.tree.get(*part_expr),
                        },
                    )
                    .inserted_id();
            }

            *arr_data = HNodeData::Ident {
                var_type: var_type.clone(),
                name: new_array,
            };
        },
    )
}
