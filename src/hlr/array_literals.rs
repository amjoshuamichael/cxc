use crate::{Type, TypeEnum, ArrayType};

use super::{
    expr_tree::{HNodeData, SetGen, IndexGen, HLit},
    hlr_data::FuncRep,
};

pub fn array_literals(hlr: &mut FuncRep) {
    hlr.modify_many_infallible_rev(
        |arr_id, hlr| {
            let array_lit = hlr.tree.get(arr_id);
            let HNodeData::ArrayLit { ref var_type, parts: ref part_exprs, .. } = 
                array_lit else { return };
            let TypeEnum::Array(ArrayType { base, .. }) = var_type.as_type_enum() 
                else { unreachable!() };

            let new_array = hlr.add_variable(&var_type);

            let mut current_statement = arr_id;

            for (index, part_expr) in part_exprs.iter().enumerate().rev() {
                current_statement = hlr
                    .insert_statement_before(
                        current_statement,
                        SetGen {
                            lhs: IndexGen {
                                object: new_array.clone(),
                                index: HNodeData::Lit {
                                    lit: HLit::Int(index as u64),
                                    var_type: Type::i(64),
                                },
                            },
                            rhs: *part_expr,
                        },
                    )
                    .inserted_id();
            }

            hlr.tree.replace(arr_id, HNodeData::Ident {
                var_type: var_type.clone(),
                var_id: new_array,
            });
        },
    )
}
