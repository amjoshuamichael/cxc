use crate::{Type, TypeEnum};

use super::{
    expr_tree::{MemberGen, HNodeData, SetVarGen},
    hlr_data::FuncRep,
};

#[cfg_attr(debug_assertions, inline(never))]
pub fn struct_literals(hlr: &mut FuncRep) {
    hlr.modify_many_infallible_rev(
        |structlit, struct_data, hlr| {
            let HNodeData::StructLit { ref mut var_type, fields: field_exprs, .. } = struct_data 
                else { return };

            if field_exprs.len() == 0 {
                *struct_data = HNodeData::Number { lit_type: Type::i(32), value: 0 };
                return;
            }
            
            let new_struct_type = {
                let TypeEnum::Struct(struct_type) = var_type.as_type_enum()
                    else { unreachable!() };

                let type_is_not_accurate =  field_exprs.iter()
                    .any(|(name, id)| {
                        let typed_field = &struct_type.get_field_type(name).unwrap();
                        let expr_field = hlr.tree.get(*id).ret_type();

                        typed_field.as_type_enum() != expr_field.as_type_enum()
                    });

                if type_is_not_accurate {
                    panic!();
                    //let new_fields = field_exprs
                    //    .iter()
                    //    .map(|(name, id)| Field {
                    //        name: name.clone(), 
                    //        typ: hlr.tree.get(*id).ret_type(),
                    //        inherited: false,
                    //    });
                    //let new_type = Type::new_struct(new_fields.collect());

                    //new_type
                } else {
                    var_type.clone()
                }
            };

            *var_type = new_struct_type.clone();

            let new_struct = 
                hlr.add_variable("struct", &new_struct_type);

            let mut current_statement = structlit;

            for (field_name, field_expr) in field_exprs.iter().rev() {
                current_statement = hlr
                    .insert_statement_before(
                        current_statement,
                        SetVarGen {
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
                var_type: new_struct_type.clone(),
                name: new_struct,
            };
        },
    )
}
