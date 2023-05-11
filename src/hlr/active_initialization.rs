use crate::{lex::VarName, parse::InitOpts, TypeEnum, TypeRelation, UniqueFuncInfo};

use super::{
    expr_tree::{CallGen, IndexGen, MakeVarGen, MemberGen, HNodeData, SetVarGen},
    hlr_data::FuncRep,
};

pub fn active_initialization(hlr: &mut FuncRep) {
    handle_struct_active_initialization(hlr);
    handle_array_active_initialization(hlr);
}

fn handle_struct_active_initialization(hlr: &mut FuncRep) {
    hlr.modify_many_infallible(
        |structlit_id, structlit_data, hlr| {
            let HNodeData::StructLit { var_type, fields: field_ids, initialize } = 
                structlit_data else { return };

            if *initialize != InitOpts::Default {
                return;
            }

            let TypeEnum::Struct(struct_type) = var_type.as_type_enum() 
                else { todo!("This literal can only use a struct type") };

            let (new_default, _) = 
                hlr.add_variable("default", &var_type);

            hlr.insert_statement_before(
                structlit_id,
                SetVarGen {
                    lhs: box new_default.clone(),
                    rhs: box CallGen {
                        info: UniqueFuncInfo {
                            name: VarName::from("default"),
                            relation: TypeRelation::Static(var_type.clone()),
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                },
            );

            for (field_name, _) in &struct_type.fields {
                if field_ids.iter().any(|(name, _)| name == field_name) {
                    // struct already has this field, it does not need to be set
                    // to its default value
                    continue;
                }

                let initialized = hlr.insert_quick(
                    structlit_id,
                    MemberGen {
                        object: box new_default.clone(),
                        field: field_name.clone(),
                    },
                );

                field_ids.push((field_name.clone(), initialized));
            }
        },
    );
}

fn handle_array_active_initialization(hlr: &mut FuncRep) {
    hlr.modify_many_infallible(
        |arraylit_id, arraylit_data, hlr| {
            let HNodeData::ArrayLit { var_type, parts: part_ids, initialize } = 
                arraylit_data.clone() else { return };

            if initialize != InitOpts::Default {
                return;
            }

            let TypeEnum::Array(array_type) = var_type.as_type_enum() 
                else { panic!() };

            let (defaulted_array, make_defaulted_array) = 
                hlr.add_variable("defaulted_array", &var_type);

            let set_default_array = hlr
                .insert_statement_before(
                    arraylit_id,
                    SetVarGen {
                        lhs: box defaulted_array.clone(),
                        rhs: box arraylit_data.clone(),
                    },
                )
                .inserted_id();

            hlr.tree.set_parent(arraylit_id, set_default_array);

            for index in part_ids.len()..(array_type.count as usize) {
                hlr.insert_statement_before(
                    arraylit_id,
                    SetVarGen {
                        lhs: box IndexGen {
                            object: box defaulted_array.clone(),
                            index: box index,
                            ret_type: array_type.base.clone(),
                        },
                        rhs: box UniqueFuncInfo {
                            name: VarName::from("default"),
                            relation: TypeRelation::Static(array_type.base.clone()),
                            ..Default::default()
                        },
                    },
                );
            }

            *arraylit_data = HNodeData::Ident {
                name: defaulted_array,
                var_type: var_type.clone(),
            };
        },
    );
}
