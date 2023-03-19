use crate::{lex::VarName, parse::InitOpts, TypeEnum, TypeRelation, UniqueFuncInfo};

use super::{
    expr_tree::{CallGen, IndexGen, MakeVarGen, MemberGen, NodeData, SetVarGen},
    hlr_data::{DataFlowInfo, FuncRep},
};

pub fn active_initialization(hlr: &mut FuncRep) {
    handle_struct_active_initialization(hlr);
    handle_array_active_initialization(hlr);
}

fn handle_struct_active_initialization(hlr: &mut FuncRep) {
    hlr.modify_many(
        |data| matches!(data, NodeData::StructLit { .. }),
        |structlit_id, structlit_data, hlr| {
            let NodeData::StructLit { var_type, fields: field_ids, initialize } = 
                structlit_data else { panic!() };

            if *initialize != InitOpts::Default {
                return;
            }

            let TypeEnum::Struct(struct_type) = var_type.as_type_enum() 
                else { panic!() };

            let new_default_var_name = hlr.uniqueify_varname("default");

            hlr.insert_statement_before(
                structlit_id,
                MakeVarGen {
                    set: new_default_var_name.clone(),
                    var_type: var_type.clone(),
                    to: box CallGen {
                        info: UniqueFuncInfo {
                            name: VarName::from("default"),
                            relation: TypeRelation::Static(var_type.clone()),
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
            );

            for (field_name, field_type) in &struct_type.fields {
                if field_ids.iter().any(|(name, _)| name == field_name) {
                    // struct already has this field, it does not need to be set
                    // to its default value
                    continue;
                }

                let initialized = hlr.insert_quick(
                    structlit_id,
                    MemberGen {
                        object: box NodeData::Ident {
                            var_type: var_type.clone(),
                            name: new_default_var_name.clone(),
                        },
                        field: field_name.clone(),
                        ret_type: field_type.clone(),
                    },
                );

                field_ids.push((field_name.clone(), initialized));
            }
        },
    );
}

fn handle_array_active_initialization(hlr: &mut FuncRep) {
    hlr.modify_many(
        |data| matches!(data, NodeData::ArrayLit { .. }),
        |arraylit_id, arraylit_data, hlr| {
            let NodeData::ArrayLit { var_type, parts: part_ids, initialize } = 
                arraylit_data.clone() else { panic!() };

            if initialize != InitOpts::Default {
                return;
            }

            let TypeEnum::Array(array_type) = var_type.as_type_enum() 
                else { panic!() };

            let defaulted_array = hlr.uniqueify_varname("defaulted_array");

            hlr.data_flow.insert(
                defaulted_array.clone(),
                DataFlowInfo {
                    typ: var_type.clone(),
                    ..Default::default()
                },
            );

            let set_default_array = hlr
                .insert_statement_before(
                    arraylit_id,
                    MakeVarGen {
                        set: defaulted_array.clone(),
                        var_type: var_type.clone(),
                        to: box arraylit_data.clone(),
                        ..Default::default()
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

            *arraylit_data = NodeData::Ident {
                name: defaulted_array,
                var_type: var_type.clone(),
            };
        },
    );
}
