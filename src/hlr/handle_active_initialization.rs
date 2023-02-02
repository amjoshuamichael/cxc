use crate::{lex::VarName, parse::StructFill, TypeEnum, TypeRelation, UniqueFuncInfo};

use super::{
    expr_tree::{CallGen, MakeVarGen, MemberGen, NodeData},
    hlr_data::FuncRep,
};

pub fn handle_active_initialization(hlr: &mut FuncRep) {
    hlr.modify_many(
        |data| matches!(data, NodeData::StructLit { .. }),
        |structlit_id, structlit_data, hlr| {
            let NodeData::StructLit { var_type, fields: field_ids, initialize } = 
                structlit_data else { panic!() };

            if *initialize != StructFill::Default {
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
