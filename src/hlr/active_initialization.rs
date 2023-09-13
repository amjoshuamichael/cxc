use crate::{lex::VarName, parse::InitOpts, TypeEnum, TypeRelation, FuncQuery, typ::Field};

use super::{
    expr_tree::{CallGen, MemberGen, HNodeData, SetVarGen},
    hlr_data::FuncRep,
};

#[cfg_attr(debug_assertions, inline(never))]
pub fn active_initialization(hlr: &mut FuncRep) {
    handle_struct_active_initialization(hlr);
    handle_array_active_initialization(hlr);
}

#[cfg_attr(debug_assertions, inline(never))]
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

            let new_default = 
                hlr.add_variable("default", &var_type);

            hlr.insert_statement_before(
                structlit_id,
                SetVarGen {
                    lhs: new_default.clone(),
                    rhs: CallGen {
                        query: FuncQuery {
                            name: VarName::from("default"),
                            relation: TypeRelation::Static(var_type.clone()),
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                },
            );

            for Field { name: field_name, .. } in &struct_type.fields {
                if field_ids.iter().any(|(name, _)| name == field_name) {
                    // struct already has this field, it does not need to be set
                    // to its default value
                    continue;
                }

                let initialized = hlr.insert_quick(
                    structlit_id,
                    MemberGen {
                        object: new_default.clone(),
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
            let HNodeData::ArrayLit { var_type, parts: ref mut part_ids, initialize } = 
                arraylit_data else { return };

            if *initialize != InitOpts::Default {
                return;
            }

            let TypeEnum::Array(array_type) = var_type.as_type_enum() 
                else { panic!() };

            let new_default = hlr.insert_quick(
                arraylit_id,
                FuncQuery {
                    name: VarName::from("default"),
                    relation: TypeRelation::Static(array_type.base.clone()),
                    ..Default::default()
                }
            );

            for _ in part_ids.len()..(array_type.count as usize) {
                part_ids.push(new_default);
            }
        },
    );
}
