use crate::{lex::VarName, parse::{InitOpts, Opcode}, TypeEnum, TypeRelation, FuncQuery, typ::Field, Type};

use super::{
    expr_tree::*,
    hlr_data::FuncRep,
};

pub fn active_initialization(hlr: &mut FuncRep) {
    handle_struct_active_initialization(hlr);
    handle_array_active_initialization(hlr);
}

fn handle_struct_active_initialization(hlr: &mut FuncRep) {
    hlr.modify_many_infallible(
        |structlit_id, hlr| {
            let mut struct_data = hlr.tree.get(structlit_id);
            let HNodeData::StructLit { ref var_type, fields: ref mut field_ids, ref initialize } = 
                struct_data else { return };

            if *initialize != InitOpts::Default {
                return;
            }

            let TypeEnum::Struct(struct_type) = var_type.as_type_enum() 
                else { todo!("This literal can only use a struct type") };

            let new_default = hlr.add_variable(&var_type);

            hlr.insert_statement_before(
                structlit_id,
                SetGen {
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

            hlr.tree.replace(structlit_id, struct_data);
        },
    );
}

fn handle_array_active_initialization(hlr: &mut FuncRep) {
    hlr.modify_many_infallible(
        |arraylit_id, hlr| {
            let mut arraylit = hlr.tree.get(arraylit_id);
            let HNodeData::ArrayLit { ref var_type, parts: ref mut part_ids, ref initialize } = 
                arraylit else { return };

            if *initialize != InitOpts::Default {
                return;
            }

            let TypeEnum::Array(array_type) = var_type.as_type_enum() 
                else { panic!() };

            let default_call = CallGen {
                query: FuncQuery {
                    name: VarName::from("default"),
                    relation: TypeRelation::Static(array_type.base.clone()),
                    ..Default::default()
                },
                ..Default::default()
            };

            if array_type.count <= 8 {
                let new_default = hlr.insert_quick(
                    arraylit_id,
                    default_call,
                );

                for _ in part_ids.len()..(array_type.count as usize) {
                    part_ids.push(new_default);
                }

                hlr.replace_quick(arraylit_id, arraylit);
            } else {
                let array = hlr.separate_expression(arraylit_id);
                let pos = hlr.add_variable(&Type::u(64));

                hlr.insert_statement_before(
                    arraylit_id,
                    SetGen {
                        lhs: Box::new(pos),
                        rhs: Box::new(HNodeData::Lit {
                            lit: HLit::Int(0),
                            var_type: Type::u(64),
                        }),
                    }
                );

                hlr.insert_statement_before(
                    arraylit_id,
                    WhileGen {
                        w: BinOpGen {
                            lhs: Box::new(pos),
                            op: Opcode::LessThan,
                            rhs: Box::new(HNodeData::Lit {
                                lit: HLit::Int(array_type.count as u64),
                                var_type: Type::u(64), 
                            }),
                        },
                        d: {
                            let mut stmts = Vec::<Box<dyn NodeDataGen>>::new();
                            stmts.push(Box::new(SetGen {
                                lhs: IndexGen { object: array, index: pos },
                                rhs: default_call,
                            }));
                            stmts.push(Box::new(SetGen {
                                lhs: pos,
                                rhs: BinOpGen {
                                    lhs: Box::new(pos),
                                    op: Opcode::Plus,
                                    rhs: Box::new(HNodeData::Lit {
                                        lit: HLit::Int(1),
                                        var_type: Type::u(64),
                                    })
                                }
                            }));
                            stmts
                        },
                    }
                );

                hlr.replace_quick(arraylit_id, array);
            }
        },
    );
}
