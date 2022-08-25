use super::prelude::*;
use crate::parse::*;
use crate::unit::FunctionDef;
use crate::unit::Functions;
use std::any::type_name;
use std::collections::HashMap;

pub fn infer_types(hlr: &mut FuncRep, globals: &Functions) {
    let mut type_by_id = HashMap::new();

    for (n, node) in hlr.tree.top_down_iter() {
        match node {
            NodeData::MakeVar {
                type_spec,
                ref mut var_type,
                name,
                ..
            } => {
                *var_type = match type_spec {
                    Some(type_spec) => {
                        let var_type =
                            hlr.types.get_spec(type_spec).unwrap().clone();
                        hlr.data_flow.get_mut(&name.clone()).unwrap().typ =
                            var_type.clone();
                        var_type
                    },
                    None => {
                        hlr.data_flow.get_mut(&name.clone()).unwrap().typ.clone()
                    },
                }
                .clone();
            },
            NodeData::Ident {
                ref mut var_type,
                name,
            } => {
                let instances = hlr.data_flow.get_mut(&name.clone()).unwrap();
                *var_type = instances.typ.clone();
            },
            _ => {},
        };

        type_by_id.insert(n, node.ret_type());
    }

    for (n, node) in hlr.tree.top_down_iter().rev() {
        match node {
            NodeData::BinOp {
                ref mut ret_type,
                rhs,
                ..
            } => {
                *ret_type = type_by_id.get(rhs).unwrap().clone();
                *type_by_id.get_mut(&n).unwrap() = ret_type.clone();
            },
            NodeData::UnarOp {
                ref mut ret_type,
                hs,
                op,
                ..
            } => {
                *ret_type = type_by_id.get(hs).unwrap().clone();

                match op {
                    Opcode::Deref(count) => {
                        *ret_type = ret_type.clone().deref_x_times(*count).unwrap();
                    },
                    Opcode::Ref(count) => {
                        *ret_type = ret_type.clone().ref_x_times(*count)
                    },
                    _ => unreachable!(),
                }

                *type_by_id.get_mut(&n).unwrap() = ret_type.clone();
            },
            NodeData::SetVar {
                ref mut ret_type,
                lhs,
                rhs,
            } => {
                *ret_type = type_by_id.get(lhs).unwrap().clone();

                *type_by_id.get_mut(&n).unwrap() = ret_type.clone();
            },
            NodeData::Member {
                ref mut ret_type,
                object,
                field,
            } => {
                let typ = type_by_id.get(object).unwrap().clone();
                let type_enum = typ.as_type_enum();
                let TypeEnum::Struct(struct_type) = type_enum else { panic!() };

                *ret_type = struct_type.get_field_type(field).clone();

                *type_by_id.get_mut(&n).unwrap() = ret_type.clone();
            },
            NodeData::Call {
                ref mut ret_type,
                name,
                a,
                def,
            } => {
                let arg_types = a
                    .iter()
                    .map(|id| type_by_id.get_mut(&id).unwrap().clone())
                    .collect();

                let new_def = FunctionDef {
                    name: name.clone(),
                    arg_types,
                };

                let return_type = globals.get_type(new_def.clone()).unwrap();

                *ret_type = return_type.clone();
                *def = Some(new_def);

                *type_by_id.get_mut(&n).unwrap() = ret_type.clone();
            },
            _ => {},
        }
    }
}
