use super::prelude::*;
use crate::parse::*;
use crate::unit::Functions;
use crate::unit::UniqueFuncInfo;
use std::collections::HashMap;

pub fn infer_types(hlr: &mut FuncRep, functions: &Functions) {
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
                            hlr.types.get_gen_spec(type_spec, &hlr.generics).unwrap().clone();
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
                op,
                ..
            } => {
                use crate::parse::Opcode::*;
                *ret_type = match op {
                    Equal | Inequal | GrtrThan | GreaterOrEqual | LessThan
                    | LessOrEqual => Type::i(64),
                    _ => type_by_id.get(rhs).unwrap().clone(),
                };
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
                ..
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

                let complete_deref = typ.complete_deref();
                let TypeEnum::Struct(struct_type) = complete_deref
                    .as_type_enum()
                    else { panic!() };

                *ret_type = struct_type.get_field_type(field).unwrap().clone();

                *type_by_id.get_mut(&n).unwrap() = ret_type.clone();
            },
            NodeData::Call {
                ref mut ret_type,
                f,
                a,
                data,
                is_method,
            } => {
                let arg_types = a
                    .iter()
                    .map(|id| type_by_id.get_mut(&id).unwrap().clone())
                    .collect();

                let new_data = UniqueFuncInfo::from(
                    f,
                    &arg_types,
                    *is_method,
                );

                let return_type = functions.get_type(new_data.clone()).unwrap();

                *ret_type = return_type.clone();
                *data = Some(new_data);

                *type_by_id.get_mut(&n).unwrap() = ret_type.clone();
            },
            NodeData::Index {
                ref mut ret_type,
                object,
                ..
            } => {
                let object_type = type_by_id.get_mut(&object).unwrap().clone();
                let TypeEnum::Array(array_type) = object_type.as_type_enum() 
                    else { panic!() };

                *ret_type = array_type.base().clone();

                *type_by_id.get_mut(&n).unwrap() = ret_type.clone();
            },
            NodeData::ArrayLit {
                ref mut var_type,
                parts,
            } => {
                *var_type = type_by_id.get(&parts[0]).unwrap().clone().get_array(parts.len() as u32);
                *type_by_id.get_mut(&n).unwrap() = var_type.clone();
            }
            _ => {},
        }
    }
}
