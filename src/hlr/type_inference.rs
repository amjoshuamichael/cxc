use super::prelude::*;
use crate::parse::*;
use crate::unit::Globals;
use std::any::type_name;
use std::collections::HashMap;

pub fn infer_types(hlr: &mut FuncRep, globals: &Globals) {
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
                        println!("{:?}", type_spec);
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
            NodeData::Global {
                ref mut var_type,
                name,
            } => {
                *var_type = globals.get_type(name.clone()).unwrap();
            },
            NodeData::StructLit {
                ref mut struct_type,
                type_name,
                ..
            } => {
                *struct_type =
                    hlr.types.get_spec(&TypeSpec::new(&*type_name, 0)).unwrap();
            },
            _ => {},
        };

        if let Some(ret_type) = node.ret_type() {
            type_by_id.insert(n, ret_type);
        }
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
                    Opcode::Deref(count) => ret_type.ref_count -= *count,
                    Opcode::Ref(count) => ret_type.ref_count += *count,
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
            NodeData::Call {
                ref mut ret_type,
                f,
                ..
            } => {
                let function_type = type_by_id.get(f).unwrap();
                *ret_type = function_type.func_ret_type();

                *type_by_id.get_mut(&n).unwrap() = ret_type.clone();
            },

            NodeData::Member {
                ref mut ret_type,
                object,
                field,
            } => {
                let object_type = type_by_id.get(object).unwrap();

                *ret_type = object_type.get_field_type(field).clone();
            },
            _ => {},
        }
    }
}
