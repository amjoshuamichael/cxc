use super::prelude::*;
use crate::parse::*;
use std::collections::HashMap;

pub fn infer_types(hlr: &mut FuncRep) {
    let mut types = HashMap::new();

    for (n, node) in hlr.tree.top_down_iter() {
        match node {
            NodeData::SetVar {
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

        if let Some(ret_type) = node.ret_type() {
            types.insert(n, ret_type);
        }
    }

    for (n, node) in hlr.tree.top_down_iter().rev() {
        match node {
            NodeData::BinOp {
                ref mut ret_type,
                rhs,
                ..
            } => {
                *ret_type = types.get(rhs).unwrap().clone();
                *types.get_mut(&n).unwrap() = ret_type.clone();
            },
            NodeData::UnarOp {
                ref mut ret_type,
                hs,
                op,
                ..
            } => {
                *ret_type = types.get(hs).unwrap().clone();

                match op {
                    Opcode::Deref(count) => ret_type.ref_count -= *count,
                    Opcode::Ref(count) => ret_type.ref_count += *count,
                    _ => unreachable!(),
                }

                *types.get_mut(&n).unwrap() = ret_type.clone();
            },
            _ => {},
        }
    }
}
