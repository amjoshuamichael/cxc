use super::prelude::*;
use crate::parse::prelude::*;
use std::collections::HashMap;

pub fn infer_types(hlr: &mut HLR) {
    let mut types = HashMap::new();

    for (n, node) in hlr.tree.top_down_iter() {
        match node {
            NodeData::VarDecl {
                type_spec,
                ref mut var_type,
                name,
                ..
            } => {
                *var_type = match type_spec {
                    Some(type_spec) => hlr.types.get_spec(type_spec).unwrap().clone(),
                    None => hlr.data_flow.get_mut(&name.clone()).unwrap().0.clone(),
                }
                .clone();

                hlr.data_flow.insert(name.clone(), (var_type.clone(), vec![n]));
            }
            NodeData::Ident { ref mut var_type, name } => {
                let instances = hlr.data_flow.get_mut(&name.clone()).unwrap();
                instances.1.push(n);

                *var_type = instances.0.clone();
            }
            _ => {}
        };

        if let Some(ret_type) = node.ret_type() {
            types.insert(n, ret_type);
        }
    }

    for (n, node) in hlr.tree.bottom_up_iter() {
        match node {
            NodeData::BinOp {
                ref mut ret_type, rhs, ..
            } => {
                *ret_type = types.get(rhs).unwrap().clone();
                *types.get_mut(&n).unwrap() = ret_type.clone();
            }
            NodeData::UnarOp {
                ref mut ret_type,
                hs,
                op,
                ..
            } => {
                *ret_type = types.get(hs).unwrap().clone();

                match op {
                    Opcode::Deref => ret_type.ref_count -= 1,
                    Opcode::Ref => ret_type.ref_count += 1,
                    _ => unreachable!(),
                }

                *types.get_mut(&n).unwrap() = ret_type.clone();
            }
            _ => {}
        }
    }
}
