use crate::{Type, TypeEnum, unit::CompData, typ::FuncType};
use super::prelude::*;
use crate::parse::*;
use std::{collections::HashMap, rc::Rc};

pub fn infer_types(hlr: &mut FuncRep) {
    let mut type_by_id = HashMap::new();

    for n in hlr.tree.ids() {
        let mut node = hlr.tree.get(n);

        match node {
            NodeData::MakeVar {
                ref var_type,
                ref name,
                ..
            } => {
                hlr.data_flow.get_mut(&name.clone()).unwrap().typ = var_type.clone();
            },
            NodeData::Ident {
                ref mut var_type,
                ref name,
            } => {
                let instances = hlr.data_flow.get_mut(&name.clone()).unwrap();
                *var_type = instances.typ.clone();
            },
            _ => {},
        };

        type_by_id.insert(n, node.ret_type());
        hlr.tree.replace(n, node)
    }

    for n in hlr.tree.ids().rev() {
        let mut node = hlr.tree.get(n);

        match node {
            NodeData::Return { to_return, .. } => {
                hlr.ret_type = 
                    type_by_id.get(&to_return.unwrap()).unwrap().clone();
            }
            NodeData::BinOp {
                ref mut ret_type,
                ref lhs,
                op,
                ..
            } => {
                use crate::parse::Opcode::*;
                *ret_type = match op {
                    Equal | Inequal | GrtrThan | GreaterOrEqual | LessThan
                    | LessOrEqual => Type::i(64),
                    _ => type_by_id.get(lhs).unwrap().clone(),
                };
                *type_by_id.get_mut(&n).unwrap() = ret_type.clone();
            },
            NodeData::UnarOp {
                ref mut ret_type,
                ref hs,
                op,
                ..
            } => {
                let hs_type = type_by_id.get(hs).unwrap().clone();

                match op {
                    Opcode::Deref(count) => {
                        *ret_type = hs_type.clone().deref_x_times(count).unwrap();
                    },
                    Opcode::Ref(count) => {
                        *ret_type = hs_type.clone().ref_x_times(count)
                    },
                    Opcode::Not => {
                        *ret_type = Type::bool()
                    }
                    _ => unreachable!(),
                }

                *type_by_id.get_mut(&n).unwrap() = ret_type.clone();
            },
            NodeData::SetVar {
                ref mut ret_type,
                ref lhs,
                ..
            } => {
                *ret_type = type_by_id.get(lhs).unwrap().clone();

                *type_by_id.get_mut(&n).unwrap() = ret_type.clone();
            },
            NodeData::Member {
                ref mut ret_type,
                ref object,
                ref field,
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
                ..            
            } => {
                let func_info = {
                    let call = hlr.tree.get(n);
                    hlr.tree.unique_func_info_of_call(&call)
                };

                let func_type = hlr.types.get_type(&func_info).unwrap();
                let TypeEnum::Func(FuncType { ret_type: return_type, .. }) = 
                    func_type.as_type_enum() else { panic!() };

                *ret_type = return_type.clone();

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

                *ret_type = array_type.base.clone();

                *type_by_id.get_mut(&n).unwrap() = ret_type.clone();
            },
            NodeData::ArrayLit {
                ref mut var_type,
                ref parts,
            } => {
                if var_type.is_never() {
                    *var_type = type_by_id.get(&parts[0]).unwrap().clone().get_array(parts.len() as u32);
                }
                *type_by_id.get_mut(&n).unwrap() = var_type.clone();
            }
            _ => {},
        }

        hlr.tree.replace(n, node);
    }
}
