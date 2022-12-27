use super::prelude::*;
use crate::parse::*;
use crate::{typ::FuncType, Type, TypeEnum, UniqueFuncInfo};
use std::collections::HashMap;

pub fn infer_types(hlr: &mut FuncRep) {
    let mut type_by_id = HashMap::new();

    for n in hlr.tree.ids_in_order() {
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
            } => match hlr.data_flow.get(&name.clone()) {
                Some(data_flow_info) => {
                    *var_type = data_flow_info.typ.clone();
                },
                None => {
                    let global = hlr
                        .comp_data
                        .globals
                        .get(&name.clone())
                        .expect(&*format!("could not find identifier {name}"));
                    *var_type = global.0.clone();
                },
            },
            _ => {},
        };

        type_by_id.insert(n, node.ret_type());
        hlr.tree.replace(n, node)
    }

    for n in hlr.tree.ids_in_order().drain(..).rev() {
        let mut node = hlr.tree.get(n);

        match node {
            NodeData::Return { to_return, .. } => {
                hlr.ret_type = type_by_id.get(&to_return.unwrap()).unwrap().clone();
            },
            NodeData::BinOp {
                ref mut ret_type,
                ref lhs,
                op,
                ..
            } => {
                use crate::parse::Opcode::*;
                *ret_type = match op {
                    Equal | Inequal | GrtrThan | GreaterOrEqual | LessThan | LessOrEqual => {
                        Type::i(64)
                    },
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
                    Opcode::Ref(count) => *ret_type = hs_type.clone().ref_x_times(count),
                    Opcode::Not => *ret_type = Type::bool(),
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
                ref mut relation,
                ref a,
                ref f,
                ref generics,
            } => {
                match relation {
                    TypeRelation::MethodOf(ref mut method_origin) => {
                        let method_arg = a.last().unwrap();
                        *method_origin = type_by_id.get(method_arg).unwrap().clone();
                    },
                    _ => {},
                }

                let func_info = UniqueFuncInfo {
                    name: f.clone(),
                    relation: relation.clone(),
                    generics: generics.clone(),
                };

                let func_type = hlr.comp_data.get_type(&func_info).unwrap();
                let TypeEnum::Func(FuncType { ret_type: return_type, .. }) = 
                    func_type.as_type_enum() else { panic!() };

                *ret_type = return_type.clone();

                *type_by_id.get_mut(&n).unwrap() = ret_type.clone();
            },
            NodeData::FirstClassCall {
                ref mut ret_type,
                ref mut f,
                ..
            } => {
                let func_type = type_by_id.get(f).unwrap().clone();
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
                if var_type.is_unknown() {
                    *var_type = type_by_id
                        .get(&parts[0])
                        .unwrap()
                        .clone()
                        .get_array(parts.len() as u32);
                }
                *type_by_id.get_mut(&n).unwrap() = var_type.clone();
            },
            NodeData::Block {
                ref mut ret_type,
                ref stmts,
            } => match hlr.tree.get(*stmts.last().unwrap()) {
                NodeData::Return { to_return, .. } => {
                    if to_return.is_some() {
                        *ret_type = type_by_id.get(&to_return.unwrap()).unwrap().clone();
                    }
                },
                _ => {},
            },
            _ => {},
        }

        hlr.tree.replace(n, node);
    }
}
