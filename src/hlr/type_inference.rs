use crate::{
    parse::{Opcode, TypeSpec},
    FuncType, Type, TypeEnum, TypeRelation, VarName,
};
use indexmap::IndexMap;
use indexmap::IndexSet;

use super::prelude::*;
use std::hash::Hash;

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
enum Inferable {
    Expr(ExprID),
    Var(VarName),
    Relation(ExprID),
    CallGeneric(ExprID, usize),
    ReturnType,
}

impl Into<Inferable> for &VarName {
    fn into(self) -> Inferable { Inferable::Var(self.clone()) }
}

impl Into<Inferable> for ExprID {
    fn into(self) -> Inferable { Inferable::Expr(self) }
}

#[derive(Default, Clone, Debug)]
struct InferMap(IndexMap<Inferable, IndexSet<Constraint>>);

impl InferMap {
    pub fn insert(&mut self, inferable: impl Into<Inferable>, constraint: Constraint) {
        let inferable = inferable.into();

        match self.0.get_mut(&inferable) {
            Some(ref mut constraints) => {
                constraints.insert(constraint);
            },
            None => {
                self.0.insert(inferable, [constraint].into_iter().collect());
            },
        }
    }

    pub fn iter(&self) -> indexmap::map::Iter<Inferable, IndexSet<Constraint>> { self.0.iter() }
}

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
enum Constraint {
    IsType(Type),
    RelatedTo {
        spec: TypeSpec,
        gen_params: Vec<Inferable>,
    },
    SameAs(Inferable),
    Call,
}

impl Into<Constraint> for Type {
    fn into(self) -> Constraint { Constraint::IsType(self) }
}

fn type_of_inferable(inferable: &Inferable, hlr: &FuncRep) -> Type {
    use Inferable::*;

    match inferable {
        Expr(id) => hlr.tree.get(*id).ret_type(),
        Var(name) => hlr.data_flow.get(name).unwrap().typ.clone(),
        Relation(id) => {
            let NodeData::Call { relation, .. } = hlr.tree.get(*id) else { panic!() };
            relation.inner_type().unwrap()
        },
        CallGeneric(id, index) => {
            let NodeData::Call { generics, .. } = hlr.tree.get(*id) else { panic!() };
            generics[*index as usize].clone()
        },
        ReturnType => hlr.ret_type.clone(),
    }
}

fn type_of_inferable_mut<'a>(inferable: &'a Inferable, hlr: &'a mut FuncRep) -> &'a mut Type {
    use Inferable::*;

    match inferable {
        Expr(id) => hlr.tree.get_mut(*id).ret_type_mut().unwrap(),
        Var(name) => &mut hlr.data_flow.get_mut(name).unwrap().typ,
        Relation(id) => {
            let NodeData::Call { ref mut relation, .. } = hlr.tree.get_mut(*id) else { panic!() };
            relation.inner_type_mut().unwrap()
        },
        CallGeneric(id, index) => {
            let NodeData::Call { ref mut generics, .. } = hlr.tree.get_mut(*id) else { panic!() };
            &mut generics[*index as usize]
        },
        ReturnType => &mut hlr.ret_type,
    }
}

pub fn infer_types(hlr: &mut FuncRep) {
    let mut infer_map = InferMap::default();

    for id in hlr.tree.ids_in_order().into_iter().rev() {
        let node_data = hlr.tree.get(id);

        if !node_data.ret_type().is_unknown() && !matches!(node_data, NodeData::MakeVar { .. })
        {
            continue;
        }

        match node_data {
            NodeData::Ident { ref name, .. } => {
                infer_map.insert(id, Constraint::SameAs(name.into()));
            },
            NodeData::BinOp { lhs, .. } => {
                infer_map.insert(id, Constraint::SameAs(lhs.into()));
            },
            NodeData::UnarOp { op, hs, .. } => {
                let constraint = match op {
                    Opcode::Not => Constraint::SameAs(hs.into()),
                    Opcode::Ref => Constraint::RelatedTo {
                        spec: TypeSpec::Ref(box TypeSpec::GenParam(0)),
                        gen_params: vec![hs.into()],
                    },
                    Opcode::Deref => Constraint::RelatedTo {
                        spec: TypeSpec::Deref(box TypeSpec::GenParam(0)),
                        gen_params: vec![hs.into()],
                    },
                    _ => continue,
                };

                infer_map.insert(id, constraint);
            },
            NodeData::Call {
                ref relation,
                ref a,
                ..
            } => {
                match relation {
                    TypeRelation::MethodOf(_) => {
                        let last_arg = *a.last().unwrap();
                        infer_map.insert(
                            Inferable::Relation(id),
                            Constraint::SameAs(last_arg.into()),
                        );
                        infer_map.insert(id, Constraint::Call);
                    },
                    TypeRelation::Static(_) | TypeRelation::Unrelated => {
                        infer_map.insert(id, Constraint::Call);
                    },
                };
            },
            NodeData::Member { object, field, .. } => {
                infer_map.insert(
                    id,
                    Constraint::RelatedTo {
                        spec: TypeSpec::StructMember(box TypeSpec::GenParam(0), field.clone()),
                        gen_params: vec![object.into()],
                    },
                );
            },
            NodeData::FirstClassCall { f, .. } => {
                infer_map.insert(
                    id,
                    Constraint::RelatedTo {
                        spec: TypeSpec::FuncReturnType(box TypeSpec::GenParam(0)),
                        gen_params: vec![f.into()],
                    },
                );
            },
            NodeData::Block { stmts, .. } => {
                let last_stmt = stmts[stmts.len() - 1];
                infer_map.insert(id, Constraint::SameAs(last_stmt.into()));
            },
            NodeData::Return { to_return, .. } => {
                if let Some(to_return) = to_return {
                    infer_map.insert(id, Constraint::SameAs(to_return.into()));
                } else {
                    infer_map.insert(id, Constraint::IsType(Type::void()));
                }
            },
            NodeData::ArrayLit { parts, .. } => {
                let first_element = *parts.first().unwrap();
                infer_map.insert(
                    id,
                    Constraint::RelatedTo {
                        spec: TypeSpec::Array(box TypeSpec::GenParam(0), parts.len() as u32),
                        gen_params: vec![first_element.into()],
                    },
                );
            },
            NodeData::Index { object, .. } => infer_map.insert(
                id,
                Constraint::RelatedTo {
                    spec: TypeSpec::ArrayElem(box TypeSpec::GenParam(0)),
                    gen_params: vec![object.into()],
                },
            ),
            NodeData::MakeVar { ref name, rhs, .. } => {
                infer_map.insert(name, Constraint::SameAs(rhs.into()));
                infer_map.insert(id, Constraint::SameAs(name.into()));
            },
            NodeData::Set { lhs, rhs, .. } => {
                infer_map.insert(id, Constraint::IsType(Type::void()));
                infer_map.insert(lhs, Constraint::SameAs(rhs.into()));
            },
            _ => {},
        }
    }

    infer_map.insert(Inferable::ReturnType, Constraint::SameAs(hlr.tree.root.into()));

    for _ in 0..5 {
        type_solving_round(&mut infer_map, hlr);

        if all_types_known(&infer_map, hlr) {
            return;
        }

        introduce_reverse_constraints(&mut infer_map, hlr);
    }

    panic!("type inference failed");
}

fn all_types_known(infer_map: &InferMap, hlr: &FuncRep) -> bool {
    infer_map
        .iter()
        .all(|(inferable, _)| !type_of_inferable(inferable, hlr).is_unknown())
}

pub fn type_spec_from_perspective_of_generic(
    spec: &TypeSpec,
    generic_index: u32,
) -> Option<TypeSpec> {
    use TypeSpec::*;

    match spec {
        GenParam(index) => {
            if *index as u32 == generic_index {
                Some(spec.clone())
            } else {
                None
            }
        },
        Ref(inner) | Deref(inner) | Array(inner, _) => {
            let from_perspective =
                box type_spec_from_perspective_of_generic(inner, generic_index)?;
            let output = match spec {
                Ref(_) => Deref(from_perspective),
                Deref(_) => Ref(from_perspective),
                Array(..) => ArrayElem(from_perspective),
                _ => unreachable!(),
            };

            Some(output)
        },
        _ => None,
    }
}

fn introduce_reverse_constraints(infer_map: &mut InferMap, hlr: &FuncRep) {
    for (original, constraints) in infer_map.clone().iter() {
        for constraint in constraints {
            match constraint {
                Constraint::IsType(_) | Constraint::Call { .. } => {},
                Constraint::SameAs(other) => {
                    if !type_of_inferable(other, hlr).is_unknown() {
                        continue;
                    }

                    infer_map.insert(other.clone(), Constraint::SameAs(original.clone()));
                },
                Constraint::RelatedTo { spec, gen_params } => {
                    for (param_index, param) in gen_params.iter().enumerate() {
                        if !type_of_inferable(param, hlr).is_unknown() {
                            continue;
                        }

                        let reversed_spec =
                            type_spec_from_perspective_of_generic(&spec, param_index as u32);

                        if reversed_spec.is_none() {
                            continue;
                        }

                        infer_map.insert(
                            param.clone(),
                            Constraint::RelatedTo {
                                spec: reversed_spec.unwrap(),
                                gen_params: vec![original.clone()],
                            },
                        );
                    }
                },
            }
        }
    }
}

fn type_solving_round(infer_map: &mut InferMap, hlr: &mut FuncRep) {
    for (unknown, constraints) in infer_map.clone().iter() {
        for constraint in constraints {
            if !type_of_inferable(unknown, hlr).is_unknown() {
                break;
            }

            match constraint {
                Constraint::IsType(typ) => {
                    let set_type = type_of_inferable_mut(unknown, hlr);
                    *set_type = typ.clone();
                },
                Constraint::SameAs(inferable) => {
                    let to = type_of_inferable(&inferable, hlr);

                    if to.is_unknown() {
                        continue;
                    }

                    let set_type = type_of_inferable_mut(unknown, hlr);
                    *set_type = to;
                },
                Constraint::RelatedTo { spec, gen_params } => {
                    let gen_params = gen_params
                        .into_iter()
                        .map(|inferable| type_of_inferable(&inferable, hlr))
                        .collect::<Vec<_>>();

                    if gen_params.iter().all(|typ| !typ.is_unknown()) {
                        let to = hlr.comp_data.get_spec(&spec, &gen_params).unwrap();

                        let set_type = type_of_inferable_mut(unknown, hlr);
                        *set_type = to;
                    };
                },
                Constraint::Call => {
                    let Inferable::Expr(id) = unknown else { panic!() };
                    let node_data = hlr.tree.get(*id);
                    let func_info = hlr.tree.unique_func_info_of_call(&node_data);

                    let NodeData::Call { a, generics, .. } = hlr.tree.get(*id) else { unreachable!() };

                    if let Some(func_type) = hlr.comp_data.get_type(&func_info) && 
                        generics.iter().all(Type::is_known) {
                        let TypeEnum::Func(FuncType { ret_type, .. }) = 
                            func_type.as_type_enum() else { panic!() };

                        let set_type = type_of_inferable_mut(unknown, hlr);
                        *set_type = ret_type.clone();
                    } else if let Some(decl_info) = hlr.comp_data.get_declaration_of(&func_info)
                    {
                        let code = hlr.comp_data.func_code.get(&decl_info);

                        if let Some(code) = code && code.has_generics() {
                            {
                                let NodeData::Call {
                                    ref mut generics, ..
                                } = hlr.tree.get_mut(*id) else { unreachable!() };
                                generics.resize(code.generic_count as usize, Type::unknown());
                            }

                            let call_generic_inferables = (0..code.generic_count)
                                .map(|index| Inferable::CallGeneric(*id, index as usize))
                                .collect::<Vec<_>>();

                            

                            for (arg_index, arg) in code.args.iter().enumerate() {
                                infer_map.insert(
                                    a[arg_index],
                                    Constraint::RelatedTo {
                                        spec: arg.type_spec.clone(),
                                        gen_params: call_generic_inferables.clone(),
                                    },
                                );
                            }

                            infer_map.insert(
                                *id,
                                Constraint::RelatedTo {
                                    spec: code.ret_type.clone(),
                                    gen_params: call_generic_inferables,
                                },
                            )
                        }
                    }
                },
            }
        }
    }
}
