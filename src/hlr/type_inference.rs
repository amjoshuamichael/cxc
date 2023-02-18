use super::prelude::*;
use crate::{
    lex::indent_parens,
    parse::{InitOpts, Opcode, TypeSpec},
    FuncType, Type, TypeEnum, TypeRelation, VarName,
};
use indexmap::IndexMap;
use indexmap::IndexSet;
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
struct InferMap {
    inferables: IndexMap<Inferable, IndexSet<Constraint>>,
    calls: IndexSet<Inferable>,
}

impl InferMap {
    pub fn insert(&mut self, inferable: impl Into<Inferable>, constraint: Constraint) {
        let inferable = inferable.into();

        match self.inferables.get_mut(&inferable) {
            Some(ref mut constraints) => {
                constraints.insert(constraint);
            },
            None => {
                self.inferables
                    .insert(inferable, [constraint].into_iter().collect());
            },
        }
    }

    pub fn iter(&self) -> indexmap::map::Iter<Inferable, IndexSet<Constraint>> {
        self.inferables.iter()
    }

    pub fn add_call(&mut self, inferable: impl Into<Inferable>) {
        self.calls.insert(inferable.into());
    }

    pub fn remove_call(&mut self, inferable: &Inferable) { self.calls.remove(inferable); }

    pub fn calls(&self) -> indexmap::set::Iter<Inferable> { self.calls.iter() }

    pub fn all_types_known(&self, hlr: &FuncRep) -> bool {
        self.inferables
            .keys()
            .all(|inferable| type_of_inferable(inferable, hlr).is_known())
            && self.calls.len() == 0
    }
}

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
enum Constraint {
    IsType(Type),
    RelatedTo {
        spec: TypeSpec,
        gen_params: Vec<Inferable>,
        method_of: Option<Inferable>,
    },
    SameAs(Inferable),
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

fn set_inferable(inferable: &Inferable, to: &Type, hlr: &mut FuncRep) {
    use Inferable::*;

    let to = to.clone();

    match inferable {
        Expr(id) => {
            if let Some(typ) = hlr.tree.get_mut(*id).ret_type_mut() {
                *typ = to
            }
        },
        Var(name) => hlr.data_flow.get_mut(name).unwrap().typ = to,
        Relation(id) => {
            let NodeData::Call { ref mut relation, .. } = hlr.tree.get_mut(*id) else { panic!() };
            *relation.inner_type_mut().unwrap() = to
        },
        CallGeneric(id, index) => {
            let NodeData::Call { ref mut generics, .. } = hlr.tree.get_mut(*id) else { panic!() };
            generics[*index as usize] = to
        },
        ReturnType => hlr.ret_type = to,
    }
}

pub fn infer_types(hlr: &mut FuncRep) {
    let mut infer_map = InferMap::default();

    // TODO: separate this out into its own function
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
                        method_of: None,
                    },
                    Opcode::Deref => Constraint::RelatedTo {
                        spec: TypeSpec::Deref(box TypeSpec::GenParam(0)),
                        gen_params: vec![hs.into()],
                        method_of: None,
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
                        let last_arg = *a.first().unwrap();
                        infer_map.insert(
                            Inferable::Relation(id),
                            Constraint::SameAs(last_arg.into()),
                        );
                        infer_map.add_call(id);
                    },
                    TypeRelation::Static(_) | TypeRelation::Unrelated => {
                        infer_map.add_call(id);
                    },
                };
            },
            NodeData::Member { object, field, .. } => {
                infer_map.insert(
                    id,
                    Constraint::RelatedTo {
                        spec: TypeSpec::StructMember(box TypeSpec::GenParam(0), field.clone()),
                        gen_params: vec![object.into()],
                        method_of: None,
                    },
                );
            },
            NodeData::FirstClassCall { f, .. } => {
                infer_map.insert(
                    id,
                    Constraint::RelatedTo {
                        spec: TypeSpec::FuncReturnType(box TypeSpec::GenParam(0)),
                        gen_params: vec![f.into()],
                        method_of: None,
                    },
                );
            },
            NodeData::Block { stmts, .. } => {
                if let Some(&last_stmt) = stmts.last() {
                    infer_map.insert(id, Constraint::SameAs(last_stmt.into()));
                } else {
                    infer_map.insert(id, Constraint::IsType(Type::void()));
                }
            },
            NodeData::Return { to_return, .. } => {
                if let Some(to_return) = to_return {
                    infer_map.insert(id, Constraint::SameAs(to_return.into()));
                } else {
                    infer_map.insert(id, Constraint::IsType(Type::void()));
                }
            },
            NodeData::StructLit {
                var_type,
                fields,
                initialize,
            } => {
                if var_type.is_unknown() && initialize == InitOpts::NoFill {
                    let infer_fields = fields
                        .iter()
                        .enumerate()
                        .map(|(index, (field_name, _))| {
                            (field_name.clone(), TypeSpec::GenParam(index as u8))
                        })
                        .collect();

                    let spec = TypeSpec::Struct(infer_fields);

                    infer_map.insert(
                        id,
                        Constraint::RelatedTo {
                            spec,
                            gen_params: fields.iter().map(|(_, id)| (*id).into()).collect(),
                            method_of: None,
                        },
                    );
                }
            },
            NodeData::ArrayLit {
                var_type,
                parts,
                initialize,
            } => {
                if var_type.is_unknown() && initialize == InitOpts::NoFill {
                    let first_element = *parts.first().unwrap();
                    infer_map.insert(
                        id,
                        Constraint::RelatedTo {
                            spec: TypeSpec::Array(
                                box TypeSpec::GenParam(0),
                                parts.len() as u32,
                            ),
                            gen_params: vec![first_element.into()],
                            method_of: None,
                        },
                    );
                }
            },
            NodeData::Index { object, .. } => infer_map.insert(
                id,
                Constraint::RelatedTo {
                    spec: TypeSpec::ArrayElem(box TypeSpec::GenParam(0)),
                    gen_params: vec![object.into()],
                    method_of: None,
                },
            ),
            NodeData::MakeVar {
                ref name,
                rhs,
                var_type,
            } => {
                if var_type.is_known() {
                    infer_map.insert(name, Constraint::IsType(var_type.clone()));
                    infer_map.insert(rhs, Constraint::IsType(var_type.clone()));
                    infer_map.insert(id, Constraint::IsType(Type::void()));
                } else {
                    infer_map.insert(name, Constraint::SameAs(rhs.into()));
                    infer_map.insert(id, Constraint::IsType(Type::void()));
                }
            },
            NodeData::Set { lhs, rhs, .. } => {
                infer_map.insert(id, Constraint::IsType(Type::void()));
                infer_map.insert(lhs, Constraint::SameAs(rhs.into()));
            },
            _ => {},
        }
    }

    infer_map.insert(Inferable::ReturnType, Constraint::SameAs(hlr.tree.root.into()));

    for _ in 0..12 {
        type_solving_round(&mut infer_map, hlr);
        infer_calls(&mut infer_map, hlr);

        introduce_reverse_constraints(&mut infer_map, hlr);

        if infer_map.all_types_known(hlr) {
            return;
        }
    }

    if crate::XC_DEBUG {
        println!("{}", indent_parens(format!("{infer_map:?}").replace("\n", "")));
        println!("{:?}", &hlr.tree);
        println!("{}", &hlr.tree.to_string());

        println!("---UNKNOWNS---");
        for inferable in infer_map
            .inferables
            .iter()
            .filter(|(inferable, _)| type_of_inferable(inferable, hlr).is_unknown())
        {
            println!("{:?}", inferable);
        }

        println!("---UKNOWN CALLS---");
        for call in &infer_map.calls {
            let Inferable::Expr(call_id) = call else { panic!() };
            let call_data = hlr.tree.get(*call_id);
            let unique_info = hlr.tree.unique_func_info_of_call(&call_data);
            println!("{unique_info:?}");
        }
    }

    panic!("type inference failed");
}

fn spec_from_perspective_of_generic(spec: &TypeSpec, generic_index: u32) -> Option<TypeSpec> {
    // this algorithm works like an algebraic math problem.
    use TypeSpec::*;

    let gen_param_spec = GenParam(generic_index as u8);

    let mut lhs = GenParam(0);
    let mut rhs = spec.clone();

    while &rhs != &gen_param_spec {
        match &rhs {
            GenParam(_) => return None, // gen param is not equal to the one we're looking for
            Ref(elem) | Deref(elem) | Array(elem, _) => {
                lhs = match rhs {
                    Ref(_) => Deref(box lhs),
                    Deref(_) => Ref(box lhs),
                    Array(..) => ArrayElem(box lhs),
                    _ => unreachable!(),
                };

                rhs = *elem.clone();
            },
            Generic(_, generics) => {
                let mut found_generic = false;

                for (index, generic) in generics.iter().enumerate() {
                    if spec_from_perspective_of_generic(generic, generic_index).is_some() {
                        lhs = GetGeneric(box lhs, index as u32);
                        rhs = generic.clone();

                        found_generic = true;

                        break;
                    }
                }

                if !found_generic {
                    return None;
                }
            },
            _ => return None,
        }
    }

    Some(lhs)
}

fn introduce_reverse_constraints(infer_map: &mut InferMap, hlr: &FuncRep) {
    for (original, constraints) in infer_map.clone().iter() {
        for constraint in constraints {
            match constraint {
                Constraint::IsType(_) => {},
                Constraint::SameAs(other) => {
                    infer_map.insert(other.clone(), Constraint::SameAs(original.clone()));
                },
                Constraint::RelatedTo {
                    spec,
                    gen_params,
                    method_of,
                } => {
                    // TODO: do something with method_of
                    for (param_index, param) in gen_params.iter().enumerate() {
                        if !type_of_inferable(param, hlr).is_unknown() {
                            continue;
                        }

                        let reversed_spec =
                            spec_from_perspective_of_generic(&spec, param_index as u32);

                        if reversed_spec.is_none() {
                            continue;
                        }

                        infer_map.insert(
                            param.clone(),
                            Constraint::RelatedTo {
                                spec: reversed_spec.unwrap(),
                                gen_params: vec![original.clone()],
                                method_of: method_of.clone(),
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
            match constraint {
                Constraint::IsType(typ) => {
                    if !Type::are_subtypes(typ, &type_of_inferable(&unknown, hlr)) {
                        set_inferable(unknown, typ, hlr);
                    }
                },
                Constraint::SameAs(inferable) => {
                    let to = type_of_inferable(&inferable, hlr);

                    if to.is_unknown() {
                        continue;
                    }

                    set_inferable(unknown, &to, hlr);
                },
                Constraint::RelatedTo {
                    spec,
                    gen_params,
                    method_of,
                } => {
                    let gen_params = gen_params
                        .into_iter()
                        .map(|inferable| type_of_inferable(&inferable, hlr))
                        .collect::<Vec<_>>();

                    let method_of = method_of
                        .as_ref()
                        .map(|inferable| type_of_inferable(&inferable, hlr));

                    if gen_params.iter().all(Type::is_known)
                        && (method_of.is_none() || method_of.as_ref().unwrap().is_known())
                    {
                        let to = hlr
                            .comp_data
                            .get_spec(&spec, &(gen_params, method_of))
                            .unwrap();

                        set_inferable(unknown, &to, hlr);
                    };
                },
            }
        }
    }
}

fn infer_calls(infer_map: &mut InferMap, hlr: &mut FuncRep) {
    for unknown in infer_map.clone().calls() {
        let Inferable::Expr(id) = unknown else { panic!() };
        let node_data = hlr.tree.get(*id);
        let func_info = hlr.tree.unique_func_info_of_call(&node_data);

        let NodeData::Call { a, generics, .. } = 
                        hlr.tree.get(*id) else { unreachable!() };

        if let Ok(func_type) = hlr.comp_data.get_func_type(&func_info) && 
            generics.iter().all(Type::is_known) {

            let TypeEnum::Func(FuncType { ret: ret_type, .. }) = 
                func_type.as_type_enum() else { panic!() };

            infer_map
                .remove_call(unknown);

            set_inferable(unknown, ret_type, hlr);
        } else if let Some(decl_info) = hlr.comp_data.get_declaration_of(&func_info)
        {
            infer_map
                .remove_call(unknown);
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

                let relation_inferable = if code.relation.inner_type().is_some() {
                    Some(Inferable::Relation(*id))
                } else {
                    None
                };

                for (arg_index, arg) in code.args.iter().enumerate() {
                    infer_map.insert(
                        a[arg_index],
                        Constraint::RelatedTo {
                            spec: arg.type_spec.clone(),
                            gen_params: call_generic_inferables.clone(),
                            method_of: relation_inferable.clone(),
                        },
                    );
                }

                infer_map.insert(
                    *id,
                    Constraint::RelatedTo {
                        spec: code.ret_type.clone(),
                        gen_params: call_generic_inferables.clone(),
                        method_of: relation_inferable.clone(),
                    },
                );

                if let Some(relation) = code.relation.inner_type() {
                    infer_map.insert(
                        Inferable::Relation(*id),
                        Constraint::RelatedTo {
                            spec: relation,
                            gen_params: call_generic_inferables,
                            method_of: relation_inferable,
                        },
                    );   
                }
            }
        }
    }
}
