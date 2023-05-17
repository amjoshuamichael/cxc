use super::prelude::*;
use crate::{
    lex::indent_parens,
    parse::{InitOpts, Opcode, TypeSpec},
    Type, TypeRelation, UniqueFuncInfo, VarName,
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

impl From<&VarName> for Inferable {
    fn from(var: &VarName) -> Inferable { Inferable::Var(var.clone()) }
}

impl From<ExprID> for Inferable {
    fn from(id: ExprID) -> Inferable { Inferable::Expr(id) }
}

#[derive(Default, Clone, Debug)]
struct InferMap {
    inferables: IndexMap<Inferable, IndexSet<Constraint>>,
    calls: IndexSet<ExprID>,
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

    pub fn add_call(&mut self, inferable: ExprID) { self.calls.insert(inferable); }

    pub fn remove_call(&mut self, inferable: &ExprID) { self.calls.remove(inferable); }

    pub fn calls(&self) -> indexmap::set::Iter<ExprID> { self.calls.iter() }

    pub fn all_types_known(&self, hlr: &FuncRep) -> bool {
        self.inferables
            .keys()
            .all(|inferable| type_of_inferable(inferable, hlr).is_known())
            && self.calls.is_empty()
            && hlr.tree.ids_in_order().iter().all(|id| {
                let node = hlr.tree.get_ref(*id);
                node.ret_type().is_known()
            })
    }
}

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
enum Constraint {
    IsType(Type),
    RelatedTo {
        spec: TypeSpec,
        gen_params: Vec<Inferable>,
        // Some functions use the Me keyword to reference the type that they are a
        // method of. This is that type, and it is used when doing type solving on
        // arguments and return types.
        method_of: Option<Inferable>,
    },
    SameAs(Inferable),
}

fn type_of_inferable(inferable: &Inferable, hlr: &FuncRep) -> Type {
    use Inferable::*;

    match inferable {
        Expr(id) => hlr.tree.get(*id).ret_type(),
        Var(name) => hlr.variables.get(name).unwrap().typ.clone(),
        Relation(id) => {
            let HNodeData::Call { relation, .. } = hlr.tree.get(*id) else { panic!() };
            relation.inner_type().unwrap()
        },
        CallGeneric(id, index) => {
            let HNodeData::Call { generics, .. } = hlr.tree.get(*id) else { panic!() };
            generics[*index].clone()
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
        Var(name) => hlr.variables.get_mut(name).unwrap().typ = to,
        Relation(id) => {
            let HNodeData::Call { ref mut relation, .. } = hlr.tree.get_mut(*id) else { panic!() };
            *relation.inner_type_mut().unwrap() = to
        },
        CallGeneric(id, index) => {
            let HNodeData::Call { ref mut generics, .. } = hlr.tree.get_mut(*id) else { panic!() };
            generics[*index] = to
        },
        ReturnType => hlr.ret_type = to,
    }
}

pub fn infer_types(hlr: &mut FuncRep) {
    let mut infer_map = InferMap::default();

    setup_initial_constraints(hlr, &mut infer_map); 

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
        println!("{}", indent_parens(format!("{infer_map:?}").replace('\n', "")));
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

        for id in hlr.tree.ids_in_order() {
            if hlr.tree.get(id).ret_type().is_unknown() {
                println!("{:?}", hlr.tree.get(id));
            }
        }

        println!("---UKNOWN CALLS---");
        for call_id in &infer_map.calls {
            let call_data = hlr.tree.get(*call_id);
            let unique_info = hlr.tree.unique_func_info_of_call(&call_data);
            println!("{unique_info:?}");
        }
    }

    panic!("type inference failed");
}

fn setup_initial_constraints(hlr: &mut FuncRep, infer_map: &mut InferMap) {
    for (var, info) in &hlr.variables {
        infer_map.insert(Inferable::Var(var.clone()), Constraint::IsType(info.typ.clone()));
    }

    for id in hlr.tree.ids_in_order().into_iter().rev() {
        let node_data = hlr.tree.get(id);

        if node_data.ret_type().is_known() && !matches!(node_data, HNodeData::Set { .. })
        {
            continue;
        }

        match node_data {
            HNodeData::Ident { ref name, .. } => {
                infer_map.insert(id, Constraint::SameAs(name.into()));
            },
            HNodeData::BinOp { lhs, .. } => {
                infer_map.insert(id, Constraint::SameAs(lhs.into()));
            },
            HNodeData::UnarOp { op, hs, .. } => {
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
            HNodeData::Call {
                ref relation,
                ..
            } => {
                match relation {
                    TypeRelation::MethodOf(_) => {
                        infer_map.add_call(id);
                    },
                    TypeRelation::Static(_) | TypeRelation::Unrelated => {
                        infer_map.add_call(id);
                    },
                };
            },
            HNodeData::Member { object, field, .. } => {
                infer_map.insert(
                    id,
                    Constraint::RelatedTo {
                        spec: TypeSpec::StructMember(box TypeSpec::GenParam(0), field.clone()),
                        gen_params: vec![object.into()],
                        method_of: None,
                    },
                );
            },
            HNodeData::IndirectCall { f, .. } => {
                infer_map.insert(
                    id,
                    Constraint::RelatedTo {
                        spec: TypeSpec::FuncReturnType(box TypeSpec::GenParam(0)),
                        gen_params: vec![f.into()],
                        method_of: None,
                    },
                );
            },
            HNodeData::Block { stmts, .. } => {
                if let Some(&last_stmt) = stmts.last() {
                    infer_map.insert(id, Constraint::SameAs(last_stmt.into()));
                } else {
                    infer_map.insert(id, Constraint::IsType(Type::void()));
                }
            },
            HNodeData::Return { to_return, .. } => {
                if let Some(to_return) = to_return {
                    infer_map.insert(id, Constraint::SameAs(to_return.into()));
                } else {
                    infer_map.insert(id, Constraint::IsType(Type::void()));
                }
            },
            HNodeData::StructLit {
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
            HNodeData::ArrayLit {
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
            HNodeData::Index { object, .. } => infer_map.insert(
                id,
                Constraint::RelatedTo {
                    spec: TypeSpec::ArrayElem(box TypeSpec::GenParam(0)),
                    gen_params: vec![object.into()],
                    method_of: None,
                },
            ),
            HNodeData::Set { lhs, rhs, .. } => {
                infer_map.insert(lhs, Constraint::SameAs(rhs.into()));
            },
            _ => {},
        }
    }
}

fn spec_from_perspective_of_generic(spec: &TypeSpec, generic_index: u32) -> Option<TypeSpec> {
    // this algorithm works like an algebraic math problem.
    use TypeSpec::*;

    let gen_param_spec = GenParam(generic_index as u8);

    let mut lhs = GenParam(0);
    let mut rhs = spec.clone();

    while rhs != gen_param_spec {
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
            Struct(fields) => {
                let mut found_field = false;

                for (field_name, field_spec) in fields.iter() {
                    if spec_from_perspective_of_generic(field_spec, generic_index).is_some() {
                        lhs = StructMember(box lhs, field_name.clone());
                        rhs = field_spec.clone();

                        found_field = true;

                        break;
                    }
                }

                if !found_field {
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
                    for (param_index, param) in gen_params.iter().enumerate() {
                        if !type_of_inferable(param, hlr).is_unknown() {
                            continue;
                        }

                        let reversed_spec =
                            spec_from_perspective_of_generic(spec, param_index as u32);

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
                    if !Type::are_subtypes(typ, &type_of_inferable(unknown, hlr)) {
                        set_inferable(unknown, typ, hlr);
                    }
                },
                Constraint::SameAs(inferable) => {
                    let to = type_of_inferable(inferable, hlr);

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
                        .iter()
                        .map(|inferable| type_of_inferable(inferable, hlr))
                        .collect::<Vec<_>>();

                    let method_of = method_of
                        .as_ref()
                        .map(|inferable| type_of_inferable(inferable, hlr));

                    if gen_params.iter().all(Type::is_known)
                        && (method_of.is_none() || method_of.as_ref().unwrap().is_known())
                    {
                        let to = hlr
                            .comp_data
                            .get_spec(spec, &(gen_params, method_of))
                            .unwrap();

                        set_inferable(unknown, &to, hlr);
                    };
                },
            }
        }
    }
}

fn infer_calls(infer_map: &mut InferMap, hlr: &mut FuncRep) {
    for id in infer_map.clone().calls() {
        let node_data = hlr.tree.get(*id);
        let func_info = hlr.tree.unique_func_info_of_call(&node_data);

        if func_info.relation.is_method() {
            let HNodeData::Call { a, .. } = node_data else { unreachable!() };
            let first_arg = hlr.tree.get(*a.first().unwrap());
            let method_of = first_arg.ret_type();

            for deref in method_of.deref_chain(&*hlr.comp_data) {
                let doing_deref = deref != method_of;

                let dereffed_func_info = UniqueFuncInfo {
                    relation: TypeRelation::MethodOf(deref),
                    ..func_info.clone()
                };

                if fill_in_call(infer_map, hlr, dereffed_func_info.clone(), id, doing_deref) {
                    let HNodeData::Call { relation: ref mut og_call_relation, .. } = hlr.tree.get_mut(*id) else { unreachable!() };
                    *og_call_relation = dereffed_func_info.relation;
                    break;
                }
            }
        } else {
            fill_in_call(infer_map, hlr, func_info, id, false);
        }
    }
}

fn fill_in_call(
    infer_map: &mut InferMap,
    hlr: &mut FuncRep,
    func_info: UniqueFuncInfo,
    id: &ExprID,
    doing_deref: bool,
) -> bool {
    if let Ok(func_type) = hlr.comp_data.get_func_type(&func_info) {
        infer_map.remove_call(id);
        set_inferable(&(*id).into(), &func_type.ret, hlr);

        true
    } else if let Some(decl_info) = hlr.comp_data.get_declaration_of(&func_info) {
        // Function has generics that we do not know, but because of the
        // signature of the function declaration (found by the
        // get_declaration_of) method, we can fill in some constraints
        // that will help us find the type

        infer_map.remove_call(id);

        let code = hlr.comp_data.func_code.get(&decl_info).unwrap();

        let HNodeData::Call { a, .. } = hlr.tree.get(*id) else { unreachable!() };

        {
            let HNodeData::Call {
                        ref mut generics, ..
                    } = hlr.tree.get_mut(*id) else { unreachable!() };
            generics.resize(code.generic_count as usize, Type::unknown());
        }

        let call_generic_inferables = (0..code.generic_count)
            .map(|index| Inferable::CallGeneric(*id, index as usize))
            .collect::<Vec<_>>();

        let relation_inferable = code
            .relation
            .inner_type()
            .is_some()
            .then_some(Inferable::Relation(*id));

        for (arg_index, arg) in code.args.iter().enumerate() {
            if doing_deref && arg_index == 0 {
                // First argument is the self argument, which is already
                // dereffed
                continue;
            }

            infer_map.insert(
                a[arg_index], // TODO: if this fails, func has too few / too many args
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

        if !doing_deref && code.relation.is_method() {
            let first_arg = *a.first().unwrap();
            infer_map.insert(Inferable::Relation(*id), Constraint::SameAs(first_arg.into()));
        } else if doing_deref {
            infer_map.insert(
                Inferable::Relation(*id), 
                Constraint::IsType(func_info.relation.inner_type().unwrap())
            );
        }

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

        true
    } else {
        false
    }
}
