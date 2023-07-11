use super::prelude::*;
use crate::{
    parse::{InitOpts, Opcode, TypeSpec},
    Type, TypeRelation, UniqueFuncInfo, VarName,
};
use indexmap::IndexMap;
use indexmap::IndexSet;
use std::hash::Hash;

#[cfg(feature ="xc-debug")]
use crate::lex::indent_parens;

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
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

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct InferableConnection {
    spec: TypeSpec,
    gen_params: Vec<Inferable>,
    // Some functions use the Me keyword to reference the type that they are a
    // method of. This is that type, and it is used when doing type solving on
    // arguments and return types.
    method_of: Option<Inferable>,
}

#[derive(Default, Debug)]
struct Constraints {
    same_as: IndexSet<Inferable>,
    connections: IndexSet<InferableConnection>,
    known: KnownInferableInformation,
    is: Type,
}

#[derive(Default, Debug)]
enum KnownInferableInformation {
    Int,
    Struct {
        has_fields: IndexSet<(VarName, Type)>,
        field_count: Option<usize>,
    },
    #[default]
    None,
}

impl KnownInferableInformation {
    fn has_fields(&mut self) -> &mut IndexSet<(VarName, Type)> {
        self.has_fields_field_count().0
    }

    fn field_count(&mut self) -> &mut Option<usize> {
        self.has_fields_field_count().1
    }

    fn has_fields_field_count(&mut self) -> (&mut IndexSet<(VarName, Type)>, &mut Option<usize>) {
        match self {
            KnownInferableInformation::Struct { has_fields, field_count, .. } => 
                (has_fields, field_count),
            KnownInferableInformation::None => {
                *self = KnownInferableInformation::Struct { 
                    has_fields: IndexSet::new(),
                    field_count: None,
                };

                let KnownInferableInformation::Struct { has_fields, field_count } = self
                    else { unreachable!() };

                (has_fields, field_count)
            }
            KnownInferableInformation::Int => panic!(), // TODO: should be an error
        }
    }
}

#[derive(Default, Debug)]
struct InferMap {
    infers: IndexMap<Inferable, Constraints>,
    calls: IndexSet<ExprID>,
}

impl InferMap {
    pub fn all_types_known(&self, hlr: &FuncRep) -> bool {
        self.infers
            .keys()
            .all(|inferable| type_of_inferable(inferable, hlr).is_known())
            && self.calls.is_empty()
            && hlr.tree.ids_in_order().iter().all(|id| {
                let node = hlr.tree.get_ref(*id);
                node.ret_type().is_known()
            })
    }

    // just a way to check if we've done anything since the last round
    pub fn known_count(&self, hlr: &FuncRep) -> usize {
        self.infers
            .values()
            .map(|Constraints { same_as, connections, known, .. }| 
                 same_as.len() + 
                 connections.len() + 
                 if let KnownInferableInformation::Struct { ref has_fields, .. } = known { 
                     has_fields.len() 
                 } else { 
                     0 
                 }
             ).sum::<usize>() + 
        self.infers.keys().filter(|inferable| type_of_inferable(inferable, hlr).is_known()).count() +
        (hlr.tree.count() - self.calls.len())
    }

    pub fn on(&mut self, inferable: impl Into<Inferable>) -> &mut Constraints {
        let inferable = inferable.into();

        if self.infers.contains_key(&inferable) {
            &mut self.infers[&inferable]
        } else {
            self.infers.insert(inferable, Constraints::default());
            self.infers.last_mut().unwrap().1
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum InferenceSteps {
    Start = 0,
    FillStructLiterals = 1,
    FillIntsAndFloats = 2,
    Failure = 3,
}

pub fn infer_types(hlr: &mut FuncRep) {
    let mut infer_map = InferMap::default();

    setup_initial_constraints(hlr, &mut infer_map); 
    
    let mut previous_known_count = 0;
    let mut step = InferenceSteps::Start;

    for _ in 0.. {
        type_solving_round(&mut infer_map, hlr);
        fill_known_information(&mut infer_map, hlr);
        infer_calls(&mut infer_map, hlr);

        introduce_reverse_constraints(&mut infer_map, hlr);

        if infer_map.all_types_known(hlr) {
            return;
        }

        let current_known_count = infer_map.known_count(hlr);

        if previous_known_count != current_known_count {
            previous_known_count = current_known_count; 
        } else {
            advance_inference_step(&mut step, hlr, &mut infer_map);
            if step == InferenceSteps::Failure {
                break;
            }
        }
    }

    #[cfg(feature = "xc-debug")]
    {
        println!("{}", indent_parens(format!("{infer_map:?}").replace('\n', "")));
        println!("{:?}", &hlr.tree);
        println!("{}", &hlr.tree.to_string());

        println!("---UNKNOWNS---");
        for inferable in infer_map
            .infers
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
        infer_map.on(var).is = info.typ.clone();
    }

    for id in hlr.tree.ids_in_order().into_iter().rev() {
        let node_data = hlr.tree.get(id);

        if node_data.ret_type().is_known() && !matches!(node_data, HNodeData::Set { .. })
        {
            continue;
        }

        match node_data {
            HNodeData::Ident { ref name, .. } => {
                infer_map.on(id).same_as.insert(name.into());
            },
            HNodeData::BinOp { lhs, rhs, op, .. } => {
                if op.is_cmp() {
                    infer_map.on(id).is = Type::bool();
                } else {
                    infer_map.on(id).same_as.insert(lhs.into());
                }

                infer_map.on(lhs).same_as.insert(rhs.into());
            },
            HNodeData::UnarOp { op, hs, .. } => {
                let contraints = infer_map.on(id);

                match op {
                    Opcode::Not => { contraints.same_as.insert(hs.into()); },
                    Opcode::Ref | Opcode::Deref => {
                        let spec = if op == Opcode::Ref {
                            TypeSpec::GenParam(0).get_ref() 
                        } else { 
                            TypeSpec::GenParam(0).get_deref() 
                        };

                        contraints.connections.insert(InferableConnection {
                            spec,
                            gen_params: vec![hs.into()],
                            method_of: None,
                        });
                    },
                    _ => continue,
                }
            },
            HNodeData::Call { .. } => { infer_map.calls.insert(id); },
            HNodeData::Member { object, field, .. } => {
                infer_map.on(id).connections.insert(
                    InferableConnection {
                        spec: TypeSpec::StructMember(
                            Box::new(TypeSpec::GenParam(0)),
                            field.clone()
                        ),
                        gen_params: vec![object.into()],
                        method_of: None,
                    }.into(),
                );
            },
            HNodeData::IndirectCall { f, .. } => {
                infer_map.on(id).connections.insert(
                    InferableConnection {
                        spec: TypeSpec::FuncReturnType(Box::new(TypeSpec::GenParam(0))),
                        gen_params: vec![f.into()],
                        method_of: None,
                    }.into(),
                );
            },
            HNodeData::Block { .. } => { 
                infer_map.on(id).is = Type::void();
            },
            HNodeData::Return { to_return, .. } => {
                infer_map.on(id).is = Type::void();

                if let Some(to_return) = to_return {
                    infer_map.on(to_return).same_as.insert(Inferable::ReturnType);
                }
            },
            HNodeData::StructLit {
                fields,
                initialize,
                ..
            } => {
                let constraints = infer_map.on(id);

                if initialize == InitOpts::NoFill {
                    let infer_fields = fields
                        .iter()
                        .enumerate()
                        .map(|(index, (field_name, _))| {
                            (field_name.clone(), TypeSpec::GenParam(index as u8))
                        })
                        .collect();

                    let spec = TypeSpec::Struct(infer_fields);

                    constraints.connections.insert(
                        InferableConnection {
                            spec,
                            gen_params: fields.iter().map(|(_, id)| (*id).into()).collect(),
                            method_of: None,
                        }
                    );

                    constraints.known = KnownInferableInformation::Struct {
                        has_fields: IndexSet::new(),
                        field_count: Some(fields.len()),
                    }
                } else {
                    constraints.known = KnownInferableInformation::Struct {
                        has_fields: IndexSet::new(),
                        field_count: None,
                    }
                }
            },
            HNodeData::ArrayLit {
                var_type,
                parts,
                initialize,
            } => {
                if var_type.is_unknown() && initialize == InitOpts::NoFill {
                    let first_element = *parts.first().unwrap();
                    infer_map.on(id).connections.insert(
                        InferableConnection {
                            spec: TypeSpec::Array(
                                Box::new(TypeSpec::GenParam(0)),
                                parts.len() as u32,
                            ),
                            gen_params: vec![first_element.into()],
                            method_of: None,
                        },
                    );
                }
            },
            HNodeData::Index { object, .. } => {
                infer_map.on(id).connections.insert(
                    InferableConnection {
                        spec: TypeSpec::ArrayElem(Box::new(TypeSpec::GenParam(0))),
                        gen_params: vec![object.into()],
                        method_of: None,
                    },
                );
            },
            HNodeData::Set { lhs, rhs, .. } => {
                infer_map.on(lhs).same_as.insert(rhs.into());
            },
            _ => {},
        }
    }
}

fn spec_from_perspective_of_generic(spec: &TypeSpec, generic_index: usize) -> Option<TypeSpec> {
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
                    Ref(_) => Deref(Box::new(lhs)),
                    Deref(_) => Ref(Box::new(lhs)),
                    Array(..) => ArrayElem(Box::new(lhs)),
                    _ => unreachable!(),
                };

                rhs = *elem.clone();
            },
            Generic(_, generics) => {
                let mut found_generic = false;

                for (index, generic) in generics.iter().enumerate() {
                    if spec_from_perspective_of_generic(generic, generic_index).is_some() {
                        lhs = GetGeneric(Box::new(lhs), index as u32);
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
                        lhs = StructMember(Box::new(lhs), field_name.clone());
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
    for index in 0..infer_map.infers.len() {
        let (original, _) = infer_map.infers.get_index(index).unwrap();
        let original = original.clone();
        let constraints = infer_map.infers
            .insert(original.clone(), Constraints::default()).unwrap();

        for other in &constraints.same_as {
            infer_map.on(other.clone()).same_as.insert(original.clone());
            infer_map.on(other.clone()).same_as.extend(constraints.same_as.clone());
        }

        for connection in &constraints.connections {
            for (param_index, param) in connection.gen_params.iter().enumerate() {
                if !type_of_inferable(param, hlr).is_unknown() {
                    continue;
                }

                let reversed_spec =
                    spec_from_perspective_of_generic(&connection.spec, param_index);

                if reversed_spec.is_none() {
                    continue;
                }

                infer_map.on(param.clone()).connections.insert(
                    InferableConnection {
                        spec: reversed_spec.unwrap(),
                        gen_params: vec![original.clone()],
                        method_of: connection.method_of.clone(),
                    },
                );
            }
        }

        infer_map.infers.insert(original.clone(), constraints);
    }
}

fn type_solving_round(infer_map: &mut InferMap, hlr: &mut FuncRep) {
    for index in 0..infer_map.infers.len() {
        let (unknown, constraints) = infer_map.infers.get_index_mut(index).unwrap();

        if constraints.is.is_known() {
            set_inferable(unknown, &constraints.is, hlr)
        } else {
            for inferable in &constraints.same_as {
                let to = type_of_inferable(inferable, hlr);

                if to.is_known() {
                    set_inferable(unknown, &to, hlr);
                    break;
                }
            }
        }
        
        for InferableConnection { spec, gen_params, method_of } in &constraints.connections {
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
        }
    }
}

fn fill_known_information(infer_map: &mut InferMap, hlr: &mut FuncRep) {
    for id in hlr.tree.ids_in_order().into_iter() {
        let node_data = hlr.tree.get_ref(id);

        match node_data {
            HNodeData::Member { ret_type, object, field } if ret_type.is_known() => {
                infer_map.on(*object).known.has_fields() 
                    .insert((field.clone(), ret_type.clone()));
            }
            HNodeData::StructLit { fields, .. } => {
                for field in fields {
                    let field_type = type_of_inferable(&field.1.into(), hlr);

                    if field_type.is_unknown() { continue }

                    infer_map
                        .on(id)
                        .known
                        .has_fields()
                        .insert((field.0.clone(), field_type.clone()));
                }
            }
            _ => {}
        }

        if let KnownInferableInformation::Struct { ref has_fields, ref field_count } = 
            infer_map.on(id).known {
            let mut all_fields = has_fields.clone();
            let mut real_field_count = *field_count;

            let same_as = infer_map.on(id).same_as.clone();

            for similarity in same_as.clone() {
                let (similarity_has, similarity_field_count) = 
                    infer_map.on(similarity).known.has_fields_field_count();

                all_fields.extend(similarity_has.iter().cloned());

                if similarity_field_count.is_some() {
                    if real_field_count.is_some() {
                        assert_eq!(real_field_count, *similarity_field_count);
                    } else {
                        real_field_count = *similarity_field_count;
                    }
                }
            }

            *infer_map.on(id).known.has_fields() = all_fields.clone();
            for similarity in same_as {
                *infer_map.on(similarity.clone()).known.has_fields() = all_fields.clone();
                *infer_map.on(similarity).known.field_count() = real_field_count;
            }
        }
    }
}

fn infer_calls(infer_map: &mut InferMap, hlr: &mut FuncRep) {
    for call_index in 0.. {
        let id = match infer_map.calls.get_index(call_index) {
            Some(id) => *id,
            None => break,
        };

        let node_data = hlr.tree.get(id);
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

                if fill_in_call(infer_map, hlr, dereffed_func_info.clone(), &id, doing_deref) {
                    let HNodeData::Call { relation: ref mut og_call_relation, .. } = hlr.tree.get_mut(id) else { unreachable!() };
                    *og_call_relation = dereffed_func_info.relation;
                    break;
                }
            }
        } else {
            fill_in_call(infer_map, hlr, func_info, &id, false);
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
        infer_map.calls.remove(id);
        set_inferable(&(*id).into(), &func_type.ret, hlr);

        let HNodeData::Call { a, .. } = hlr.tree.get_ref(*id) else { unreachable!() };
        let a = a.clone();

        for (a, (arg_type, arg_id)) in func_type.args.iter().zip(a.into_iter()).enumerate() {
            let arg_type = if doing_deref && a == 0 {
                func_info.relation.inner_type().unwrap()
            } else {
                arg_type.clone()
            };

            set_inferable(&arg_id.into(), &arg_type, hlr);
        }

        true
    } else if let Some(decl_info) = hlr.comp_data.get_declaration_of(&func_info) {
        // Function has generics that we do not know, but because of the
        // signature of the function declaration (found by the
        // get_declaration_of) method, we can fill in some constraints
        // that will help us find the type

        infer_map.calls.remove(id);

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

        // TODO: check if func has too few / too many args
        for (arg_index, arg) in code.args.iter().enumerate() {
            if doing_deref && arg_index == 0 {
                // First argument is the self argument, which is already
                // dereffed
                continue;
            }

            infer_map.on(a[arg_index]).connections.insert( 
                InferableConnection {
                    spec: arg.type_spec.clone(),
                    gen_params: call_generic_inferables.clone(),
                    method_of: relation_inferable.clone(),
                },
            );
        }

        infer_map.on(*id).connections.insert(
            InferableConnection {
                spec: code.ret_type.clone(),
                gen_params: call_generic_inferables.clone(),
                method_of: relation_inferable.clone(),
            },
        );

        if !doing_deref && code.relation.is_method() {
            let first_arg = *a.first().unwrap();
            infer_map.on(Inferable::Relation(*id)).same_as.insert(first_arg.into());
        } else if doing_deref {
            infer_map.on(Inferable::Relation(*id)).is = 
                func_info.relation.inner_type().unwrap()
        }

        if let Some(relation) = code.relation.inner_type() {
            infer_map.on(Inferable::Relation(*id)).connections.insert(
                InferableConnection {
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

fn advance_inference_step(
    step: &mut InferenceSteps, 
    hlr: &mut FuncRep, 
    infer_map: &mut InferMap
) {
    *step = match step {
        InferenceSteps::Start => InferenceSteps::FillStructLiterals,
        InferenceSteps::FillStructLiterals => InferenceSteps::FillIntsAndFloats,
        InferenceSteps::FillIntsAndFloats => InferenceSteps::Failure,
        InferenceSteps::Failure => panic!(), // should be caught by infer_types
    };

    if *step >= InferenceSteps::FillStructLiterals {
        hlr.modify_many_infallible(|struct_id, struct_data, _| {
            let HNodeData::StructLit { var_type, .. } = struct_data else { return };

            if var_type.is_known() { return; }

            let KnownInferableInformation::Struct { ref has_fields, field_count, .. } = 
                infer_map.on(struct_id).known else { return };

            if let Some(field_count) = field_count && has_fields.len() != field_count {
                return;
            }

            *var_type = Type::new_struct(has_fields.iter().cloned().collect());
        })
    }

    if *step >= InferenceSteps::FillIntsAndFloats {
        hlr.modify_many_infallible(|_, number_data, _| {
            match number_data {
                HNodeData::Number { lit_type, .. } if lit_type.is_unknown() => {
                    *lit_type = Type::i(32);
                },
                HNodeData::Float { lit_type, .. } if lit_type.is_unknown() => {
                    *lit_type = Type::f(32);
                },
                _ => {}
            };
        })
    }
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


