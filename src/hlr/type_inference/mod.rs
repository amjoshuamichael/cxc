use super::{prelude::*, hlr_data::VariableInfo};
use crate::{
    parse::{InitOpts, Opcode, TypeSpec, VarDecl},
    Type, FuncQuery, VarName, CompData, errors::TResult, typ::could_come_from::Transformation
};
use indexmap::IndexMap;
use std::{hash::Hash, collections::HashMap};

#[cfg(feature ="xc-debug")]
use crate::lex::indent_parens;

mod unique_vec;

use unique_vec::UniqueVec;

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

type InferableIndex = usize;

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct InferableConnection {
    spec: TypeSpec,
    gen_params: Vec<ConstraintId>,
    // Some functions use the Me keyword to reference the type that they are a
    // method of. This is that type, and it is used when doing type solving on
    // arguments and return types.
    method_of: Option<ConstraintId>,
}

#[derive(Default, Debug)]
struct Constraints {
    acts_on: UniqueVec<InferableIndex>,
    connections: UniqueVec<InferableConnection>,
    usages: Usages,
    is: Type,
    last_set_by: LastSetBy,
}

#[derive(Default, Debug)]
enum LastSetBy {
    Connection(usize),
    Known(KnownBy),
    ReturnTypeOfFunction,
    ArgTypeOfFunction(FuncQuery, usize),
    StructLiteral,
    Call,
    #[default]
    None, // TODO: do we need this?
}

#[derive(Debug)]
enum KnownBy {
    IsBlock,
    IsReturn,
    Specified,
    Comparison,
    Condition,
    VarType,
}

#[derive(PartialEq, Eq, Debug, Default)]
struct Usages {
    is_int: Vec<ExprID>,
    is_float: Vec<ExprID>,
    is_struct: Vec<ExprID>,
    has_fields: IndexMap<VarName, Vec<ExprID>>,
    fulfilled: bool,
}

#[derive(Default, Debug)]
struct InferMap {
    constraints: Vec<Constraints>,
    inferables: Vec<Inferable>,
    inferable_index_to_constraint_index: Vec<ConstraintId>,
    calls: UniqueVec<ExprID>,
    has_been_modified_since_last_round: bool,
}

type ConstraintId = usize;

impl InferMap {
    pub fn all_types_known(&self) -> bool {
        self.constraints
            .iter()
            .all(|constraint| constraint.is.is_known())
            && self.calls.is_empty()
    }

    pub fn constraint_of(&mut self, inferable: impl Into<Inferable>) -> &mut Constraints {
        let constraint_index = self.constraint_index_of(inferable);
        &mut self.constraints[constraint_index]
    }

    pub fn constraint_index_of(&self, inferable: impl Into<Inferable>) -> ConstraintId {
        self.constraint_index_of_option(inferable).unwrap()
    }

    pub fn constraint_index_of_option(&self, inferable: impl Into<Inferable>) -> Option<ConstraintId> {
        let inferable = inferable.into();

        let inferable_index = self.inferables.iter().position(|i| i == &inferable)?;

        Some(self.inferable_index_to_constraint_index[inferable_index])
    }

    pub fn register_new_inferable(&mut self, inferable: impl Into<Inferable>) ->
        ConstraintId {
        let inferable = inferable.into();

        self.inferables.push(inferable);

        self.constraints.push({
            let mut new_constraints = Constraints::default();
            new_constraints.acts_on.push(self.inferables.len() - 1);
            new_constraints
        });

        let new_constraint_index = self.constraints.len() - 1;
        self.inferable_index_to_constraint_index.push(new_constraint_index);

        new_constraint_index
    }

    pub fn detach_inferables(
        &mut self, 
        from: impl Into<Inferable>, 
        remove: impl Into<Inferable>
    ) {
        let from = from.into();
        let remove = remove.into();

        let shared_constraint_index = self.constraint_index_of(from);
        assert_eq!(shared_constraint_index, self.constraint_index_of(remove.clone()));

        let shared_constraint = &mut self.constraints[shared_constraint_index];
        let remove_inferable_index = self
            .inferables
            .iter()
            .position(|i| i == &remove)
            .unwrap();
        shared_constraint.acts_on.remove(&remove_inferable_index);

        self.constraints.push(Constraints {
            acts_on: {
                let mut acts_on = UniqueVec::new();
                acts_on.push(remove_inferable_index);
                acts_on
            },
            ..Default::default()
        });
        let new_constaint_id = self.constraints.len() - 1;
        self.inferable_index_to_constraint_index[remove_inferable_index] = new_constaint_id;
    }

    pub fn join_constraints(
        &mut self,
        mut constraints_to_join: Vec<ConstraintId>,
    ) -> ConstraintId {
        // sort indices in reverse order, so that removing them from the constraints
        // vector doesn't lead to any indexing errors
        constraints_to_join.sort_by(|a, b| b.cmp(a));
        let constraints_to_join = constraints_to_join; // remove mutability

        let shared_type = constraints_to_join
            .iter()
            .map(|c| &self.constraints[*c].is)
            .find(|typ| typ.is_known())
            .cloned()
            .unwrap_or_else(|| Type::unknown());

        assert!(
            constraints_to_join
                .iter()
                .map(|c| &self.constraints[*c].is)
                .all(|typ| typ == &shared_type || typ.is_unknown()
             )
        );

        let mut new = Constraints::default();

        for joining_id in &constraints_to_join {
            let joining = std::mem::replace(
                &mut self.constraints[*joining_id],
                Constraints {
                    is: Type::void(),
                    ..Default::default()
                },
            );

            new.acts_on.extend(joining.acts_on.into_iter());
            new.connections.extend(joining.connections.into_iter());
            new.usages.is_int.extend(joining.usages.is_int.into_iter());
            new.usages.is_float.extend(joining.usages.is_float.into_iter());
            new.usages.is_struct.extend(joining.usages.is_struct.into_iter());
            new.usages.has_fields.extend(joining.usages.has_fields.into_iter());
        }

        new.is = shared_type;

        self.constraints.push(new);
        let new_constraint_index = self.constraints.len() - 1;

        for constraint in &mut self.constraints {
            for connection in constraint.connections.iter_mut() {
                for gen_param in &mut connection.gen_params {
                    if constraints_to_join.contains(gen_param) {
                        *gen_param = new_constraint_index;
                    }
                }

                if let Some(method_of) = connection.method_of.as_mut() &&
                    constraints_to_join.contains(method_of) {
                    *method_of = new_constraint_index;
                }
            }
        }

        for constraint_index in &mut self.inferable_index_to_constraint_index {
            if constraints_to_join.contains(&constraint_index) {
                *constraint_index = new_constraint_index;
            }
        }

        new_constraint_index
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum InferenceSteps {
    Start = 0,
    FillStructLiterals = 1,
    FillIntsAndFloats = 2,
    Failure = 3,
}

#[cfg_attr(debug_assertions, inline(never))]
pub fn infer_types(hlr: &mut FuncRep) {
    let mut infer_map = InferMap::default();

    setup_initial_constraints(hlr, &mut infer_map);
   
    let mut step = InferenceSteps::Start;
    let mut x_steps = 0;

    loop {
        match type_solving_round(&mut infer_map, &hlr.comp_data) {
            Err(_) => break,
            _ => {},
        }
        infer_calls(&mut infer_map, hlr).unwrap();

        introduce_reverse_constraints(&mut infer_map);

        if infer_map.all_types_known() {
            set_types_in_hlr(infer_map, hlr);
            return;
        }

        if !infer_map.has_been_modified_since_last_round {
            advance_inference_step(&mut step, &mut infer_map);
            if step == InferenceSteps::Failure {
                x_steps += 1;
            }

            if x_steps == 2 {
                break;
            }
        }

        infer_map.has_been_modified_since_last_round = false;
    }

    #[cfg(feature = "xc-debug")]
    {
        for (c, constraint) in infer_map.constraints.iter().enumerate() {
            println!("{c} {constraint:?}");
        }

        for (i, inferable) in infer_map.inferables.iter().enumerate() {
            println!("{i} {inferable:?}");
        }
        println!("calls: {:?}", &infer_map.calls);
        set_types_in_hlr(infer_map, hlr);
        println!("{:?}", &hlr.tree);
        println!("{}", &hlr.tree.to_string());
    }

    panic!("type inference failed");
}

fn setup_initial_constraints(hlr: &mut FuncRep, infer_map: &mut InferMap) {
    #[derive(Default)]
    struct SimilarityGraph {
        root: Vec<usize>,
        size: Vec<usize>,
        all_inferables: Vec<Inferable>,
        knowns: HashMap<usize, (Type, KnownBy)>,
    }

    impl SimilarityGraph {
        fn find_root(&mut self, v: usize) -> usize {
            if v == self.root[v] {
                return v;
            }

            let root = self.find_root(self.root[v]);
            self.root[v] = root;
            root
        }

        fn join(&mut self, linfer: impl Into<Inferable>, rinfer: impl Into<Inferable>) {
            let l = self.index_of(linfer);
            let r = self.index_of(rinfer);

            let mut l = self.find_root(l);
            let mut r = self.find_root(r);

            if l != r {
                if self.size[l] < self.size[r] {
                    std::mem::swap(&mut l, &mut r);
                }

                self.root[r] = l;
                self.size[l] += self.size[r];
            }
        }

        fn index_of(&mut self, inferable: impl Into<Inferable>) -> usize {
            let inferable = inferable.into();

            if let Some(index) = self.all_inferables.iter().position(|i| *i == inferable) {
                index
            } else {
                self.add_to_inferables_list(inferable)
            }
        }

        fn mark_known(&mut self, set: impl Into<Inferable>, to: Type, known_by: KnownBy) {
            let set = set.into();
            let set_index = self.index_of(set);
            self.knowns.insert(set_index, (to, known_by));
        }

        fn add_to_inferables_list(&mut self, add: impl Into<Inferable>) -> usize {
            let add = add.into();
            assert!(!self.all_inferables.contains(&add));

            self.all_inferables.push(add);

            let new_index = self.all_inferables.len() - 1;
            self.root.push(new_index);
            self.size.push(1);
            new_index
        }

        fn verify_all_roots(&mut self) {
            for r in 0..self.root.len() {
                self.root[r] = self.find_root(r);
            }
        }
    }

    let mut graph = SimilarityGraph::default();

    for (var, info) in &hlr.variables {
        if info.typ.is_known() {
            graph.mark_known(var, info.typ.clone(), KnownBy::VarType);
        }
    }

    if hlr.ret_type.is_known() {
        graph.mark_known(Inferable::ReturnType, hlr.ret_type.clone(), KnownBy::Specified);
    }

    let ids_in_order = hlr.tree.ids_in_order();
    // mark inferables as "the same", for example, both sides of a binary operation *must*
    // be the same. we also mark "known types" here, like how the result of a comparison
    // must be a boolean.
    for id in ids_in_order.iter().copied().rev() {
        let node_data = hlr.tree.get_ref(id);

        match node_data {
            HNodeData::Ident { ref name, .. } => {
                if let Some(global_type) = hlr.comp_data.globals.get(name) {
                    graph.mark_known(name, global_type.clone(), KnownBy::VarType);
                }

                graph.join(id, name)
            }
            HNodeData::BinOp { lhs, rhs, op, .. } => {
                graph.join(*lhs, *rhs);

                if op.is_cmp() {
                    graph.mark_known(id, Type::bool(), KnownBy::Comparison);
                } else {
                    graph.join(id, *lhs);
                }
            },
            HNodeData::UnarOp { op, hs, .. } => {
                if *op == Opcode::Not {
                    graph.join(id, *hs);
                } else {
                    graph.add_to_inferables_list(id);
                }
            },
            HNodeData::Call { query, a, .. } => {
                infer_map.calls.insert(id);
                graph.add_to_inferables_list(id);

                if let Some(rel_type) = query.relation.inner_type() && rel_type.is_known() {
                    graph.mark_known(Inferable::Relation(id), rel_type, KnownBy::Specified);
                } else if query.relation.is_method() {
                    graph.join(a[0], Inferable::Relation(id));
                } else if query.relation.inner_type().is_some() {
                    graph.add_to_inferables_list(Inferable::Relation(id));
                }
            },
            HNodeData::Block { .. } => {
                graph.mark_known(id, Type::void(), KnownBy::IsBlock)
            }
            HNodeData::Return { to_return, .. } => {
                graph.mark_known(id, Type::void(), KnownBy::IsReturn);

                if let Some(to_return) = to_return {
                    graph.join(*to_return, Inferable::ReturnType);
                }
            },
            HNodeData::StructLit { var_type, .. } => { 
                if var_type.is_known() {
                    graph.mark_known(
                        id, 
                        var_type.clone(),
                        KnownBy::Specified,
                    );
                } else {
                    graph.add_to_inferables_list(id);
                }
            }
            HNodeData::ArrayLit { ref parts, .. } => {
                for ab in parts.windows(2) {
                    graph.join(ab[0], ab[1]);
                }
            },
            HNodeData::Set { lhs, rhs, .. } => {
                graph.join(*lhs, *rhs);
            },
            HNodeData::Transform { .. } |
            HNodeData::Member { .. } |
            HNodeData::IndirectCall { .. } |
            HNodeData::Number { .. } |
            HNodeData::Float { .. } |
            HNodeData::Index { .. } |
            HNodeData::Bool { .. } => {
                graph.add_to_inferables_list(id);
            }
            HNodeData::IfThen { i: cond, .. } |
            HNodeData::IfThenElse { i: cond, .. } |
            HNodeData::While { w: cond, .. } => graph.mark_known(
                *cond, 
                Type::bool(), 
                KnownBy::Condition,
            ),
        }

        let node_data_ret_type = node_data.ret_type();
        if node_data_ret_type.is_known() {
            graph.mark_known(id, node_data_ret_type, KnownBy::Specified);
        }
    }

    graph.verify_all_roots();

    let mut root_to_constraint_index = HashMap::<usize, ConstraintId>::new();
    let mut inferable_index_to_constraint_index: Vec::<ConstraintId> =
        vec![Default::default(); graph.all_inferables.len()];
    let mut constraints_set = Vec::<Constraints>::default();

    for (node, root) in graph.root.into_iter().enumerate() {
        let (constraints_id, constraints) =
            if let Some(constraints_id) = root_to_constraint_index.get(&root) {
                (*constraints_id, &mut constraints_set[*constraints_id])
            } else {
                constraints_set.push(Constraints::default());
                let new_index = constraints_set.len() - 1;
                root_to_constraint_index.insert(root, new_index);
                (new_index, &mut constraints_set[new_index])
            };

        if let Some((known_type, known_by)) = graph.knowns.remove(&node) {
            if constraints.is.is_known() && known_type != constraints.is {
                panic!() // TODO: error
            }

            constraints.is = known_type;
            constraints.last_set_by = LastSetBy::Known(known_by);
        }

        inferable_index_to_constraint_index[node] = constraints_id;
        constraints.acts_on.insert(node);
    }

    infer_map.inferable_index_to_constraint_index = inferable_index_to_constraint_index;
    infer_map.inferables = graph.all_inferables;
    infer_map.constraints = constraints_set;

    for id in ids_in_order.into_iter().rev() {
        match hlr.tree.get_ref(id) {
            HNodeData::Number { .. } => {
                infer_map.constraint_of(id).usages.is_int.push(id);
            }
            HNodeData::Float { .. } => {
                infer_map.constraint_of(id).usages.is_float.push(id);
            }
            HNodeData::UnarOp { op, hs, .. } => {
                let spec = match op {
                    Opcode::Ref => TypeSpec::GenParam(0).get_ref(),
                    Opcode::Deref => TypeSpec::GenParam(0).get_deref(),
                    _ => continue,
                };
                        
                let hs_constraint_index = infer_map.constraint_index_of(*hs);
                let constraints = infer_map.constraint_of(id);
                constraints.connections.insert(InferableConnection {
                    spec,
                    gen_params: vec![hs_constraint_index],
                    method_of: None,
                });
            }
            HNodeData::StructLit { fields, .. } => {
                infer_map.constraint_of(id).usages.is_struct.push(id);

                for (field_name, field) in fields {
                    infer_map.constraint_of(id).usages.has_fields
                        .entry(field_name.clone())
                        .or_insert_with(|| Vec::new())
                        .push(*field);
                }
            },
            HNodeData::ArrayLit { parts, initialize, .. } => {
                let first_element = *parts.first().unwrap();
                let parts_constraint_index = infer_map.constraint_index_of(first_element);

                if *initialize == InitOpts::NoFill {
                    infer_map.constraint_of(id).connections.insert(
                        InferableConnection {
                            spec: TypeSpec::Array(
                                Box::new(TypeSpec::GenParam(0)),
                                parts.len() as u32,
                            ),
                            gen_params: vec![parts_constraint_index],
                            method_of: None,
                        },
                    );
                } else {
                    let array_constraint_index = infer_map.constraint_index_of(id);

                    infer_map.constraints[parts_constraint_index].connections.insert(
                        InferableConnection {
                            spec: TypeSpec::ArrayElem(Box::new(TypeSpec::GenParam(0))),
                            gen_params: vec![array_constraint_index],
                            method_of: None,
                        }
                    );
                }
            },
            HNodeData::IndirectCall { f, a: args, .. } => {
                let f_constraint_index = infer_map.constraint_index_of(*f);

                infer_map.constraint_of(id).connections.insert(
                    InferableConnection {
                        spec: TypeSpec::FuncReturnType(Box::new(TypeSpec::GenParam(0))),
                        gen_params: vec![f_constraint_index],
                        method_of: None,
                    }.into(),
                );

                for (a, arg) in args.iter().enumerate() {
                    infer_map.constraint_of(*arg).connections.insert(
                        InferableConnection {
                            spec: TypeSpec::FuncArgType(Box::new(TypeSpec::GenParam(0)), a),
                            gen_params: vec![f_constraint_index],
                            method_of: None,
                        }.into(),
                    );
                }
            },
            HNodeData::Member { object, field, .. } => {
                let object_constraint_index =
                    infer_map.constraint_index_of(*object);

                let member_constraint_index = infer_map.constraint_index_of(id);
                infer_map.constraints[member_constraint_index].connections.insert(
                    InferableConnection {
                        spec: TypeSpec::StructMember(
                            Box::new(TypeSpec::GenParam(0)),
                            field.clone()
                        ),
                        gen_params: vec![object_constraint_index],
                        method_of: None,
                    }.into(),
                );

                infer_map.constraints[object_constraint_index].usages.has_fields
                    .entry(field.clone())
                    .or_insert_with(|| Vec::new())
                    .push(id);
            },
            HNodeData::Index { object, .. } => {
                let object_constraint_index = infer_map.constraint_index_of(*object);

                infer_map.constraint_of(id).connections.insert(
                    InferableConnection {
                        spec: TypeSpec::ArrayElem(Box::new(TypeSpec::GenParam(0))),
                        gen_params: vec![object_constraint_index],
                        method_of: None,
                    },
                );
            },
            HNodeData::Bool { .. } |
            HNodeData::Ident { .. } | 
            HNodeData::Set { .. } | 
            HNodeData::Call { .. } |
            HNodeData::BinOp { .. } | 
            HNodeData::Transform { .. } | 
            HNodeData::IfThen { .. } |
            HNodeData::IfThenElse { .. } | 
            HNodeData::While { .. } |
            HNodeData::Block { .. } | HNodeData::Return { .. } => {},
        }
    }
}

fn spec_from_perspective_of_generic(
    spec: &TypeSpec,
    generic_index: usize
) -> Option<TypeSpec> {
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

                for (_, field_name, field_spec) in fields {
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

fn introduce_reverse_constraints(infer_map: &mut InferMap) {
    for constraint_index in 0..infer_map.constraints.len() {
        let constraints = std::mem::replace(
            &mut infer_map.constraints[constraint_index],
            Constraints::default()
        );

        for connection in &constraints.connections {
            for (param_index, param_constraint_index) in
                connection.gen_params.iter().copied().enumerate() {
                let param_constraint = &infer_map.constraints[param_constraint_index];
                if param_constraint.is.is_known() {
                    continue;
                }

                let reversed_spec =
                    spec_from_perspective_of_generic(&connection.spec, param_index);

                if reversed_spec.is_none() {
                    continue;
                }

                infer_map.constraints[param_constraint_index].connections.insert(
                    InferableConnection {
                        spec: reversed_spec.unwrap(),
                        gen_params: vec![constraint_index],
                        method_of: connection.method_of.clone(),
                    },
                );
            }
        }

        infer_map.constraints[constraint_index] = constraints;
    }
}

fn type_solving_round(infer_map: &mut InferMap, comp_data: &CompData) -> Result<(), ()> {
    for constraint_index in 0..infer_map.constraints.len() {
        let constraints = &infer_map.constraints[constraint_index];

        for (connections_index, InferableConnection { spec, gen_params, method_of }) 
            in constraints.connections.iter().enumerate() {
            let gen_params = gen_params
                .iter()
                .map(|inferable| &infer_map.constraints[*inferable].is)
                .collect::<Vec<&Type>>();

            let method_of = method_of
                .as_ref()
                .map(|inferable| &infer_map.constraints[*inferable].is);

            if gen_params.iter().copied().all(Type::is_known)
                && (method_of.is_none() || method_of.as_ref().unwrap().is_known())
            {
                let found_type = comp_data
                    .get_spec(
                        &spec,
                        &(
                            gen_params.into_iter().cloned().collect::<Vec<Type>>(),
                            method_of.cloned()
                        )
                    )
                    .unwrap(); // TODO: error

                if constraints.is.is_known() { 
                    if constraints.is != found_type {
                        println!("{:?} is not {:?}", constraints.is, found_type);
                        return Err(())
                    }
                } else {
                    infer_map.constraints[constraint_index].is = found_type;
                    infer_map.constraints[constraint_index].last_set_by = 
                        LastSetBy::Connection(connections_index);

                    infer_map.has_been_modified_since_last_round = true;
                }
                
                break;
            };
        }
    }

    Ok(())
}

fn set_types_in_hlr(infer_map: InferMap, hlr: &mut FuncRep) {
    for constraint in &infer_map.constraints {
        for inferable_index in &constraint.acts_on {
            let set = constraint.is.clone();
            match &infer_map.inferables[*inferable_index] {
                Inferable::Expr(id) => {
                    if let Some(typ) = hlr.tree.get_mut(*id).ret_type_mut() {
                        *typ = set;
                    }
                },
                Inferable::Var(name) => {
                    if let Some(VariableInfo { typ, .. }) = hlr.variables.get_mut(name) {
                        *typ = set
                    }
                },
                Inferable::ReturnType => hlr.ret_type = set,
                Inferable::Relation(id) => {
                    let HNodeData::Call { ref mut query, .. } = hlr.tree.get_mut(*id) 
                        else { unreachable!() };
                    *query.relation.inner_type_mut().unwrap() = set;
                },
                Inferable::CallGeneric(id, index) => {
                    let HNodeData::Call { ref mut query, .. } = hlr.tree.get_mut(*id) 
                        else { unreachable!() };
                    query.generics[*index] = set;
                },
            }
        }
    }
}

fn infer_calls(infer_map: &mut InferMap, hlr: &mut FuncRep) -> TResult<()> {
    for call_id in infer_map.calls.clone() {
        let node_data = hlr.tree.get(call_id);
        let HNodeData::Call { query: FuncQuery { name, relation, generics }, .. } = node_data 
            else { unreachable!() };

        let relation_typ = infer_map.constraint_index_of_option(Inferable::Relation(call_id))
            .map(|constraint_index| infer_map.constraints[constraint_index].is.clone());

        let func_query = FuncQuery {
            name: name.clone(),
            relation: relation.map_inner_type(|_| relation_typ.unwrap()),
            generics: generics.clone(),
        };

        if fill_in_call(infer_map, hlr, func_query.clone(), &call_id)? {
            infer_map.calls.remove(&call_id);
            infer_map.has_been_modified_since_last_round = true;
        }
    }

    Ok(())
}

fn fill_in_call(
    infer_map: &mut InferMap,
    hlr: &mut FuncRep,
    func_query: FuncQuery,
    call_id: &ExprID,
) -> TResult<bool> {
    // TODO: this is a hack
    let derive_code_storage;

    let (code, trans) = 
        if let Some((code_id, trans)) = 
            hlr.comp_data.query_for_code_with_transformation(func_query.code_query()) {
            (&hlr.comp_data.func_code[code_id], trans)
        } else if let Some(code) = hlr.comp_data.get_derived_code(func_query.code_query()) {
            derive_code_storage = Some(code);
            (derive_code_storage.as_ref().unwrap(), None)
        } else {
            return Ok(false);
        };

    if func_query.generics.len() == code.generic_count {
        // we don't have to infer any generics.
        hlr.specified_dependencies.insert(func_query.code_query().to_owned_fcq());

        let constraint = &mut infer_map.constraint_of(*call_id);
        constraint.is = hlr.comp_data.get_spec(&code.ret_type, &func_query)?;
        constraint.last_set_by = LastSetBy::ReturnTypeOfFunction;
        
        let HNodeData::Call { a, .. } = hlr.tree.get_ref(*call_id) else { unreachable!() };
        let mut a = a.clone();

        if func_query.relation.is_method() {
            let relation_inferable = Inferable::Relation(*call_id);
            infer_map.detach_inferables(a[0], relation_inferable.clone());

            let transform = hlr.tree.insert(*call_id, HNodeData::Transform {
                hs: a[0],
                steps: trans.map(|trans| trans.steps).unwrap_or_default(),
                ret_type: Type::unknown(),
            });

            a[0] = transform;

            infer_map.inferables.push(transform.into());
            let transform_inferable_index = infer_map.inferables.len() - 1;

            let constraint_index = infer_map.constraint_index_of(relation_inferable);
            let constraint = &mut infer_map.constraints[constraint_index];
            constraint.acts_on.insert(transform_inferable_index);
            infer_map.inferable_index_to_constraint_index.push(constraint_index);
            constraint.is = hlr.comp_data.get_spec(
                &code.relation.inner_type().unwrap(), 
                &func_query,
            )?;
        }

        for (a, (VarDecl { type_spec: arg_type_spec, .. }, arg_id)) in
            code.args.iter().zip(a.iter()).enumerate() {
            let arg_type = hlr.comp_data.get_spec(&arg_type_spec, &func_query)?;

            let constraint = &mut infer_map.constraint_of(*arg_id); 
            constraint.is = arg_type;
            constraint.last_set_by = LastSetBy::ArgTypeOfFunction(func_query.clone(), a);
        }

        let HNodeData::Call { a: old_a, .. } = hlr.tree.get_mut(*call_id) 
            else { unreachable!() };

        *old_a = a;
    } else {
        // Function has generics that we do not know, but because of the
        // signature of the function declaration (found by the
        // get_declaration_of) method, we can fill in some constraints
        // that will help us find the type
        hlr.specified_dependencies.insert(func_query.code_query().to_owned_fcq());

        {
            let HNodeData::Call { ref mut query, .. } = hlr.tree.get_mut(*call_id) 
                else { unreachable!() };

            query.generics.resize(code.generic_count as usize, Type::unknown());
        }

        let call_generic_constraints = (0..code.generic_count)
            .map(|index| {
                let inferable = Inferable::CallGeneric(*call_id, index as usize);
                infer_map.register_new_inferable(inferable)
            })
            .collect::<Vec<_>>();

        if code.relation.is_method() {
            let HNodeData::Call { a, .. } = hlr.tree.get(*call_id) 
                else { unreachable!() };

            let relation_inferable = Inferable::Relation(*call_id);
            infer_map.detach_inferables(a[0], relation_inferable.clone());

            let transform = hlr.tree.insert(*call_id, HNodeData::Transform {
                hs: a[0],
                steps: trans.as_ref().map(|trans| trans.steps.clone()).unwrap_or_default(),
                ret_type: Type::unknown(),
            });

            {
                let HNodeData::Call { ref mut a, .. } = hlr.tree.get_mut(*call_id) 
                    else { unreachable!() };

                a[0] = transform;
            }

            infer_map.inferables.push(transform.into());
            let transform_inferable_index = infer_map.inferables.len() - 1;

            let constraint_index = infer_map.constraint_index_of(relation_inferable);
            let constraint = &mut infer_map.constraints[constraint_index];
            constraint.acts_on.insert(transform_inferable_index);
            infer_map.inferable_index_to_constraint_index.push(constraint_index);

            constraint.connections.insert(
                InferableConnection {
                    spec: code.relation.inner_type().unwrap(),
                    gen_params: call_generic_constraints.clone(),
                    method_of: None,
                }
            );
        }

        let HNodeData::Call { a, .. } = hlr.tree.get(*call_id) else { unreachable!() };

        let relation_constraint = 
            infer_map.constraint_index_of_option(Inferable::Relation(*call_id));

        // TODO: check if func has too few / too many args
        for (arg_index, arg) in code.args.iter().enumerate() {
            infer_map.constraint_of(a[arg_index]).connections.insert(
                InferableConnection {
                    spec: arg.type_spec.clone(),
                    gen_params: call_generic_constraints.clone(),
                    method_of: relation_constraint.clone(),
                },
            );
        }

        infer_map.constraint_of(*call_id).connections.insert(
            InferableConnection {
                spec: code.ret_type.clone(),
                gen_params: call_generic_constraints.clone(),
                method_of: relation_constraint.clone(),
            },
        );

        if let Some(Transformation { generics, .. }) = trans {
            for (g, generic_constraint) in call_generic_constraints.iter().enumerate() {
                infer_map.constraints[*generic_constraint].is = generics[g].clone();
            }
        }
    };

    Ok(true)
}

fn advance_inference_step(
    step: &mut InferenceSteps,
    infer_map: &mut InferMap
) {
    *step = match step {
        InferenceSteps::Start => InferenceSteps::FillStructLiterals,
        InferenceSteps::FillStructLiterals => InferenceSteps::FillIntsAndFloats,
        InferenceSteps::FillIntsAndFloats => InferenceSteps::Failure,
        InferenceSteps::Failure => InferenceSteps::Failure, // should be caught by infer_types
    };

    let mut fulfilled_usages = Vec::<ConstraintId>::new();

    if *step >= InferenceSteps::FillStructLiterals {
        for constraint_index in 0..infer_map.constraints.len() {
            let constraints = &mut infer_map.constraints[constraint_index];

            if constraints.usages.fulfilled { continue }

            if constraints.usages.has_fields.is_empty() && 
                constraints.usages.is_struct.is_empty() {
                continue;
            }

            fulfilled_usages.push(constraint_index);

            let only_set_members = constraints.is.is_known();

            let has_fields = std::mem::replace(
                &mut constraints.usages.has_fields, 
                IndexMap::default()
            );

            for (name, ids) in &has_fields {
                let field_constraint = infer_map.join_constraints(
                    ids.iter().map(|id| infer_map.constraint_index_of(*id)).collect()
                );

                infer_map.constraints[field_constraint].connections.insert(
                    InferableConnection {
                        spec: TypeSpec::StructMember(
                            Box::new(TypeSpec::GenParam(0)), 
                            name.clone()
                        ),
                        gen_params: vec![constraint_index],
                        method_of: None,
                    }
                );
            }
            
            if !only_set_members {
                let mut struct_fields = Vec::<(bool, VarName, TypeSpec)>::new();
                let mut gen_params = Vec::<ConstraintId>::new();

                for (f, (name, ids)) in has_fields.into_iter().enumerate() {
                    let field_constraint_index = infer_map.constraint_index_of(ids[0]);
                    struct_fields.push((false, name.clone(), TypeSpec::GenParam(f as u8)));
                    gen_params.push(field_constraint_index);
                }

                infer_map.constraints[constraint_index].connections.insert(
                    InferableConnection {
                        spec: TypeSpec::Struct(struct_fields),
                        gen_params,
                        method_of: None,
                    }
                );
            }
        }
    }

    if *step >= InferenceSteps::FillIntsAndFloats {
        for (constraint_index, constraints) in infer_map.constraints.iter_mut().enumerate() {
            if constraints.is.is_known() { continue }

            constraints.is = if !constraints.usages.is_int.is_empty() {
                 Type::i(32)
            } else if !constraints.usages.is_int.is_empty() {
                Type::f(32)
            } else { 
                continue;
            };

            fulfilled_usages.push(constraint_index);
        }
    }

    for constraint_id in fulfilled_usages {
        infer_map.constraints[constraint_id].usages.fulfilled = true;
    }
}
