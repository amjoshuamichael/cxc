use super::{prelude::*, hlr_data::VariableInfo};
use crate::{
    parse::{InitOpts, Opcode, TypeSpec, VarDecl, FuncCode},
    Type, FuncQuery, VarName, CompData, errors::TResult, unit::get_type_spec::GenericTable, TypeRelation,
};
use indexmap::{IndexSet, IndexMap};
use std::{hash::Hash, collections::HashMap};

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

type InferableIndex = usize;
type ConstraintIndex = usize;

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct InferableConnection {
    spec: TypeSpec,
    gen_params: Vec<ConstraintIndex>,
    // Some functions use the Me keyword to reference the type that they are a
    // method of. This is that type, and it is used when doing type solving on
    // arguments and return types.
    method_of: Option<ConstraintIndex>,
}

#[derive(Default, Debug)]
struct Constraints {
    acts_on: IndexSet<InferableIndex>,
    connections: IndexSet<InferableConnection>,
    used_as: UsageContext,
    is: Type,
    last_set_by: LastSetBy,
}

#[derive(Default, Debug)]
enum LastSetBy {
    Connection(usize),
    Known,
    ReturnTypeOfFunction,
    ArgTypeOfFunction(FuncQuery, usize),
    StructLiteral,
    Call,
    #[default]
    None,
}

#[derive(Default, Debug)]
enum UsageContext {
    Int,
    Float,
    Struct {
        has_fields: IndexMap<VarName, Type>,
        field_count: Option<usize>,
    },
    #[default]
    None,
}

impl UsageContext {
    fn has_fields(&mut self) -> &mut IndexMap<VarName, Type> {
        self.has_fields_field_count().0
    }

    fn field_count(&mut self) -> &mut Option<usize> {
        self.has_fields_field_count().1
    }

    fn has_fields_field_count(&mut self) -> (&mut IndexMap<VarName, Type>, &mut Option<usize>) {
        match self {
            UsageContext::Struct { has_fields, field_count, .. } =>
                (has_fields, field_count),
            UsageContext::None => {
                *self = UsageContext::Struct {
                    has_fields: IndexMap::new(),
                    field_count: None,
                };

                let UsageContext::Struct { has_fields, field_count } = self
                    else { unreachable!() };

                (has_fields, field_count)
            }
            UsageContext::Int | UsageContext::Float => panic!(), // TODO: should be an error
        }
    }
}

#[derive(Default, Debug)]
struct InferMap {
    constraints: Vec<Constraints>,
    inferables: Vec<Inferable>,
    inferable_index_to_constraint_index: Vec<ConstraintIndex>,
    calls: IndexSet<ExprID>,
}

impl InferMap {
    pub fn all_types_known(&self) -> bool {
        self.constraints
            .iter()
            .all(|constraint| constraint.is.is_known())
            && self.calls.is_empty()
    }

    // just a way to check if we've done anything since the last round
    pub fn known_count(&self, hlr: &FuncRep) -> usize {
        self.constraints
            .iter()
            .map(|Constraints { connections, used_as, is, .. }|
                 connections.len() +
                 if let UsageContext::Struct { ref has_fields, .. } = used_as {
                     has_fields.len()
                 } else {
                     0
                 } +
                 if is.is_known() { 0 } else { 1 }
             ).sum::<usize>() +
        (hlr.tree.count() - self.calls.len())
    }

    pub fn constraint_of(&mut self, inferable: impl Into<Inferable>) -> &mut Constraints {
        let constraint_index = self.constraint_index_of(inferable);
        &mut self.constraints[constraint_index]
    }

    pub fn constraint_index_of(&self, inferable: impl Into<Inferable>) -> ConstraintIndex {
        self.constraint_index_of_option(inferable).unwrap()
    }

    pub fn constraint_index_of_option(&self, inferable: impl Into<Inferable>) -> Option<ConstraintIndex> {
        let inferable = inferable.into();

        let inferable_index = self.inferables.iter().position(|i| i == &inferable)?;

        Some(self.inferable_index_to_constraint_index[inferable_index])
    }

    pub fn register_new_inferable(&mut self, inferable: impl Into<Inferable>) ->
        ConstraintIndex {
        let inferable = inferable.into();

        self.inferables.push(inferable);
        let mut new_constraints = Constraints::default();
        new_constraints.acts_on.insert(self.inferables.len() - 1);
        self.constraints.push(new_constraints);
        self.inferable_index_to_constraint_index.push(self.constraints.len() - 1);

        self.constraints.len() - 1
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
        shared_constraint.acts_on.shift_remove(&remove_inferable_index);

        self.constraints.push(Constraints {
            acts_on: {
                let mut acts_on = IndexSet::new();
                acts_on.insert(remove_inferable_index);
                acts_on
            },
            ..Default::default()
        });

        self.inferable_index_to_constraint_index[remove_inferable_index] = 
            self.constraints.len() - 1;
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
   
    let mut previous_known_count = 0;
    let mut step = InferenceSteps::Start;
    let mut x_steps = 0;

    loop {
        type_solving_round(&mut infer_map, &hlr.comp_data);
        fill_known_information(&mut infer_map, hlr);
        infer_calls(&mut infer_map, hlr).unwrap();

        introduce_reverse_constraints(&mut infer_map);

        if infer_map.all_types_known() {
            set_types_in_hlr(infer_map, hlr);
            return;
        }

        let current_known_count = infer_map.known_count(hlr);

        if previous_known_count != current_known_count {
            previous_known_count = current_known_count;
        } else {
            advance_inference_step(&mut step, &mut infer_map);
            if step == InferenceSteps::Failure {
                x_steps += 1;
            }
            if x_steps == 20 {
                break;
            }
        }
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
        knowns: HashMap<usize, Type>,
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

        fn mark_known(&mut self, set: impl Into<Inferable>, to: Type) {
            let set = set.into();
            let set_index = self.index_of(set);
            self.knowns.insert(set_index, to);
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
            graph.mark_known(var, info.typ.clone());
        }
    }

    if hlr.ret_type.is_known() {
        graph.mark_known(Inferable::ReturnType, hlr.ret_type.clone());
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
                    graph.mark_known(name, global_type.clone());
                }

                graph.join(id, name)
            }
            HNodeData::BinOp { lhs, rhs, op, .. } => {
                graph.join(*lhs, *rhs);

                if op.is_cmp() {
                    graph.mark_known(id, Type::bool());
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
                    graph.mark_known(Inferable::Relation(id), rel_type);
                } else if query.relation.is_method() {
                    graph.join(a[0], Inferable::Relation(id));
                } else if query.relation.inner_type().is_some() {
                    graph.add_to_inferables_list(Inferable::Relation(id));
                }
            },
            HNodeData::Block { .. } => {
                graph.mark_known(id, Type::void())
            }
            HNodeData::Return { to_return, .. } => {
                graph.mark_known(id, Type::void());

                if let Some(to_return) = to_return {
                    graph.join(*to_return, Inferable::ReturnType);
                }
            },
            HNodeData::StructLit { var_type, .. } => { 
                if var_type.is_known() {
                    graph.mark_known(id, var_type.clone());
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
            HNodeData::While { w: cond, .. } => graph.mark_known(*cond, Type::bool()),
        }

        let node_data_ret_type = node_data.ret_type();
        if node_data_ret_type.is_known() {
            graph.mark_known(id, node_data_ret_type);
        }
    }

    graph.verify_all_roots();

    let mut parent_to_constraint_index = HashMap::<usize, ConstraintIndex>::new();
    let mut inferable_index_to_constraint_index: Vec::<ConstraintIndex> =
        vec![0; graph.all_inferables.len()];
    let mut constraints_set = Vec::<Constraints>::new();

    for (child, parent) in graph.root.into_iter().enumerate() {
        let (constraints_index, constraints) =
            if let Some(constraints_index) = parent_to_constraint_index.get(&parent) {
                (*constraints_index, &mut constraints_set[*constraints_index])
            } else {
                parent_to_constraint_index.insert(parent, constraints_set.len());
                constraints_set.push(Constraints::default());
                (constraints_set.len() - 1, constraints_set.last_mut().unwrap())
            };

        if let Some(known_type) = graph.knowns.remove(&child) {
            if constraints.is.is_known() && known_type != constraints.is {
                panic!() // TODO: error
            }

            constraints.is = known_type;
            constraints.last_set_by = LastSetBy::Known;
        }

        inferable_index_to_constraint_index[child] = constraints_index;
        constraints.acts_on.insert(child);
    }

    infer_map.inferable_index_to_constraint_index = inferable_index_to_constraint_index;
    infer_map.inferables = graph.all_inferables;
    infer_map.constraints = constraints_set;

    for id in ids_in_order.into_iter().rev() {
        match hlr.tree.get_ref(id) {
            HNodeData::Number { .. } => {
                infer_map.constraint_of(id).used_as = UsageContext::Int;
            }
            HNodeData::Float { .. } => {
                infer_map.constraint_of(id).used_as = UsageContext::Float;
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
            HNodeData::StructLit { var_type, fields, initialize, .. } => {
                if var_type.is_unknown() && *initialize == InitOpts::NoFill {
                    let infer_fields = fields
                        .iter()
                        .enumerate()
                        .map(|(index, (field_name, _))| {
                            (field_name.clone(), TypeSpec::GenParam(index as u8))
                        })
                        .collect();

                    let spec = TypeSpec::Struct(infer_fields);

                    let gen_params = fields
                        .iter()
                        .map(|(_, id)| {
                            infer_map.constraint_index_of(*id)
                        }).collect();

                    let constraints = infer_map.constraint_of(id);
                    constraints.connections.insert(
                        InferableConnection {
                            spec,
                            gen_params,
                            method_of: None,
                        }
                    );

                    *constraints.used_as.field_count() = Some(fields.len());
                } else {
                    infer_map.constraint_of(id).used_as.has_fields();
                }
            },
            HNodeData::ArrayLit { parts, initialize, .. } => {
                if *initialize == InitOpts::NoFill {
                    let first_element = *parts.first().unwrap();
                    let parts_constraint_index =
                        infer_map.constraint_index_of(first_element);

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
                }
            },
            HNodeData::IndirectCall { f, .. } => {
                let f_constraint_index = infer_map.constraint_index_of(*f);

                infer_map.constraint_of(id).connections.insert(
                    InferableConnection {
                        spec: TypeSpec::FuncReturnType(Box::new(TypeSpec::GenParam(0))),
                        gen_params: vec![f_constraint_index],
                        method_of: None,
                    }.into(),
                );
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

                infer_map.constraints[object_constraint_index].used_as.has_fields().insert(
                    field.clone(), Type::unknown()
                );
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
            HNodeData::Ident { .. } | HNodeData::Set { .. } | HNodeData::Call { .. } |
            HNodeData::BinOp { .. } | HNodeData::IfThen { .. } |
            HNodeData::IfThenElse { .. } | HNodeData::While { .. } |
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

fn type_solving_round(infer_map: &mut InferMap, comp_data: &CompData) {
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
                        spec,
                        &(
                            gen_params.into_iter().cloned().collect::<Vec<Type>>(),
                            method_of.cloned()
                        )
                    )
                    .unwrap(); // TODO: throw error

                infer_map.constraints[constraint_index].is = found_type;
                infer_map.constraints[constraint_index].last_set_by = 
                    LastSetBy::Connection(connections_index);
                break;
            };
        }
    }
}

fn set_types_in_hlr(infer_map: InferMap, hlr: &mut FuncRep) {
    for constraint in infer_map.constraints {
        for inferable_index in constraint.acts_on {
            let set = constraint.is.clone();
            match &infer_map.inferables[inferable_index] {
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

fn fill_known_information(infer_map: &mut InferMap, hlr: &mut FuncRep) {
    for id in hlr.tree.ids_unordered() {
        let node_data = hlr.tree.get_ref(id);

        match node_data {
            HNodeData::Member { object, field, .. } => {
                let member_type = infer_map.constraint_of(id).is.clone();

                if member_type.is_known() {
                    infer_map.constraint_of(*object).used_as.has_fields()
                        .insert(field.clone(), member_type);
                }
            }
            HNodeData::StructLit { fields, .. } => {
                for field in fields {
                    let field_type = &infer_map.constraint_of(field.1).is;

                    if field_type.is_unknown() { continue }

                    let field_type = field_type.clone();
                    infer_map
                        .constraint_of(id)
                        .used_as
                        .has_fields()
                        .insert(field.0.clone(), field_type);
                }
            }
            _ => {}
        }
    }
}

fn infer_calls(infer_map: &mut InferMap, hlr: &mut FuncRep) -> TResult<()> {
    for call_index in 0.. {
        let id = match infer_map.calls.get_index(call_index) {
            Some(id) => *id,
            None => break,
        };

        let node_data = hlr.tree.get(id);
        let HNodeData::Call { query: FuncQuery { name, relation, generics }, .. } = node_data 
            else { unreachable!() };

        let relation_typ = infer_map.constraint_index_of_option(Inferable::Relation(id))
            .map(|constraint_index| infer_map.constraints[constraint_index].is.clone());

        let func_query = FuncQuery {
            name: name.clone(),
            relation: relation.map_inner_type(|_| relation_typ.unwrap()),
            generics: generics.clone(),
        };

        if fill_in_call(infer_map, hlr, func_query.clone(), &id)? {
            infer_map.calls.remove(&id);
                        
            break;
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

    let (code, code_id, known_generics) = 
        if let Some((code_id, known_generics)) = 
            hlr.comp_data.query_for_code_and_get_generics(func_query.code_query()) {
            (&hlr.comp_data.func_code[code_id], Some(code_id), known_generics)
        } else if let Some(code) = hlr.comp_data.get_derived_code(func_query.code_query()) {
            derive_code_storage = Some(code);
            (derive_code_storage.as_ref().unwrap(), None, None)
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

            let transform = hlr.tree.insert(*call_id, HNodeData::UnarOp {
                hs: a[0],
                op: Opcode::Transform,
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

            let transform = hlr.tree.insert(*call_id, HNodeData::UnarOp {
                hs: a[0],
                op: Opcode::Transform,
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

        if let Some(generics) = known_generics {
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

    if *step >= InferenceSteps::FillStructLiterals {
        for constraint_index in 0..infer_map.constraints.len() {
            let constraints = &infer_map.constraints[constraint_index];

            if constraints.is.is_known() { continue }

            let UsageContext::Struct { ref has_fields, field_count, .. } =
                constraints.used_as else { continue };

            if let Some(field_count) = field_count && has_fields.len() != field_count {
                continue;
            }

            if has_fields.iter().any(|(_, typ)| typ.is_unknown()) {
                continue;
            }

            infer_map.constraints[constraint_index].is =
                Type::new_struct(has_fields.iter().map(|(n, t)| (n.clone(), t.clone())).collect());
            infer_map.constraints[constraint_index].last_set_by = LastSetBy::StructLiteral;
        }
    }

    if *step >= InferenceSteps::FillIntsAndFloats {
        for constraints in &mut infer_map.constraints {
            if constraints.is.is_known() { continue }

            constraints.is = match constraints.used_as {
                UsageContext::Int => Type::i(32),
                UsageContext::Float => Type::f(32),
                _ => { continue }
            }
        }
    }
}
