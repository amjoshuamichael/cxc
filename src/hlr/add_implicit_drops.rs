use std::{any::Any, rc::Rc, collections::HashMap};

use slotmap::{SlotMap, DefaultKey};

use crate::{parse::Opcode, FuncQuery, TypeRelation, Type, StructType, TypeEnum, hlr::expr_tree::{MemberGen, GenSlot}, VarName, ArrayType, typ::UnionType};

use super::{hlr_data::{FuncRep, VarID, ArgIndex}, expr_tree::{HNodeData, CallGen, UnarOpGen, SetGen, ExprID, NodeDataGen, IndexGen, SetGenSlot}};

#[derive(Hash, PartialEq, Eq, Copy, Clone, Debug)]
enum ResourceOrigin {
    Argument(VarID),
    Call(ExprID),
}

#[derive(Debug)]
struct Resource {
    origin: ResourceOrigin,
    path: DestructorPath,
}

pub fn add_implicit_drops(hlr: &mut FuncRep) {
    let all_ids = hlr.tree.ids_in_order();
    let mut resources = Vec::<Resource>::new();

    for (var_id, var_info) in &hlr.variables {
        if let ArgIndex::Some(index) = var_info.arg_index {
            for path in destructor_paths(&var_info.typ) {
                resources.push(Resource {
                    origin: ResourceOrigin::Argument(var_id),
                    path,
                });
            }
        }
    }

    for id in hlr.tree.ids_in_order() {
        match hlr.tree.get_ref(id) {
            HNodeData::Call { ret_type, .. } => {
                for path in destructor_paths(&ret_type) {
                    resources.push(Resource {
                        origin: ResourceOrigin::Call(id),
                        path,
                    });
                }
            },
            _ => {}
        }
    }

    let mut prospective_destinations = SlotMap::<DefaultKey, ValueDestination>::new();

    'resource_loop: for resource in resources {
        let mut destination = match resource.origin {
            ResourceOrigin::Argument(var_id) => {
                let HNodeData::Block { stmts, .. } = hlr.tree.get_ref(hlr.tree.root)
                    else { unreachable!() };

                let first_exec = first_exec_in(hlr.tree.root, hlr);
                let next_use = next_use_of_var(var_id, first_exec, hlr, false)
                    .unwrap();
                let dest_info = DestinationInfo {
                    path: resource.path,
                    init_conditions: Vec::new(),
                    final_conditions: Vec::new(),
                    resource: Rc::new(resource.origin),
                };
                let dests = 
                    next_destinations_of_var(var_id, next_use, hlr, dest_info);

                for dest in dests {
                    prospective_destinations.insert(dest);
                }
            },
            ResourceOrigin::Call(call_id) => {
                let destination_info = DestinationInfo {
                    path: resource.path,
                    init_conditions: conditions_for_node(call_id, hlr),
                    final_conditions: Vec::new(),
                    resource: Rc::new(resource.origin),
                };

                match value_destination(call_id, hlr, destination_info) {
                    Ok(destination) => prospective_destinations.insert(destination),
                    Err(info) => {
                        prospective_destinations.insert(ValueDestination {
                            kind: ValueDestinationKind::Dies { 
                                value_at: call_id, 
                                after_statement: call_id 
                            },
                            info,
                        });
                        continue 'resource_loop;
                    }
                };
            }
        };
    }

    let mut ending_destinations = Vec::<ValueDestination>::new();

    while !prospective_destinations.is_empty() {
        let a_key = prospective_destinations.keys().next().unwrap();
        let mut destination = prospective_destinations.remove(a_key).unwrap();

        use ValueDestinationKind::*;

        match destination.kind {
            InVar(var_id, expr_id) => {
                let (NodeAfter::Some(next) | NodeAfter::Branch(next)) = 
                    node_after(expr_id, hlr) else { panic!() };
                let next_use = next_use_of_var(var_id, next, hlr, false)
                    .unwrap();
                let next_dests = 
                    next_destinations_of_var(var_id, next_use, hlr, destination.info);
                for dest in next_dests {
                    prospective_destinations.insert(dest);
                }
            },
            _ => {
                ending_destinations.push(destination);
            },
        }
    }

    'filtering: loop {
        for (da, dest_a) in ending_destinations.iter().enumerate() {
            for (db, dest_b) in ending_destinations[(da + 1)..].iter().enumerate() {
                if dest_a.kind == dest_b.kind && dest_a.info.path == dest_b.info.path {
                    // these two destinations end up in the same place.
                    if dest_a.info.final_conditions == dest_b.info.final_conditions &&
                        dest_a.info.init_conditions == dest_b.info.init_conditions {
                        ending_destinations.remove(da);
                        continue 'filtering;
                    }

                    for (ca, cond_a) in dest_a.info.final_conditions.iter().enumerate() {
                        let cond_a_inv = cond_a.invert();

                        if let Some(pos) = dest_b.info.final_conditions.iter()
                            .position(|c| *c == cond_a_inv) {
                            let mut dest = ending_destinations.remove(da);
                            ending_destinations.remove(db);
                            dest.info.final_conditions.remove(da);
                            ending_destinations.push(dest);
                            continue 'filtering;
                        }
                    }
                }
            } 
        }

        break;
    }

    for destination in ending_destinations {
        match destination.kind {
            ValueDestinationKind::Dies { value_at, after_statement } => {
                let separated_val = hlr.separate_expression(value_at);

                hlr.insert_statement_after(
                    after_statement,
                    SetGenSlot {
                        set_to: separated_val,
                        then: UnarOpGen {
                            op: Opcode::Destroy,
                            hs: destination.info.path,
                            ret_type: Type::void(),
                        }
                    },
                );
            },
            ValueDestinationKind::Replaced { at } => {
                let HNodeData::Set { lhs, .. } = hlr.tree.get_ref(at)
                   else { unreachable!() };

                let destroy_gen = SetGenSlot {
                    set_to: *lhs,
                    then: UnarOpGen {
                        op: Opcode::Destroy,
                        hs: destination.info.path,
                        ret_type: Type::void(),
                    }
                };
                hlr.insert_statement_before(at, destroy_gen);
            },
            ValueDestinationKind::VarDies { var_id, return_id } => {
                let destructor = SetGenSlot {
                    set_to: var_id,
                    then: UnarOpGen {
                        op: Opcode::Destroy,
                        hs: destination.info.path,
                        ret_type: Type::void(),
                    }
                };

                if let Some(cond) = destination.info.init_conditions.get(0) {
                    let id = match cond {
                        ValueCondition::True(id) => {
                            let HNodeData::IfThenElse { t, .. } = hlr.tree.get_ref(*id)
                                else { unreachable!() };
                            let t = *t;
                            let space = hlr.tree.make_one_space(t);
                            let HNodeData::Block { stmts, .. } = hlr.tree.get_mut(t)
                                else { unreachable!() };
                            stmts.push(space);
                            space
                        },
                        ValueCondition::False(id) => {
                            let HNodeData::IfThenElse { e, .. } = hlr.tree.get_ref(*id)
                                else { unreachable!() };
                            let e = *e;
                            let space = hlr.tree.make_one_space(e);
                            let HNodeData::Block { stmts, .. } = hlr.tree.get_mut(e)
                                else { unreachable!() };
                            stmts.push(space);
                            space
                        },
                    };

                    hlr.replace_quick(id, destructor);
                } else {
                    hlr.insert_statement_before(return_id, destructor);
                }
            }
            _ => {},
        }
    }
}

#[derive(Debug)]
struct ValueDestination {
    kind: ValueDestinationKind,
    info: DestinationInfo,
}

#[derive(Clone, Debug)]
struct DestinationInfo {
    path: DestructorPath,
    init_conditions: Vec<ValueCondition>,
    final_conditions: Vec<ValueCondition>,
    resource: Rc<ResourceOrigin>,
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
enum ValueCondition {
    True(ExprID), 
    False(ExprID), 
}

impl ValueCondition {
    pub fn invert(self) -> Self {
        match self {
            ValueCondition::True(id) => ValueCondition::False(id),
            ValueCondition::False(id) => ValueCondition::True(id),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum ValueDestinationKind {
    InVar(VarID, ExprID),
    Dies { 
        value_at: ExprID, 
        after_statement: ExprID,
    },
    VarDies { var_id: VarID, return_id: ExprID, },
    Replaced { at: ExprID },
    GoesToAnotherFunction,
    GoesIntoPointer,
}

#[derive(Debug)]
enum NextUseOfVar {
    Some(ExprID),
    Dies(ExprID),
    IfThenElse(ExprID, Box<NextUseOfVar>, Box<NextUseOfVar>),
}

fn next_destinations_of_var(
    var_id: VarID, 
    next_use: NextUseOfVar, 
    hlr: &FuncRep,
    info: DestinationInfo,
) -> Vec<ValueDestination> {
    match next_use {
        NextUseOfVar::Some(next_use) => {
            let dest = var_destination(var_id, next_use, hlr, info);
            vec![dest]
        }
        NextUseOfVar::Dies(return_id) => {
            vec![ValueDestination {
                kind: ValueDestinationKind::VarDies { var_id, return_id },
                info,
            }]
        },
        NextUseOfVar::IfThenElse(if_id, then_id, else_id) => {
            let mut then_dests = 
                next_destinations_of_var(var_id, *then_id, hlr, info.clone());
            for dest in &mut then_dests {
                dest.info.final_conditions.push(ValueCondition::True(if_id));
            }

            let mut else_dests = 
                next_destinations_of_var(var_id, *else_id, hlr, info);
            for dest in &mut else_dests {
                dest.info.final_conditions.push(ValueCondition::False(if_id));
            }

            then_dests.into_iter().chain(else_dests.into_iter()).collect()
        },
    }
}

fn next_use_of_var(
    value: VarID, 
    mut search: ExprID, 
    hlr: &FuncRep,
    skip_returns: bool,
) -> Result<NextUseOfVar, ()> {
    loop {
        match hlr.tree.get_ref(search) {
            HNodeData::IfThenElse { i, t, e, .. } => {
                let l = next_use_of_var(value, first_exec_in(*t, hlr), hlr, false)?;
                let r = next_use_of_var(value, first_exec_in(*e, hlr), hlr, false)?;

                return Ok(NextUseOfVar::IfThenElse(search, Box::new(l), Box::new(r)));
            },
            HNodeData::Return { to_return: Some(to_return), .. } if !skip_returns => {
                let mut to_return_ids = Vec::new();
                hlr.tree.ids_of(search, &mut to_return_ids);

                if let Ok(use_in_var) = next_use_of_var(value, *to_return, hlr, true) {
                    return Ok(use_in_var);
                } else {
                    return Ok(NextUseOfVar::Dies(search));
                }
            }
            HNodeData::Return { to_return: None, .. } => {
                return Ok(NextUseOfVar::Dies(search));
            }
            HNodeData::Ident { var_id, .. } if *var_id == value => {
                return Ok(NextUseOfVar::Some(search));
            }
            _ => {},
        }

        search = match node_after(search, hlr) {
            NodeAfter::Some(next) | NodeAfter::Branch(next) => next,
            NodeAfter::Finished => return Err(()),
            NodeAfter::Returns(return_id) => return Ok(NextUseOfVar::Dies(search)),
        };
    }
}

#[derive(PartialEq, Eq, Default, Debug, Clone)]
pub struct DestructorPath {
    bits: Vec<DestructorPathBit>,
}

impl NodeDataGen for DestructorPath {
    fn put_in_id(&self, hlr: &mut FuncRep, spot: ExprID) {
        let mut working: Box::<dyn NodeDataGen> = Box::new(GenSlot);

        for bit in self.bits.iter().rev() {
            match bit {
                DestructorPathBit::Member(field) => {
                    working = Box::new(MemberGen {
                        object: working,
                        field: field.clone(),
                    })
                },
                DestructorPathBit::Index(index) => {
                    let separated_index = hlr.separate_expression(*index);
                    working = Box::new(IndexGen {
                        object: working,
                        index: separated_index,
                    })
                }
            }
        }

        hlr.replace_quick(spot, working);
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
enum DestructorPathBit {
    Member(VarName),
    Index(ExprID),
}

fn var_destination(
    var_id: VarID,
    next_use: ExprID,
    hlr: &FuncRep,
    info: DestinationInfo,
) -> ValueDestination {
    match value_destination(next_use, hlr, info) {
        Ok(destination) => destination,
        Err(info) => ValueDestination {
            kind: ValueDestinationKind::InVar(var_id, next_use), 
            info,
        }
    }
}
// figures out where a value goes. for example, if 'value' refers to the 2 in x = 2,
// the destination is Some(InVar(X)). If the value goes nowhere, like x in { x = 2 },
// then the path is returned back to the caller.
fn value_destination(
    value: ExprID, 
    hlr: &FuncRep, 
    mut info: DestinationInfo,
) -> Result<ValueDestination, DestinationInfo> {
    use HNodeData::*;
    let parent_id = hlr.tree.parent(value);
    match hlr.tree.get_ref(parent_id) {
        StructLit { var_type, fields, initialize } => todo!(),
        ArrayLit { var_type, parts, initialize } => todo!(),
        Ident { var_type, var_id } => todo!(),
        Set { lhs, rhs } if *rhs == value => value_source(*lhs, hlr, info),
        Set { lhs, .. } if *lhs == value => {
            Ok(ValueDestination {
                kind: ValueDestinationKind::Replaced { at: parent_id },
                info,
            })
        },
        Call { .. } => {
            // Since Srets haven't been set yet, this must be an argument
            Ok(ValueDestination {
                kind: ValueDestinationKind::GoesToAnotherFunction,
                info,
            })
        }
        IndirectCall { ret_type, f, a, sret } => todo!(),
        Member { ret_type, object, field } => {
            if let Some(last) = info.path.bits.last() && 
                last == &DestructorPathBit::Member(field.clone()) {
                let member_bit = info.path.bits.pop().unwrap();

                match value_destination(parent_id, hlr, info) {
                    Err(mut info) => {
                        info.path.bits.push(member_bit);
                        Err(info)
                    }
                    ok => ok,
                }
            } else {
                Err(info)
            }
        },
        Index { ret_type, object, index } => {
            if let Some(last) = info.path.bits.last() && 
                last == &DestructorPathBit::Index(index.clone()) {
                let member_bit = info.path.bits.pop().unwrap();

                match value_destination(parent_id, hlr, info) {
                    Err(mut info) => {
                        info.path.bits.push(member_bit);
                        Err(info)
                    }
                    ok => ok,
                }
            } else {
                Err(info)
            }
        },
        Transform { hs, ret_type, steps } => todo!(),
        IfThenElse { ret_type, i, t, e } => todo!(),
        While { w, d } => todo!(),
        Block { ret_type, stmts, declared, aliases, withs, goto_labels } => Err(info),
        Return { ret_type, to_return } => Ok(ValueDestination {
            kind: ValueDestinationKind::GoesToAnotherFunction,
            info,
        }),
        UnarOp { op: Opcode::Ref | Opcode::RemoveTypeWrapper, .. } => Err(info),
        UnarOp { ret_type, op: Opcode::Deref, .. } => {
            let deref_parent_id = hlr.tree.parent(parent_id);
            let deref_parent_data = hlr.tree.get(deref_parent_id);
            if matches!(deref_parent_data, HNodeData::Member { .. } | HNodeData::Index { .. }) {
                Err(info)
            } else {
                Err(info)
            }
        },
        Number { .. } | Float { .. } | Bool { .. } | BinOp { .. } | UnarOp { .. } | 
        AccessAlias(_) | GotoLabel(_) | Goto(_) | GlobalLoad { .. } => unreachable!(),
        _ => Err(info),
    }
}

fn value_source(
    value: ExprID, 
    hlr: &FuncRep, 
    info: DestinationInfo,
) -> Result<ValueDestination, DestinationInfo> {
    use HNodeData::*;
    match hlr.tree.get_ref(value) {
        HNodeData::Ident { var_id, .. } => 
            Ok(ValueDestination {
                kind: ValueDestinationKind::InVar(*var_id, value),
                info,
            }),
        HNodeData::Member { ret_type, object, field } => {
            value_source(*object, hlr, info).map(|mut source| {
                source.info.path.bits.push(DestructorPathBit::Member(field.clone()));
                source
            })
        },
        HNodeData::Index { object, index, .. } => {
            value_source(*object, hlr, info).map(|mut source| {
                source.info.path.bits.push(DestructorPathBit::Index(*index));
                source
            })
        },
        HNodeData::UnarOp { op: Opcode::Deref, .. } => {
            Ok(ValueDestination {
                kind: ValueDestinationKind::GoesIntoPointer,
                info,
            })
        }
        _ => Err(info),
    }
}

pub fn destructor_paths(typ: &Type) -> Vec<DestructorPath> {
    use TypeEnum::*;
    match typ.as_type_enum() {
        Int(_) | Ref(_) | Float(_) | Func(_) | Bool | Void => Vec::new(),
        Unknown => panic!(),
        Struct(StructType { fields, .. }) => {
            fields.iter()
            .map(|field| {
                destructor_paths(&field.typ)
                    .into_iter()
                    .map(|mut path| {
                        path.bits.push(DestructorPathBit::Member(field.name.clone()));
                        path
                    })
            })
            .flatten()
            .collect::<Vec<_>>()
        },
        Union(UnionType { fields, .. }) => {
            fields.iter()
            .map(|field| {
                destructor_paths(&field.typ)
                    .into_iter()
                    .map(|mut path| {
                        todo!();
                        path.bits.push(DestructorPathBit::Member(field.name.clone()));
                        path
                    })
            })
            .flatten()
            .collect::<Vec<_>>()
        },
        Array(ArrayType { base, .. }) => {
            if destructor_paths(&base).len() > 0 {
                panic!();
            } else {
                Vec::new()
            }
        },
        Destructor(_) => vec![
            DestructorPath::default(),
        ],
    }
}

fn first_exec_in(id: ExprID, hlr: &FuncRep) -> ExprID {
    match hlr.tree.get_ref(id) {
        HNodeData::StructLit { fields, .. } => first_exec_in(fields[0].1, hlr),
        HNodeData::ArrayLit { parts: many, .. } |
        HNodeData::Call { a: many, .. } |
        HNodeData::IndirectCall { a: many, .. } |
        HNodeData::Block { stmts: many, .. } => {
            if many.len() == 0 {
                id
            } else {
                first_exec_in(many[0], hlr)
            }
        },
        HNodeData::Set { lhs: x, .. } |
        HNodeData::Member { object: x, .. } |
        HNodeData::Index { object: x, .. } |
        HNodeData::UnarOp { hs: x, .. } |
        HNodeData::BinOp { lhs: x, .. } |
        HNodeData::IfThenElse { i: x, .. } |
        HNodeData::While { w: x, .. } |
        HNodeData::Return { to_return: Some(x), .. } => first_exec_in(*x, hlr),
        _ => id,
    }
}

#[derive(Debug)]
enum NodeAfter {
    Some(ExprID),
    Finished,
    Returns(ExprID),
    Branch(ExprID),
}

fn node_after(id: ExprID, hlr: &FuncRep) -> NodeAfter {
    if id == hlr.tree.root {
        return NodeAfter::Finished; 
    }

    if matches!(hlr.tree.get_ref(id), HNodeData::Return { .. }) {
        return NodeAfter::Returns(id);
    }

    let parent = hlr.tree.parent(id);

    let some = match hlr.tree.get_ref(parent) {
        HNodeData::StructLit { fields, .. } => {
            if let Some(next) = fields.iter().skip_while(|fid| fid.1 != id).skip(1).next() {
                first_exec_in(next.1, hlr)
            } else {
                return node_after(parent, hlr);
            }
        },
        HNodeData::ArrayLit { parts: many, .. } |
        HNodeData::Block { stmts: many, .. } |
        HNodeData::Call { a: many, .. } |
        HNodeData::IndirectCall { a: many, .. } => {
            if let Some(next) = many.iter().skip_while(|fid| **fid != id).skip(1).next() {
                first_exec_in(*next, hlr)
            } else {
                return node_after(parent, hlr);
            }
        }
        HNodeData::Goto(_) => todo!(),
        HNodeData::Set { lhs: l, rhs: r } |
        HNodeData::Index { object: l, index: r, .. } |
        HNodeData::BinOp { lhs: l, rhs: r, .. } => {
            if *l == id {
                first_exec_in(*r, hlr)
            } else {
                return node_after(parent, hlr);
            }
        },
        HNodeData::IfThenElse { i, t, e, .. } => {
            if *t == id || *e == id {
                return node_after(parent, hlr);
            } else if *i == id {
                return NodeAfter::Branch(parent)
            } else {
                panic!()
            }
        },
        HNodeData::While { w: l, d: r } => {
            return NodeAfter::Branch(parent)
        },
        _ => parent,
    };

    NodeAfter::Some(some)
}

fn conditions_for_node(mut id: ExprID, hlr: &FuncRep) -> Vec<ValueCondition> {
    let mut conditions = Vec::new();

    while id != hlr.tree.root {
        let parent = hlr.tree.parent(id);

        match hlr.tree.get_ref(parent) {
            HNodeData::IfThenElse { t, e, .. } => {
                if id == *t {
                    conditions.push(ValueCondition::True(parent));
                } else if id == *e {
                    conditions.push(ValueCondition::False(parent));
                }
            },
            _ => {},
        }

        id = parent;
    }

    conditions
}
