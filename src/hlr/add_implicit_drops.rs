use std::{any::Any, rc::Rc};

use crate::{parse::Opcode, FuncQuery, TypeRelation, Type, StructType, TypeEnum, hlr::expr_tree::{MemberGen, GenSlot}, VarName};

use super::{hlr_data::{FuncRep, VarID, ArgIndex}, expr_tree::{HNodeData, CallGen, UnarOpGen, SetGen, ExprID, NodeDataGen, IndexGen, SetGenSlot}};

#[derive(Copy, Clone, Debug)]
enum ResourceOrigin {
    Argument(VarID),
    Call(ExprID),
}

#[derive(Debug)]
struct Resource {
    origin: ResourceOrigin,
    path: DestructorPath,
}

#[cfg_attr(debug_assertions, inline(never))]
pub fn add_implicit_drops(hlr: &mut FuncRep) {
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

    let mut destinations = Vec::<ValueDestination>::new();

    'resource_loop: for resource in resources {
        let mut destination = match resource.origin {
            ResourceOrigin::Argument(var_id) => {
                ValueDestination {
                    kind: ValueDestinationKind::InVar(var_id, hlr.tree.root),
                    path: resource.path,
                }
            },
            ResourceOrigin::Call(call_id) => {
                match value_destination(call_id, hlr, resource.path) {
                    Ok(destination) => destination,
                    Err(path) => {
                        destinations.push(ValueDestination {
                            kind: ValueDestinationKind::Dies { after: call_id },
                            path,
                        });
                        continue 'resource_loop;
                    }
                }
            }
        };

        loop {
            match destination.kind {
                ValueDestinationKind::InVar(..) => {
                    destination = next_destination(destination, hlr);
                    continue;
                },
                ValueDestinationKind::GoesToAnotherFunction | ValueDestinationKind::GoesIntoPointer => {
                    continue 'resource_loop;
                },
                ValueDestinationKind::Dies { .. } | ValueDestinationKind::Replaced { .. } => {
                    destinations.push(destination);
                    break;
                },
            }
        }
    }

    for destination in destinations {
        match destination.kind {
            ValueDestinationKind::Dies { after } => {
                let separated_val = hlr.separate_expression(after);

                let (_, block) = hlr.tree.statement_and_block(after);
                let HNodeData::Block { stmts, .. } = hlr.tree.get_ref(block)
                    else { unreachable!() };

                let destroy_gen = SetGenSlot {
                    set_to: separated_val,
                    then: UnarOpGen {
                        op: Opcode::Destroy,
                        hs: destination.path,
                        ret_type: Type::void(),
                    }
                };

                if matches!(hlr.tree.get_ref(*stmts.last().unwrap()), HNodeData::Return { .. }) {
                    hlr.insert_statement_before(
                       *stmts.last().unwrap(),
                        destroy_gen,
                    );
                } else {
                    hlr.insert_statement_after(
                        *stmts.last().unwrap(),
                        destroy_gen,
                    );
                }
            },
            ValueDestinationKind::Replaced { at } => {
                let HNodeData::Set { lhs, .. } = hlr.tree.get_ref(at)
                   else { unreachable!() };

                let destroy_gen = SetGenSlot {
                    set_to: *lhs,
                    then: UnarOpGen {
                        op: Opcode::Destroy,
                        hs: destination.path,
                        ret_type: Type::void(),
                    }
                };
                hlr.insert_statement_before(at, destroy_gen);
            },
            _ => {},
        }
    }
}

#[derive(Debug)]
struct ValueDestination {
    kind: ValueDestinationKind,
    path: DestructorPath
}

#[derive(Debug)]
enum ValueDestinationKind {
    InVar(VarID, ExprID),
    Dies {
        after: ExprID,
    },
    Replaced { at: ExprID },
    GoesToAnotherFunction,
    GoesIntoPointer,
}

fn next_destination(
    start: ValueDestination,
    hlr: &FuncRep,
) -> ValueDestination {
    use ValueDestinationKind::*;
    match start.kind {
        InVar(var_id, expr_id) => {
            if let Some(next_use) = next_use_of_var(var_id, expr_id, hlr) {
                match value_destination(next_use, hlr, start.path) {
                    Ok(destination) => destination,
                    Err(path) => {
                        let new_destination = ValueDestination {
                            kind: InVar(var_id, next_use), 
                            path,
                        };
                        return next_destination(new_destination, hlr);
                    }
                }
            } else {
                ValueDestination {
                    kind: Dies { after: expr_id },
                    path: start.path,
                }
            }
        },
        _ => unreachable!(),
    }
}

fn next_use_of_var(
    value: VarID, 
    start_search_at: ExprID, 
    hlr: &FuncRep
) -> Option<ExprID> {
    let is_value = |id: &ExprID| {
        let HNodeData::Ident { var_id, .. } = hlr.tree.get_ref(*id)
            else { return false };
        *var_id == value
    };

    let (statement, block) = hlr.tree.statement_and_block(start_search_at);

    let next_use_in_statement = hlr
        .tree
        .ids_of(statement)
        .into_iter()
        .skip_while(|id| *id != start_search_at)
        .skip(1)
        .find(is_value);

    if let Some(next_use) = next_use_in_statement {
        return Some(next_use);
    }

    hlr.tree.ids_of(block)
        .into_iter()
        .skip_while(|id| *id != start_search_at )
        .skip(1)
        .find(is_value)
}

#[derive(Default, Debug)]
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

#[derive(PartialEq, Eq, Debug)]
enum DestructorPathBit {
    Member(VarName),
    Index(ExprID),
}

// figures out where a value goes. for example, if 'value' refers to the 2 in x = 2,
// the destination is Some(InVar(X)). If the value goes nowhere, like x in { x = 2 },
// then the path is returned back to the caller.
fn value_destination(
    value: ExprID, 
    hlr: &FuncRep, 
    mut path: DestructorPath,
) -> Result<ValueDestination, DestructorPath> {
    use HNodeData::*;
    let parent_id = hlr.tree.parent(value);
    match hlr.tree.get_ref(parent_id) {
        StructLit { var_type, fields, initialize } => todo!(),
        ArrayLit { var_type, parts, initialize } => todo!(),
        Ident { var_type, var_id } => todo!(),
        Set { lhs, rhs } if *rhs == value => value_source(*lhs, hlr, path),
        Set { lhs, .. } if *lhs == value => {
            Ok(ValueDestination {
                kind: ValueDestinationKind::Replaced { at: parent_id },
                path,
            })
        },
        Call { .. } => {
            // Since Srets haven't been set yet, this must be an argument
            Ok(ValueDestination {
                kind: ValueDestinationKind::GoesToAnotherFunction,
                path,
            })
        }
        IndirectCall { ret_type, f, a, sret } => todo!(),
        Member { ret_type, object, field } => {
            if let Some(last) = path.bits.last() && 
                last == &DestructorPathBit::Member(field.clone()) {
                let member_bit = path.bits.pop().unwrap();

                match value_destination(parent_id, hlr, path) {
                    Err(mut path) => {
                        path.bits.push(member_bit);
                        Err(path)
                    }
                    ok => ok,
                }
            } else {
                Err(path)
            }
        },
        Index { ret_type, object, index } => {
            if let Some(last) = path.bits.last() && 
                last == &DestructorPathBit::Index(index.clone()) {
                let member_bit = path.bits.pop().unwrap();

                match value_destination(parent_id, hlr, path) {
                    Err(mut path) => {
                        path.bits.push(member_bit);
                        Err(path)
                    }
                    ok => ok,
                }
            } else {
                Err(path)
            }
        },
        Transform { hs, ret_type, steps } => todo!(),
        IfThen { ret_type, i, t } => todo!(),
        IfThenElse { ret_type, i, t, e } => todo!(),
        While { w, d } => todo!(),
        Block { ret_type, stmts, declared, aliases, withs, goto_labels } => Err(path),
        Return { ret_type, to_return } => Ok(ValueDestination {
            kind: ValueDestinationKind::GoesToAnotherFunction,
            path: DestructorPath::default(),
        }),
        UnarOp { op: Opcode::Ref | Opcode::RemoveTypeWrapper, .. } => Err(path),
        UnarOp { ret_type, op: Opcode::Deref, .. } => {
            let deref_parent_id = hlr.tree.parent(parent_id);
            let deref_parent_data = hlr.tree.get(deref_parent_id);
            if matches!(deref_parent_data, HNodeData::Member { .. } | HNodeData::Index { .. }) {
                Err(path)
            } else {
                panic!()
            }
        },
        Number { .. } | Float { .. } | Bool { .. } | BinOp { .. } | UnarOp { .. } | 
        AccessAlias(_) | GotoLabel(_) | Goto(_) | GlobalLoad { .. } => unreachable!(),
        _ => Err(path),
    }
}

fn value_source(
    value: ExprID, 
    hlr: &FuncRep, 
    path: DestructorPath,
) -> Result<ValueDestination, DestructorPath> {
    use HNodeData::*;
    match hlr.tree.get_ref(value) {
        HNodeData::Ident { var_id, .. } => 
            Ok(ValueDestination {
                kind: ValueDestinationKind::InVar(*var_id, value),
                path,
            }),
        HNodeData::Member { ret_type, object, field } => {
            value_source(*object, hlr, path).map(|mut source| {
                source.path.bits.push(DestructorPathBit::Member(field.clone()));
                source
            })
        },
        HNodeData::Index { object, index, .. } => {
            value_source(*object, hlr, path).map(|mut source| {
                source.path.bits.push(DestructorPathBit::Index(*index));
                source
            })
        },
        HNodeData::UnarOp { op: Opcode::Deref, .. } => {
            Ok(ValueDestination {
                kind: ValueDestinationKind::GoesIntoPointer,
                path,
            })
        }
        _ => Err(path),
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
        Array(_) => todo!(),
        Destructor(_) => vec![
            DestructorPath::default(),
        ],
    }
}
