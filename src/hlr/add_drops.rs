use crate::{parse::Opcode, FuncQuery, TypeRelation};

use super::{hlr_data::FuncRep, expr_tree::{HNodeData, CallGen, UnarOpGen, SetGen}};

#[cfg_attr(debug_assertions, inline(never))]
pub fn add_drops(hlr: &mut FuncRep) {
    let vars = hlr.variables
        .iter()
        .filter(|(_, info)| !info.do_not_drop)
        .map(|(name, info)| (name.clone(), info.typ.clone()))
        .collect::<Vec<_>>();

    for (var_id, var_type) in vars {
        if var_type.is_shallow() || var_type.is_primitive() {
            continue;
        }

        let should_not_drop = hlr.tree.iter().any(|(id, data)| {
            if !matches!(data, HNodeData::Ident { var_id: other_id, .. } if other_id == &var_id) {
                return false;
            }

            let parent = hlr.tree.get(hlr.tree.parent(id));

            match parent {
                HNodeData::UnarOp { op, .. } => match op {
                    Opcode::Ref => return false,
                    _ => {},
                },
                HNodeData::Member { .. } => return false,
                HNodeData::Set { lhs, .. } => {
                    if matches!(
                            hlr.tree.get_ref(lhs), 
                            HNodeData::Ident { var_id: other_id, .. } if other_id == &var_id
                        ) {
                        return false
                    }
                }
                _ => {},
            }

            // There is another place where this variable is used, without a reference,
            // so it doesn't need to be dropped.
            true
        });

        if should_not_drop {
            continue;
        }

        let (block, _) = hlr.tree
            .iter()
            .find(|(_, node)| {
                let HNodeData::Block { declared, .. } = node else { return false };
                declared.contains(&var_id)
            }).unwrap();
        let HNodeData::Block { stmts, .. } = hlr.tree.get(block) else { unreachable!() };
        let last_return = stmts
            .iter()
            .find(|&&s| matches!(hlr.tree.get(s), HNodeData::Return { .. }));

        let drop_call = CallGen {
            query: FuncQuery {
                name: "drop".into(),
                relation: TypeRelation::MethodOf(var_type.get_ref()),
                generics: var_type.generics().clone(),
                ..Default::default()
            },
            args: vec![Box::new(UnarOpGen {
                op: Opcode::Ref,
                hs: var_id,
                ret_type: var_type.get_ref()
            })],
            ..Default::default()
        };

        if let Some(last_return) = last_return {
            let mut return_data = hlr.tree.get(*last_return);
            let HNodeData::Return { to_return, .. } = &mut return_data
                else { unreachable!() };

            if let Some(to_return) = to_return {
                let to_return_data = hlr.tree.get(*to_return);
                let return_var = hlr.add_variable(&to_return_data.ret_type(), block);

                hlr.insert_statement_before(*last_return, SetGen {
                    lhs: return_var,
                    rhs: to_return_data,
                });
                hlr.insert_statement_before(*last_return, drop_call);
                *to_return = hlr.insert_quick(*last_return, return_var);
                hlr.tree.replace(*last_return, return_data);
            } else {
                hlr.insert_statement_before(*last_return, drop_call);
            }
        } else {
            hlr.insert_statement_after(*stmts.last().unwrap(), drop_call);
        }
    }
}
