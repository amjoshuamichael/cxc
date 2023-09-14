use crate::{parse::Opcode, FuncQuery, TypeRelation, TypeEnum};

use super::{hlr_data::FuncRep, expr_tree::{HNodeData, CallGen, UnarOpGen}};

#[cfg_attr(debug_assertions, inline(never))]
pub fn add_drops(hlr: &mut FuncRep) {
    let vars = hlr.variables
        .iter()
        .filter(|(_, info)| !info.do_not_drop)
        .map(|(name, info)| (name.clone(), info.typ.clone()))
        .collect::<Vec<_>>();

    for (var_name, var_type) in vars {
        if var_type.is_shallow() || 
            matches!(var_type.as_type_enum(), TypeEnum::Ref(_)) ||
            var_name.to_string().chars().next() == Some('_') {
            continue;
        }

        let should_not_drop = hlr.tree.iter().any(|(id, data)| {
            if !matches!(data, HNodeData::Ident { name, .. } if name == &var_name) {
                return false;
            }

            let parent = hlr.tree.get(hlr.tree.parent(id));

            match parent {
                HNodeData::UnarOp { op, .. } => match op {
                    Opcode::Ref => return false,
                    _ => {},
                },
                HNodeData::Set { lhs, .. } => {
                    if matches!(
                            hlr.tree.get_ref(lhs), 
                            HNodeData::Ident { name, .. } if name == &var_name
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

        let (first_use, _) = hlr.tree
            .iter()
            .find(|(_, node)| {
                matches!(node, HNodeData::Ident { name, .. } if name == &var_name)
            }).unwrap();
        let (stmt, block) = hlr.tree.statement_and_block(first_use);
        let HNodeData::Block { stmts, .. } = hlr.tree.get(block) else { unreachable!() };
        let last_return = stmts
            .iter()
            .skip_while(|&&s| s != stmt)
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
                hs: var_name,
                ret_type: var_type.get_ref()
            })],
            ..Default::default()
        };

        if let Some(last_return) = last_return {
            hlr.insert_statement_before(*last_return, drop_call);
        } else {
            hlr.insert_statement_after(*stmts.last().unwrap(), drop_call);
        }
    }
}
