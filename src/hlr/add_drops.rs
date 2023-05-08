use crate::{parse::Opcode, UniqueFuncInfo, TypeRelation, TypeEnum};

use super::{hlr_data::FuncRep, expr_tree::{HNodeData, CallGen, UnarOpGen}};

pub fn add_drops(hlr: &mut FuncRep) {
    hlr.modify_many_infallible(
        |make_id, make_lit, hlr| {
            let HNodeData::MakeVar { name: ref var_name, .. } = make_lit else { return };
            
            let var_type = hlr.variables[var_name].typ.clone();

            if var_type.is_shallow() || 
                matches!(var_type.as_type_enum(), TypeEnum::Ref(_)) ||
                var_name.to_string().chars().next() == Some('_') {
                return
            }

            for (id, data) in hlr.tree.iter() {
                if !matches!(data, HNodeData::Ident { name, .. } if name == var_name) {
                    continue
                }

                let parent = hlr.tree.get(hlr.tree.parent(id));

                match parent {
                    HNodeData::UnarOp { op, .. } => match op {
                        Opcode::Ref => continue,
                        _ => {},
                    },
                    _ => {},
                }

                // There is another place where this variable is used, without a reference,
                // so it doesn't need to be dropped.
                return;
            }

            let (stmt, block) = hlr.tree.statement_and_block(make_id);
            let HNodeData::Block { stmts, .. } = hlr.tree.get(block) else { unreachable!() };
            let last_return_index = stmts
                .iter()
                .skip_while(|&&s| s != stmt)
                .find(|&&s| matches!(hlr.tree.get(s), HNodeData::Return { .. }))
                .unwrap_or(stmts.last().unwrap());

            hlr.insert_statement_before(*last_return_index, CallGen {
                info: UniqueFuncInfo {
                    name: "drop".into(),
                    relation: TypeRelation::MethodOf(var_type.get_ref()),
                    generics: var_type.generics().clone(),
                    ..Default::default()
                },
                args: vec![box UnarOpGen {
                    op: Opcode::Ref,
                    hs: box var_name.clone(),
                    ret_type: var_type.get_ref()
                }],
            });
        }
    );
}
