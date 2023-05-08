use super::prelude::*;
use crate::hlr::expr_tree::*;
use crate::lex::VarName;
use crate::typ::ArgStyle;

pub fn large_args(hlr: &mut FuncRep) {
    handle_other_calls(hlr);
    handle_own_args(hlr);
}

fn handle_own_args(hlr: &mut FuncRep) {
    let args: Vec<_> = hlr.args().map(|(name, info)| (name.clone(), info.clone())).collect::<Vec<_>>();
    for (name, info) in args {
        match info.typ.arg_style() {
            ArgStyle::Direct => {},
            ArgStyle::Pointer => arg_by_pointer(hlr, name),
        }
    }
}

fn arg_by_pointer(hlr: &mut FuncRep, name: VarName) {
    hlr.variables[&name].typ = hlr.variables[&name].typ.get_ref();
}

fn handle_other_calls(hlr: &mut FuncRep) {
    hlr.modify_many_infallible(
        move |call_id, data, hlr| {
            let HNodeData::Call { a: args, f, .. } = data else { return };

            // TODO: make HNodeData for cast
            
            if f == &VarName::from("cast") {
                return;
            }

            for arg in args {
                if hlr.tree.get(*arg).ret_type().arg_style() == ArgStyle::Pointer {
                    let new_ref_arg = hlr.insert_quick(call_id, RefGen {
                        object: box hlr.tree.get(*arg),
                    });

                    // TODO: replace by id??
                    hlr.tree.replace(*arg, hlr.tree.get(new_ref_arg));
                }
            }
        }
    );
}
