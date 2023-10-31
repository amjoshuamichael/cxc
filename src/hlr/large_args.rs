use super::hlr_data::{VarID, ArgIndex};
use super::prelude::*;
use crate::Type;
use crate::hlr::expr_tree::*;

use crate::typ::{ArgStyle, ABI};

pub fn large_args(hlr: &mut FuncRep) {
    handle_other_calls(hlr);
    handle_own_args(hlr);
}

#[cfg_attr(debug_assertions, inline(never))]
fn handle_own_args(hlr: &mut FuncRep) {
    let args: Vec<_> = hlr
        .args()
        .into_iter()
        .map(|(id, info)| (id.clone(), info.clone()))
        .collect::<Vec<_>>();

    for (id, info) in args {
        if info.arg_index == ArgIndex::SRet { continue }

        match info.typ.arg_style(ABI::C) {
            ArgStyle::Direct => {},
            ArgStyle::Ints(..) | ArgStyle::Floats(..) => arg_by_ints_or_floats(hlr, id),
            ArgStyle::Pointer => arg_by_pointer(hlr, id),
        }
    }
}

fn arg_by_ints_or_floats(hlr: &mut FuncRep, og_arg: VarID) {
    let arg_load_arg_type = hlr.variables[og_arg].typ.clone();
    let new_arg_load_var = hlr.add_variable(&arg_load_arg_type);
    let set_arg_load_var = hlr.insert_quick(
        hlr.tree.root,
        SetGen {
            lhs: new_arg_load_var.clone(),
            rhs: HNodeData::Ident {
                var_id: og_arg.clone(),
                var_type: arg_load_arg_type.raw_arg_type(ABI::C), 
            }
        }
    );

    let HNodeData::Block { ref mut stmts, .. } = hlr.tree.get_mut(hlr.tree.root) 
        else { unreachable!() };
    stmts.insert(0, set_arg_load_var);

    hlr.modify_many_infallible(
        |var_id, var_data, hlr| {
            let HNodeData::Ident { var_id: name, .. } = var_data else { return };

            if hlr.tree.parent(var_id) == set_arg_load_var {
                // we don't want to change the inital loading into new_arg_load_var
                return;
            }

            if name == &og_arg {
                *name = new_arg_load_var.clone()
            }
        }
    );
}

fn arg_by_pointer(hlr: &mut FuncRep, arg_id: VarID) {
    let arg_type_reffed = hlr.variables[arg_id].typ.get_ref();

    hlr.modify_many_infallible_rev(
        move |ident_id, mut var_data, hlr| {
            let HNodeData::Ident { var_id, var_type, .. } = &mut var_data
                else { return };
            if arg_id != *var_id { return }

            hlr.replace_quick(ident_id, DerefGen(
                HNodeData::Ident {
                    var_id: arg_id.clone(),
                    var_type: arg_type_reffed.clone(),
                },
            ));

            *var_data = hlr.tree.get(ident_id);
        }
    );
}

#[cfg_attr(debug_assertions, inline(never))]
fn handle_other_calls(hlr: &mut FuncRep) {
    hlr.modify_many_infallible(
        move |call_id, data, hlr| {
            let HNodeData::Call { a: args, query, .. } = data else { return };

            if &*query.name == "cast" {
                return;
            }

            let abi = if let Some(id) = hlr.comp_data.query_for_code(query.code_query()) {
                hlr.comp_data.func_code[id].abi
            } else {
                ABI::C
            };

            for (a, arg) in args.clone().into_iter().enumerate() {
                let old_arg_type = hlr.tree.get(arg).ret_type();
                let arg_style = old_arg_type.arg_style(abi);

                if arg_style == ArgStyle::Pointer {
                    args[a] = hlr.insert_quick(call_id, RefGen(arg));
                } else if let ArgStyle::Ints(..) = arg_style {
                    let _new_arg_name = format!("{}_arg_{}", query.name, a);
                    let raw_arg_type = old_arg_type.raw_arg_type(abi);
                    let new_arg = hlr.add_variable(&raw_arg_type);

                    hlr.insert_statement_before(call_id, MemCpyGen {
                        from: RefGen(arg),
                        to: RefGen(new_arg),
                        size: HNodeData::Number {
                            lit_type: Type::i(64),
                            value: hlr.tree.get_ref(arg).ret_type().size() as u64,
                        }
                    });

                    args[a] = hlr.insert_quick(call_id, new_arg);
                }
            }
        }
    );
}
