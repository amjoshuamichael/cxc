use super::hlr_data::{VarID, ArgIndex};
use super::prelude::*;
use crate::{Type, TypeEnum, FuncType};
use crate::hlr::expr_tree::*;

use crate::typ::{ArgStyle, ABI};

pub fn large_args(hlr: &mut FuncRep) {
    handle_other_calls(hlr);
    handle_own_args(hlr);
}

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
            ArgStyle::Void => todo!(),
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
        |var_id, hlr| {
            if hlr.tree.parent(var_id) == set_arg_load_var {
                // we don't want to change the inital loading into new_arg_load_var
                return;
            }

            let HNodeData::Ident { var_id: ref mut name, .. } = hlr.tree.get_mut(var_id) 
                else { return };

            if name == &og_arg {
                *name = new_arg_load_var.clone()
            }
        }
    );
}

fn arg_by_pointer(hlr: &mut FuncRep, arg_id: VarID) {
    let arg_type_reffed = hlr.variables[arg_id].typ.get_ref();
    // TODO: change hlr.ret_type to hlr.func_type and set this arg type as a variable
     //hlr.variables[arg_id].typ = arg_type_reffed.clone();

    hlr.modify_many_infallible_rev(
        move |ident_id, hlr| {
            let HNodeData::Ident { var_id, var_type, .. } = &mut hlr.tree.get_mut(ident_id)
                else { return };
            if arg_id != *var_id { return }

            hlr.replace_quick(ident_id, DerefGen(
                HNodeData::Ident {
                    var_id: arg_id.clone(),
                    var_type: arg_type_reffed.clone(),
                },
            ));
        }
    );
}

fn handle_other_calls(hlr: &mut FuncRep) {
    hlr.modify_many_infallible(
        move |call_id, hlr| {
            let call_data = hlr.tree.get_ref(call_id) else { return };

            let HNodeData::Call { a: ref args, ref call, .. } = 
                call_data else { return };

            if let HCallable::Direct(query) = call && &*query.name == "cast" {
                return;
            }

            let mut args = args.clone();

            let abi = hlr.abi_of_call(call_id);

            for (a, arg) in args.clone().into_iter().enumerate() {
                let old_arg_type = hlr.tree.get(arg).ret_type();
                let arg_style = old_arg_type.arg_style(abi);

                if arg_style == ArgStyle::Pointer {
                    args[a] = hlr.insert_quick(call_id, RefGen(arg));
                } else if let ArgStyle::Ints(..) | ArgStyle::Floats(..) = arg_style {
                    let raw_arg_type = old_arg_type.raw_arg_type(abi);
                    let new_arg = hlr.add_variable(&raw_arg_type);

                    hlr.insert_statement_before(call_id, MemCpyGen {
                        from: RefGen(arg),
                        to: RefGen(new_arg),
                        size: HNodeData::Lit {
                            lit: HLit::Int(hlr.tree.get_ref(arg).ret_type().size() as u64),
                            var_type: Type::i(64),
                        },
                    });

                    args[a] = hlr.insert_quick(call_id, new_arg);
                }
            }

            let mut call_data = hlr.tree.get(call_id) else { return };
            let HNodeData::Call { a: ref mut oargs, .. } = &mut call_data 
                else { unreachable!() };

            *oargs = args;

            hlr.tree.replace(call_id, call_data);
        }
    );
}
