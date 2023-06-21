use super::prelude::*;
use crate::hlr::expr_tree::*;
use crate::lex::VarName;
use crate::typ::ArgStyle;

pub fn large_args(hlr: &mut FuncRep) {
    handle_other_calls(hlr);
    handle_own_args(hlr);
}

fn handle_own_args(hlr: &mut FuncRep) {
    let args: Vec<_> = hlr
        .args()
        .into_iter()
        .map(|(name, info)| (name.clone(), info.clone()))
        .collect::<Vec<_>>();

    for (name, info) in args {
        match info.typ.arg_style() {
            ArgStyle::Direct => {},
            ArgStyle::Ints(..) => arg_by_ints(hlr, name),
            ArgStyle::Pointer => arg_by_pointer(hlr, name),
        }
    }
}

fn arg_by_ints(hlr: &mut FuncRep, og_arg: VarName) {
    let arg_load_arg_type = hlr.variables[&og_arg].typ.clone();
    let new_arg_load_var = hlr.add_variable(&*format!("{og_arg}_load"), &arg_load_arg_type);
    let set_arg_load_var = hlr.insert_quick(
        hlr.tree.root,
        SetVarGen {
            lhs: new_arg_load_var.clone(),
            rhs: HNodeData::Ident {
                name: og_arg.clone(),
                var_type: arg_load_arg_type.raw_arg_type(), 
            }
        }
    );

    let HNodeData::Block { ref mut stmts, .. } = hlr.tree.get_mut(hlr.tree.root) 
        else { unreachable!() };
    stmts.insert(0, set_arg_load_var);

    hlr.modify_many_infallible(
        |var_id, var_data, hlr| {
            let HNodeData::Ident { name, .. } = var_data else { return };

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

fn arg_by_pointer(hlr: &mut FuncRep, arg_name: VarName) {
    hlr.variables[&arg_name].typ = hlr.variables[&arg_name].typ.get_ref();

    hlr.modify_many_infallible_rev(
        move |var_id, var_data, hlr| {
            if !matches!(var_data, HNodeData::Ident { name, .. } if name == &arg_name) {
                return;
            }

            hlr.replace_quick(var_id, DerefGen { object: arg_name.clone() });
            *var_data = hlr.tree.get(var_id);
        }
    );
}

fn handle_other_calls(hlr: &mut FuncRep) {
    hlr.modify_many_infallible(
        move |call_id, data, hlr| {
            let HNodeData::Call { a: args, f, .. } = data else { return };

            if f == &VarName::from("cast") {
                return;
            }

            for (a, arg) in args.clone().into_iter().enumerate() {
                let old_arg_type = hlr.tree.get(arg).ret_type();
                let arg_style = old_arg_type.arg_style();

                if arg_style == ArgStyle::Pointer {
                    args[0] = hlr.insert_quick(call_id, get_ref(arg));
                } else if let ArgStyle::Ints(..) = arg_style {
                    let new_arg_name = format!("{f}_arg_{a}");
                    let new_arg = hlr.add_variable(&*new_arg_name, &old_arg_type.raw_arg_type());

                    hlr.insert_statement_before(call_id, SetVarGen {
                        lhs: new_arg.clone(),
                        rhs: arg,
                    });

                    args[a] = hlr.insert_quick(call_id, new_arg);
                }
            }
        }
    );
}
