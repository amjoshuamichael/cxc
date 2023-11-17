use super::hlr_data::ArgIndex;
use super::hlr_data::VarID;
use super::hlr_data::VariableInfo;
use super::prelude::*;
use crate::hlr::expr_tree::*;
use crate::typ::ABI;
use crate::typ::ReturnStyle;
use crate::Type;

pub fn large_returns(hlr: &mut FuncRep) {
    handle_other_calls(hlr);
    handle_own_return(hlr);
}

fn handle_own_return(hlr: &mut FuncRep) {
    match hlr.ret_type.return_style(ABI::C) {
        ReturnStyle::ThroughI32 |
        ReturnStyle::ThroughI64 |
        ReturnStyle::ThroughF64 if hlr.ret_type != hlr.ret_type.raw_return_type(ABI::C) => {
            return_by_small_cast(hlr, hlr.ret_type.raw_return_type(ABI::C));
        },
        ReturnStyle::ThroughI32I32 |
        ReturnStyle::ThroughI64I32 |
        ReturnStyle::ThroughI64I64 |
        ReturnStyle::ThroughF32F32 |
        ReturnStyle::ThroughF64F32 |
        ReturnStyle::ThroughF64F64 if hlr.ret_type != hlr.ret_type.raw_return_type(ABI::C) => {
            return_by_big_cast(hlr, hlr.ret_type.raw_return_type(ABI::C));
        }
        ReturnStyle::Pointer => return_by_pointer(hlr),
        _ => {},
    }
}

fn return_by_small_cast(hlr: &mut FuncRep, load_as: Type) {
    hlr.modify_many_infallible(
        move |return_id, hlr| {
            let HNodeData::Return { to_return: Some(ref to_return), .. } = 
                hlr.tree.get_ref(return_id) else { return };

            hlr.replace_quick(*to_return, CastGen {
                cast: hlr.tree.get(*to_return),
                to: load_as.clone(),
            });
        }
    );
}

fn return_by_big_cast(hlr: &mut FuncRep, load_as: Type) {
    let casted_ret = hlr.add_variable(&load_as);

    hlr.modify_many_infallible(
        move |return_id, hlr| {
            let HNodeData::Return { to_return: Some(ref to_return), .. } = 
                hlr.tree.get_ref(return_id) else { return };
            let to_return = *to_return;

            hlr.insert_statement_before(return_id, SetGen {
                lhs: casted_ret.clone(),
                rhs: hlr.tree.get(to_return),
            });

            hlr.replace_quick(to_return, casted_ret.clone());
        }
    );
}

fn return_by_pointer(hlr: &mut FuncRep) {
    let output_var = hlr.variables.insert(VariableInfo {
        typ: hlr.ret_type.get_ref(),
        arg_index: ArgIndex::SRet,
        ..Default::default()
    });

    hlr.modify_many_infallible(
        |return_id, hlr| {
            let HNodeData::Return { to_return: Some(ref to_return), .. } = 
                hlr.tree.get_ref(return_id) else { return };

            hlr.insert_statement_before(
                return_id,
                MemCpyGen {
                    from: RefGen(hlr.tree.get(*to_return)),
                    to: output_var,
                    size: HNodeData::Lit {
                        lit: HLit::Int(hlr.ret_type.size() as u64),
                        var_type: Type::i(64),
                    }
                }
            );

            let HNodeData::Return { ref mut to_return, .. } = 
                hlr.tree.get_mut(return_id) else { return };
            *to_return = None;
        },
    );
}

fn handle_other_calls(hlr: &mut FuncRep) {
    let calls = hlr
        .tree
        .ids_in_order()
        .into_iter()
        .rev()
        .filter(|id| matches!(hlr.tree.get_ref(*id), HNodeData::Call { .. }))
        .collect::<Vec<_>>();

    for call_id in calls {
        let data = hlr.tree.get(call_id);

        if let HNodeData::Call { call: HCallable::Direct(ref query), .. } = data &&
            hlr.comp_data.name_is_intrinsic(&query.name) {
                continue;
        }
        let abi = hlr.abi_of_call(call_id);

        match data.ret_type().return_style(abi) {
            ReturnStyle::Pointer => format_call_returning_pointer(hlr, call_id),
            ReturnStyle::ThroughI32
            | ReturnStyle::ThroughI64
            | ReturnStyle::ThroughF64
            | ReturnStyle::ThroughI32I32
            | ReturnStyle::ThroughF32F32
            | ReturnStyle::ThroughF64F64
            | ReturnStyle::ThroughF64F32
            | ReturnStyle::ThroughI64I32
            | ReturnStyle::ThroughI64I64 => {
                format_call_returning_struct(hlr, call_id, abi);
            },
            ReturnStyle::Direct | ReturnStyle::Void => {},
        }
    }
}

fn format_call_returning_struct(hlr: &mut FuncRep, og_call: ExprID, abi: ABI) {
    let mut og_call_data = hlr.tree.get(og_call);

    let casted_var_type = og_call_data.ret_type();
    let casted_var_name = hlr.add_variable(&casted_var_type);

    let raw_ret_var_type = casted_var_type.raw_return_type(abi);
    let raw_ret_var_name = hlr.add_variable(&raw_ret_var_type);

    *og_call_data.ret_type_mut().unwrap() = raw_ret_var_type.clone();
    
    hlr.insert_statement_before(
        og_call,
        SetGen {
            lhs: raw_ret_var_name.clone(),
            rhs: og_call_data.clone(),
        },
    )
    .after_that(MemCpyGen {
        from: RefGen(HNodeData::Ident {
            var_type: raw_ret_var_type.clone(),
            var_id: raw_ret_var_name,
        }),
        to: RefGen(HNodeData::Ident {
            var_type: casted_var_type.clone(),
            var_id: casted_var_name.clone(),
        }),
        size: HNodeData::Lit {
            lit: HLit::Int(casted_var_type.size().min(raw_ret_var_type.size()) as u64),
            var_type: Type::i(64),
        },
    });

    hlr.tree.replace(
        og_call,
        HNodeData::Ident {
            var_type: casted_var_type,
            var_id: casted_var_name,
        },
    );
}

fn format_call_returning_pointer(hlr: &mut FuncRep, og_call_id: ExprID) {
    let mut new_data = hlr.tree.get(og_call_id);
    let HNodeData::Call { ref mut sret, ref mut ret_type, .. } = 
        &mut new_data else { unreachable!(); };

    let parent = hlr.tree.parent(og_call_id);

    if let HNodeData::Set { ref lhs, .. } = hlr.tree.get(parent) {
        hlr.tree.replace(parent, HNodeData::zero());
        let new_sret = hlr.insert_quick(og_call_id, RefGen(*lhs));
        *sret = Some(new_sret);
        *ret_type = Type::void();
        hlr.replace_quick(parent, new_data);
    } else {
        let call_var = hlr.add_variable(ret_type);

        let new_arg = hlr.insert_quick(og_call_id, RefGen(call_var.clone()));

        *sret = Some(new_arg);

        *ret_type = Type::void();
        hlr.insert_statement_before(og_call_id, new_data);
        hlr.replace_quick(og_call_id, call_var);
    }
}
