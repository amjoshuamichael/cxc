use super::hlr_data::ArgIndex;
use super::hlr_data::VariableInfo;
use super::struct_literals::struct_literals;
use super::prelude::*;
use crate::hlr::expr_tree::*;
use crate::lex::VarName;
use crate::typ::ReturnStyle;
use crate::Type;
use crate::FuncQuery;

pub fn large_returns(hlr: &mut FuncRep) {
    handle_other_calls(hlr);
    handle_own_return(hlr);
}

#[cfg_attr(debug_assertions, inline(never))]
fn handle_own_return(hlr: &mut FuncRep) {
    match hlr.ret_type.return_style() {
        ReturnStyle::ThroughI32
        | ReturnStyle::ThroughI64
        | ReturnStyle::ThroughF32F32
        | ReturnStyle::ThroughI32I32
        | ReturnStyle::ThroughI64I32
        | ReturnStyle::ThroughI64I64 if hlr.ret_type != hlr.ret_type.raw_return_type() => {
            return_by_cast(hlr, hlr.ret_type.raw_return_type());
        },
        ReturnStyle::MoveIntoDouble => return_by_move_into_double(hlr),
        ReturnStyle::MoveIntoI64I64 => return_by_move_into_i64i64(hlr),
        ReturnStyle::Sret => return_by_pointer(hlr),
        _ => {},
    }
}

fn return_by_cast(hlr: &mut FuncRep, load_as: Type) {
    let casted_ret = hlr.add_variable(&load_as, hlr.tree.root);

    hlr.modify_many_infallible(
        move |return_id, data, hlr| {
            let HNodeData::Return { to_return: Some(ref mut to_return), .. } = data else { return };

            hlr.insert_statement_before(return_id, SetGen {
                lhs: casted_ret.clone(),
                rhs: hlr.tree.get(*to_return),
            });

            hlr.replace_quick(*to_return, casted_ret.clone());
        }
    );
}

fn return_by_move_into_double(hlr: &mut FuncRep) {
    let f64 = Type::f(64);
    let casted_ret = hlr.add_variable(&f64, hlr.tree.root);

    hlr.modify_many_infallible(
        move |return_id, data, hlr| {
            let HNodeData::Return { to_return: Some(ref mut to_return), .. } = data else { return };

            hlr.insert_statement_before(return_id, SetGen {
                lhs: casted_ret.clone(),
                rhs: CallGen {
                    query: FuncQuery {
                        name: VarName::from("cast"),
                        generics: vec![hlr.ret_type.clone(), f64.clone()],
                        ..Default::default()
                    },
                    args: vec![Box::new(*to_return)],
                    ..Default::default()
                },
            });

            *data = HNodeData::Ident {
                var_id: casted_ret.clone(),
                var_type: f64.clone(),
            };
        }
    );
}

fn return_by_move_into_i64i64(hlr: &mut FuncRep) {
    let i64i64 = Type::new_tuple(vec![Type::i(64), Type::i(64)]);

    let output_var_name = hlr.add_variable(&i64i64, hlr.tree.root);

    hlr.modify_many_infallible(
        |return_id, mut data, hlr| {
            let HNodeData::Return { to_return: Some(ref mut to_return), .. } = &mut data
                else { return };

            let to_return_data = hlr.tree.get(*to_return);

            hlr.insert_statement_before(
                return_id,
                SetGen {
                    lhs: output_var_name.clone(),
                    rhs: to_return_data.clone(),
                },
            );

            let data_to_return = hlr.insert_quick(
                return_id,
                StructLitGen {
                    var_type: i64i64.clone(),
                    ..StructLitGen::tuple(vec![
                        Box::new(MemberGen {
                            object: output_var_name.clone(),
                            field: VarName::TupleIndex(0),
                        }),
                        Box::new(MemberGen {
                            object: output_var_name.clone(),
                            field: VarName::TupleIndex(1),
                        }),
                    ])
                },
            );

            *to_return = data_to_return;
        },
    );

    struct_literals(hlr)
}

fn return_by_pointer(hlr: &mut FuncRep) {
    let output_var = hlr.variables.insert(VariableInfo {
        typ: hlr.ret_type.get_ref(),
        arg_index: ArgIndex::SRet,
        ..Default::default()
    });

    hlr.modify_many_infallible(
        |return_id, data, hlr| {
            let HNodeData::Return { ref mut to_return, .. } = data else { return };

            let Some(ref mut to_return_inner) = to_return else { return };

            hlr.insert_statement_before(
                return_id,
                MemCpyGen {
                    from: RefGen(hlr.tree.get(*to_return_inner)),
                    to: output_var,
                    size: HNodeData::Number {
                        lit_type: Type::i(64),
                        value: hlr.ret_type.size() as u64,
                    }
                }
            );

            *to_return = None;
        },
    );
}

#[cfg_attr(debug_assertions, inline(never))]
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

        if let HNodeData::Call { ref query, .. } = data 
            && !hlr.comp_data.name_is_intrinsic(&query.name) {
            match data.ret_type().return_style() {
                ReturnStyle::Sret => format_call_returning_pointer(hlr, call_id),
                ReturnStyle::MoveIntoI64I64 => todo!(),
                ReturnStyle::MoveIntoDouble => {},
                ReturnStyle::ThroughI32
                | ReturnStyle::ThroughI64
                | ReturnStyle::ThroughI32I32
                | ReturnStyle::ThroughF32F32
                | ReturnStyle::ThroughI64I32
                | ReturnStyle::ThroughI64I64 => {
                    format_call_returning_struct(hlr, call_id);
                },
                ReturnStyle::Direct | ReturnStyle::Void => {},
            }
        }
    }
}

fn format_call_returning_struct(hlr: &mut FuncRep, og_call: ExprID) {
    let og_call_data = hlr.tree.get(og_call);

    let casted_var_type = og_call_data.ret_type();
    let casted_var_name = hlr.add_variable(&casted_var_type, og_call);

    let raw_ret_var_type = casted_var_type.raw_return_type();
    let raw_ret_var_name = hlr.add_variable(&raw_ret_var_type, og_call);

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
        size: HNodeData::Number {
            lit_type: Type::i(64),
            value: casted_var_type.size().min(raw_ret_var_type.size()) as u64,
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
        let new_sret = hlr.insert_quick(og_call_id, RefGen(*lhs));
        *sret = Some(new_sret);
        *ret_type = Type::void();
        hlr.replace_quick(parent, new_data);
    } else {
        let call_var = hlr.add_variable(ret_type, og_call_id);

        let new_arg = hlr.insert_quick(og_call_id, RefGen(call_var.clone()));

        *sret = Some(new_arg);

        *ret_type = Type::void();
        hlr.insert_statement_before(og_call_id, new_data);
        hlr.replace_quick(og_call_id, call_var);
    }
}
