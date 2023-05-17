use super::struct_literals::struct_literals;
use super::prelude::*;
use crate::hlr::expr_tree::*;
use crate::lex::VarName;
use crate::parse::*;
use crate::typ::ReturnStyle;
use crate::Type;
use crate::UniqueFuncInfo;

pub fn large_returns(hlr: &mut FuncRep) {
    handle_other_calls(hlr);
    handle_own_return(hlr);
}

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
    let casted_ret = hlr.add_variable("casted_ret", &load_as);

    hlr.modify_many_infallible(
        move |return_id, data, hlr| {
            let HNodeData::Return { to_return: Some(ref mut to_return), .. } = data else { return };

            hlr.insert_statement_before(return_id, SetVarGen {
                lhs: box casted_ret.clone(),
                rhs: box hlr.tree.get(*to_return),
            });

            hlr.replace_quick(*to_return, casted_ret.clone());
        }
    );
}

fn return_by_move_into_double(hlr: &mut FuncRep) {
    let f64 = Type::f(64);
    let casted_ret = hlr.add_variable("casted_ret", &f64);

    hlr.modify_many_infallible(
        move |return_id, data, hlr| {
            let HNodeData::Return { to_return: Some(ref mut to_return), .. } = data else { return };

            hlr.insert_statement_before(return_id, SetVarGen {
                lhs: box casted_ret.clone(),
                rhs: box CallGen {
                    info: UniqueFuncInfo {
                        name: VarName::from("cast"),
                        generics: vec![hlr.tree.get(*to_return).ret_type(), f64.clone()],
                        ..Default::default()
                    },
                    args: vec![box hlr.tree.get(*to_return)],
                },
            });

            *data = HNodeData::Ident {
                name: casted_ret.clone(),
                var_type: f64.clone(),
            };
        }
    );
}

fn return_by_move_into_i64i64(hlr: &mut FuncRep) {
    let i32i64 = Type::new_tuple(vec![Type::i(32), Type::i(64)]);
    let i64i64 = Type::new_tuple(vec![Type::i(64), Type::i(64)]);

    let output_var_name = hlr.add_variable("casted_ret", &i32i64);

    hlr.modify_many_infallible(
        |return_id, mut data, hlr| {
            let HNodeData::Return { to_return: Some(ref mut to_return), .. } = &mut data
                else { return };

            let to_return_data = hlr.tree.get(*to_return);

            hlr.insert_statement_before(
                return_id,
                SetVarGen {
                    lhs: box output_var_name.clone(),
                    rhs: box to_return_data.clone(),
                },
            );

            let data_to_return = hlr.insert_quick(
                return_id,
                StructLitGen {
                    var_type: i64i64.clone(),
                    ..StructLitGen::tuple(vec![
                        box MemberGen {
                            object: box output_var_name.clone(),
                            field: 0.into(),
                        },
                        box MemberGen {
                            object: box output_var_name.clone(),
                            field: 1.into(),
                        }
                    ])
                },
            );

            *to_return = data_to_return;
        },
    );

    struct_literals(hlr)
}

fn return_by_pointer(hlr: &mut FuncRep) {
    let output_var = hlr.add_argument("Sret", hlr.ret_type.clone().get_ref());

    hlr.modify_many_infallible(
        |return_id, data, hlr| {
            let HNodeData::Return { ref mut to_return, .. } = data else { return };

            let Some(ref mut to_return_inner) = to_return else { return };

            hlr.insert_statement_before(
                return_id,
                SetVarGen {
                    lhs: get_deref(output_var.clone()),
                    rhs: box hlr.tree.get(*to_return_inner),
                },
            );

            *to_return = None;
        },
    );
}

fn handle_other_calls(hlr: &mut FuncRep) {
    for call_id in hlr.tree.ids_in_order().drain(..).rev() {
        let data = hlr.tree.get(call_id);

        if let HNodeData::Call { ref f, .. } = data && !hlr.comp_data.name_is_intrinsic(f) {
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
    let casted_var_name = 
        hlr.add_variable("casted_call_out", &casted_var_type);

    let raw_ret_var_type = casted_var_type.raw_return_type();
    let raw_ret_var_name = 
        hlr.add_variable("raw_call_out", &raw_ret_var_type);

    hlr.insert_statement_before(
        og_call,
        SetVarGen {
            lhs: box raw_ret_var_name.clone(),
            rhs: box og_call_data.clone(),
        },
    )
    .after_that(CallGen {
        info: UniqueFuncInfo {
            name: VarName::from("memcpy"),
            generics: vec![raw_ret_var_type.clone()],
            relation: TypeRelation::Unrelated,
            ..Default::default()
        },
        args: vec![
            get_ref(HNodeData::Ident {
                var_type: raw_ret_var_type.clone(),
                name: raw_ret_var_name,
            }),
            get_ref(HNodeData::Ident {
                var_type: casted_var_type.clone(),
                name: casted_var_name.clone(),
            }),
            box raw_ret_var_type.size()
        ],
    });

    hlr.tree.replace(
        og_call,
        HNodeData::Ident {
            var_type: casted_var_type,
            name: casted_var_name,
        },
    );
}

fn format_call_returning_pointer(hlr: &mut FuncRep, og_call_id: ExprID) {
    let mut new_data = hlr.tree.get(og_call_id);
    let HNodeData::Call { ref mut a, ret_type, .. } = 
        &mut new_data else { unreachable!(); };

    let call_var = hlr.add_variable("_call_out", &ret_type);

    let new_arg = hlr.insert_quick(og_call_id, RefGen {
        object: box call_var.clone(),
    });

    a.insert(0, new_arg);

    hlr.insert_statement_before(og_call_id, new_data);
    hlr.replace_quick(og_call_id, call_var);
}
