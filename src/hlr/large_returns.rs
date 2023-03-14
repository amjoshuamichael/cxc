use super::handle_struct_literals::handle_struct_literals;
use super::hlr_data::DataFlowInfo;
use super::prelude::*;
use crate::hlr::expr_tree::*;
use crate::lex::VarName;
use crate::parse::*;
use crate::typ::ReturnStyle;
use crate::Type;
use crate::UniqueFuncInfo;

pub fn handle_large_returns(hlr: &mut FuncRep) {
    handle_other_calls(hlr);
    handle_own_return(hlr);
}

fn handle_own_return(hlr: &mut FuncRep) {
    match hlr.ret_type.return_style() {
        ReturnStyle::ThroughI32
        | ReturnStyle::ThroughI64
        | ReturnStyle::ThroughI64I32
        | ReturnStyle::ThroughI64I64 => return_by_through(hlr, hlr.ret_type.raw_return_type()),
        ReturnStyle::MoveIntoI64I64 => return_by_move_into_i64i64(hlr),
        ReturnStyle::Sret => return_by_pointer(hlr),
        ReturnStyle::Void | ReturnStyle::Direct => {},
    }
}

fn return_by_through(hlr: &mut FuncRep, typ: Type) {
    hlr.ret_type = typ.clone();
    let NodeData::Block { ref mut ret_type, .. } = hlr.tree.get_mut(hlr.tree.root)
        else { panic!() };

    *ret_type = typ;
}

fn return_by_move_into_i64i64(hlr: &mut FuncRep) {
    let i32i64 = Type::new_tuple(vec![Type::i(32), Type::i(64)]);
    let i64i64 = Type::new_tuple(vec![Type::i(64), Type::i(64)]);

    let output_var_name = VarName::from("casted_ret");
    let output_var_node_data = box NodeData::Ident {
        var_type: i32i64.clone(),
        name: output_var_name.clone(),
    };

    hlr.data_flow.insert(
        output_var_name.clone(),
        DataFlowInfo {
            typ: i64i64.clone(),
            arg_index: None,
        },
    );

    hlr.modify_many(
        |data| matches!(data, NodeData::Return { .. }),
        |return_id, mut data, hlr| {
            let NodeData::Return { to_return: Some(ref mut to_return), .. } = &mut data
            else { unreachable!() };

            let to_return_data = hlr.tree.get(*to_return);

            hlr.insert_statement_before(
                return_id,
                MakeVarGen {
                    var_type: i32i64.clone(),
                    set: output_var_name.clone(),
                    to: box to_return_data.clone(),
                    ..Default::default()
                },
            );

            let data_to_return = hlr.insert_quick(
                return_id,
                StructLitGen {
                    var_type: i64i64.clone(),
                    fields: vec![
                        (
                            VarName::from("0"),
                            box MemberGen {
                                object: output_var_node_data.clone(),
                                field: VarName::from("0"),
                                ret_type: Type::i(32),
                            },
                        ),
                        (
                            VarName::from("1"),
                            box MemberGen {
                                object: output_var_node_data.clone(),
                                field: VarName::from("1"),
                                ret_type: Type::i(64),
                            },
                        ),
                    ],
                    ..Default::default()
                },
            );

            *to_return = data_to_return;
        },
    );

    handle_struct_literals(hlr);
}

fn return_by_pointer(hlr: &mut FuncRep) {
    let output_var = VarName::from("Sret");

    for (_, flow) in hlr.data_flow.iter_mut() {
        if let Some(arg_index) = &mut flow.arg_index {
            *arg_index += 1;
        }
    }

    let output_var_typ = hlr.ret_type.clone().get_ref();

    hlr.data_flow.insert(
        output_var.clone(),
        DataFlowInfo {
            typ: output_var_typ.clone(),
            arg_index: Some(0),
        },
    );

    hlr.modify_many(
        |data| matches!(data, NodeData::Return { .. }),
        |return_id, data, hlr| {
            let NodeData::Return { to_return: Some(to_return), .. } = data
            else { unreachable!() };

            hlr.insert_statement_before(
                return_id,
                CallGen {
                    info: UniqueFuncInfo {
                        name: VarName::from("write"),
                        relation: TypeRelationGeneric::MethodOf(output_var_typ.get_ref()),
                        generics: vec![hlr.ret_type.clone()],
                        ..Default::default()
                    },
                    args: vec![
                        box UnarOpGen {
                            hs: box NodeData::Ident {
                                var_type: output_var_typ.clone(),
                                name: output_var.clone(),
                            },
                            op: Opcode::Ref,
                            ret_type: output_var_typ.get_ref(),
                        },
                        box hlr.tree.get(*to_return),
                    ],
                },
            );

            let NodeData::Return { ref mut to_return, .. } = data
            else { unreachable!() };

            *to_return = None;
        },
    );
}

fn handle_other_calls(hlr: &mut FuncRep) {
    for call_id in hlr.tree.ids_in_order().drain(..).rev() {
        let data = hlr.tree.get(call_id);

        if let NodeData::Call { ref f, .. } = data && !hlr.comp_data.name_is_intrinsic(&f) {
            match data.ret_type().return_style() {
                ReturnStyle::Sret => format_call_returning_pointer(hlr, call_id),
                ReturnStyle::MoveIntoI64I64 => todo!(),
                ReturnStyle::ThroughI32
                | ReturnStyle::ThroughI64
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

    let raw_ret_var_name = hlr.uniqueify_varname("raw_call_out");
    let raw_ret_var_type = og_call_data.ret_type().raw_return_type();
    let casted_var_name = hlr.uniqueify_varname("casted_call_out");
    let casted_var_type = og_call_data.ret_type();

    hlr.insert_statement_before(
        og_call,
        MakeVarGen {
            set: raw_ret_var_name.clone(),
            to: box og_call_data.clone(),
            var_type: raw_ret_var_type.clone(),
            ..Default::default()
        },
    )
    .after_that(MakeVarGen {
        set: casted_var_name.clone(),
        var_type: casted_var_type.clone(),
        ..Default::default()
    })
    .after_that(CallGen {
        info: UniqueFuncInfo {
            name: VarName::from("memcpy"),
            generics: vec![raw_ret_var_type.clone()],
            relation: TypeRelation::Unrelated,
            ..Default::default()
        },
        args: vec![
            box UnarOpGen {
                ret_type: raw_ret_var_type.get_ref(),
                op: Opcode::Ref,
                hs: box NodeData::Ident {
                    var_type: raw_ret_var_type.clone(),
                    name: raw_ret_var_name.clone(),
                },
            },
            box UnarOpGen {
                ret_type: raw_ret_var_type.get_ref(),
                op: Opcode::Ref,
                hs: box NodeData::Ident {
                    var_type: casted_var_type.clone(),
                    name: casted_var_name.clone(),
                },
            },
            box NodeData::Number {
                lit_type: Type::i(32),
                value: raw_ret_var_type.size() as u64,
            },
        ],
    });

    hlr.tree.replace(
        og_call,
        NodeData::Ident {
            var_type: casted_var_type,
            name: casted_var_name,
        },
    );
}

fn format_call_returning_pointer(hlr: &mut FuncRep, og_call_id: ExprID) {
    let data = hlr.tree.get(og_call_id);

    let NodeData::Call { a: old_args, f, generics, relation, ret_type: call_ret_type } = 
        data.clone() else { panic!(); };

    let call_var = hlr.uniqueify_varname("call_out");

    hlr.insert_statement_before(
        og_call_id,
        MakeVarGen {
            set: call_var.clone(),
            var_type: data.ret_type(),
            ..Default::default()
        },
    );

    let mut new_args: Vec<Box<dyn NodeDataGen>> = Vec::new();

    new_args.push(box UnarOpGen {
        op: Opcode::Ref,
        hs: box NodeData::Ident {
            var_type: call_ret_type.clone(),
            name: call_var.clone(),
        },
        ret_type: call_ret_type.get_ref(),
    });

    for arg in old_args {
        new_args.push(box hlr.tree.get(arg));
    }

    hlr.insert_statement_before(
        og_call_id,
        CallGen {
            info: UniqueFuncInfo {
                name: f,
                generics,
                relation,
                ..Default::default()
            },
            args: new_args,
        },
    );

    hlr.tree.replace(
        og_call_id,
        NodeData::Ident {
            var_type: call_ret_type,
            name: call_var.clone(),
        },
    );
}
