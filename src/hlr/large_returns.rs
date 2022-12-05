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
        ReturnStyle::ThroughI64
        | ReturnStyle::ThroughI64I32
        | ReturnStyle::ThroughI64I64 => {
            change_ret_type(hlr, hlr.ret_type.raw_return_type())
        },
        ReturnStyle::Pointer => return_by_pointer(hlr),
        ReturnStyle::Void | ReturnStyle::Direct => {},
    }
}

fn change_ret_type(hlr: &mut FuncRep, typ: Type) {
    hlr.ret_type = typ.clone();
    let NodeData::Block { ref mut ret_type, .. } = hlr.tree.get_mut(ExprID::ROOT)
        else { panic!() };

    *ret_type = typ;
}

fn return_by_pointer(hlr: &mut FuncRep) {
    let output_var = VarName::from("T_output");

    for (_, flow) in hlr.data_flow.iter_mut() {
        if let Some(arg_index) = &mut flow.arg_index {
            *arg_index += 1;
        }
    }

    hlr.data_flow.insert(
        output_var.clone(),
        DataFlowInfo {
            typ: hlr.ret_type.clone(),
            arg_index: Some(0),
        },
    );

    hlr.modify_many(
        |data| matches!(data, NodeData::Return { .. }),
        |return_id, data, hlr| {
            let NodeData::Return { ref mut to_return, .. } = data
            else { unreachable!() };

            hlr.insert_statement_before(
                return_id,
                SetVarGen {
                    lhs: box NodeData::Ident {
                        var_type: hlr.ret_type.clone().get_ref(),
                        name: output_var.clone(),
                    },
                    rhs: box hlr.tree.get(to_return.unwrap()),
                },
            );

            *to_return = None;
        },
    );
}

fn handle_other_calls(hlr: &mut FuncRep) {
    for call_id in hlr.tree.ids() {
        let data = hlr.tree.get(call_id);
        if !matches!(data, NodeData::Call { .. }) {
            continue;
        };

        match data.ret_type().return_style() {
            ReturnStyle::Pointer => format_call_returning_pointer(hlr, call_id),
            ReturnStyle::ThroughI64
            | ReturnStyle::ThroughI64I32
            | ReturnStyle::ThroughI64I64 => {
                format_call_returning_struct(hlr, call_id);
            },
            ReturnStyle::Direct | ReturnStyle::Void => {},
        }
    }
}

fn format_call_returning_struct(hlr: &mut FuncRep, og_call: ExprID) {
    let og_call_data = hlr.tree.get(og_call);

    let raw_ret_var_name = VarName::from(&*(og_call.to_string() + "raw_call_out"));
    let raw_ret_var_type = og_call_data.ret_type().raw_return_type();
    let casted_var_name = VarName::from(&*(og_call.to_string() + "casted_call_out"));
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
        },
        args: vec![
            box UnarOpGen {
                ret_type: raw_ret_var_type.get_ref(),
                op: Opcode::Ref(1),
                hs: box NodeData::Ident {
                    var_type: raw_ret_var_type.clone(),
                    name: raw_ret_var_name.clone(),
                },
            },
            box UnarOpGen {
                ret_type: raw_ret_var_type.get_ref(),
                op: Opcode::Ref(1),
                hs: box NodeData::Ident {
                    var_type: casted_var_type.clone(),
                    name: casted_var_name.clone(),
                },
            },
            box NodeData::Number {
                value: raw_ret_var_type.size() as u128,
                size: 32,
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

    let call_var = VarName::from(&*(og_call_id.to_string() + "call_out"));

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
        op: Opcode::Ref(1),
        hs: box NodeData::Ident {
            var_type: call_ret_type.clone(),
            name: call_var.clone(),
        },
        ret_type: call_ret_type.get_ref(),
    });

    for arg in old_args {
        new_args.push(box arg);
    }

    hlr.insert_statement_before(
        og_call_id,
        CallGen {
            info: UniqueFuncInfo {
                name: f,
                generics,
                relation,
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
