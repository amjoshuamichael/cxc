use super::hlr_data::DataFlowInfo;
use super::prelude::*;
use crate::lex::VarName;
use crate::parse::*;
use crate::Type;
use crate::typ::ReturnStyle;

pub fn handle_large_returns(hlr: &mut FuncRep) { 
    handle_other_calls(hlr);
    handle_own_return(hlr);
}

fn handle_own_return(hlr: &mut FuncRep) {
    match hlr.ret_type.return_style() {
        ReturnStyle::ThroughI64 
        | ReturnStyle::ThroughI64I32 
        | ReturnStyle::ThroughI64I64 => 
            change_ret_type(hlr, hlr.ret_type.raw_return_type()),
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

    for id in hlr.tree.ids() {
        let mut data = hlr.tree.get(id);

        if let NodeData::Return {
            ref mut to_return, ..
        } = &mut data
        {
            let set_var = hlr.tree.make_one_space(id);
            let lhs = hlr.tree.insert(
                set_var,
                NodeData::Ident {
                    var_type: hlr.ret_type.clone().get_ref(),
                    name: output_var.clone(),
                },
            );
            let rhs = hlr.tree.insert(set_var, hlr.tree.get(to_return.unwrap()));

            hlr.tree.replace(
                set_var,
                NodeData::SetVar {
                    ret_type: hlr.ret_type.clone(),
                    lhs,
                    rhs,
                },
            );

            let block_id = hlr.tree.statement_and_block(id).1;
            let mut block = hlr.tree.get(block_id);

            if let NodeData::Block { ref mut stmts, .. } = &mut block {
                let block_pos = stmts.iter().position(|s_id| *s_id == id).unwrap();
                stmts.insert(block_pos, set_var);
            } else {
                panic!();
            }

            hlr.tree.replace(block_id, block);

            *to_return = None;
        }

        hlr.tree.replace(id, data);
    }
}

fn handle_other_calls(hlr: &mut FuncRep) {
    for call_id in hlr.tree.ids() {
        let data = hlr.tree.get(call_id);
        if !matches!(data, NodeData::Call { .. }) { continue };

        match data.ret_type().return_style() {
            ReturnStyle::Pointer => format_call_returning_pointer(hlr, call_id),
            ReturnStyle::ThroughI64 | 
            ReturnStyle::ThroughI64I32 | 
            ReturnStyle::ThroughI64I64 => {
                format_call_returning_struct(hlr, call_id);
            },
            ReturnStyle::Direct | ReturnStyle::Void => {}
        }
        
    }
}

fn format_call_returning_struct(hlr: &mut FuncRep, og_call: ExprID) {
    let og_call_data = hlr.tree.get(og_call);
    let (call_statement, block) = hlr.tree.statement_and_block(og_call);

    let raw_ret_var_name = VarName::from(&*(og_call.to_string() + "raw_call_out"));
    let raw_ret_var_type = og_call_data.ret_type().raw_return_type();

    let make_raw_ret_var = {
        let make_var = hlr.tree.make_one_space(block);

        let new_call = hlr.tree.insert(
            make_var, 
            og_call_data.clone(),
        );

        hlr.tree.replace(make_var, NodeData::MakeVar { 
            var_type: raw_ret_var_type.clone(), 
            name: raw_ret_var_name.clone(), 
            rhs: new_call 
        });

        make_var
    };

    let casted_var_name = VarName::from(&*(og_call.to_string() + "casted_call_out"));
    let casted_var_type = og_call_data.ret_type();

    let make_casted_var = {
        let make_var = hlr.tree.make_one_space(block);

        let nothing = hlr.tree.insert(
            make_var, 
            NodeData::Number { value: 0, size: 32 },
        );

        hlr.tree.replace(make_var, NodeData::MakeVar { 
            var_type: casted_var_type.clone(), 
            name: casted_var_name.clone(), 
            rhs: nothing 
        });

        make_var
    };
    
    let memcpy = {
        let call = hlr.tree.make_one_space(block);
        
        let src_ref = {
            let src_ref = hlr.tree.make_one_space(call);

            let src = hlr.tree.insert(src_ref, NodeData::Ident { 
                var_type: raw_ret_var_type.clone(), 
                name: raw_ret_var_name.clone() 
            });

            hlr.tree.replace(src_ref, NodeData::UnarOp { 
                ret_type: raw_ret_var_type.get_ref(), 
                op: Opcode::Ref(1), 
                hs: src 
            });

            src_ref
        };

        let dest_ref = {
            let dest_ref = hlr.tree.make_one_space(call);

            let dest = hlr.tree.insert(src_ref, NodeData::Ident { 
                var_type: casted_var_type.clone(), 
                name: casted_var_name.clone() 
            });

            hlr.tree.replace(dest_ref, NodeData::UnarOp { 
                ret_type: casted_var_type.get_ref(), 
                op: Opcode::Ref(1), 
                hs: dest 
            });

            dest_ref
        };

        let size = hlr.tree.insert(
            call, 
            NodeData::Number { 
                value: raw_ret_var_type.size() as u128, 
                size: 32 
            }
        );

        hlr.tree.replace(
            call, 
            NodeData::Call { 
                ret_type: Type::never(), 
                f: VarName::from("memcpy"), 
                generics: vec![raw_ret_var_type.clone()], 
                a: vec![src_ref, dest_ref, size], 
                is_method: false
            }
        );

        call
    };

    hlr.tree.replace(
        og_call, 
        NodeData::Ident { 
            var_type: casted_var_type, 
            name: casted_var_name 
        }
    );

    let mut new_block_data = hlr.tree.get(block);
    let NodeData::Block { ref mut stmts, .. } = &mut new_block_data else { panic!() };
    let call_position = stmts.iter().position(|stmt| *stmt == call_statement).unwrap();
    stmts.insert(call_position, make_raw_ret_var);
    stmts.insert(call_position + 1, make_casted_var);
    stmts.insert(call_position + 2, memcpy);
    hlr.tree.replace(block, new_block_data)
}

fn format_call_returning_pointer(hlr: &mut FuncRep, og_call_id: ExprID) {
    let data = hlr.tree.get(og_call_id);
    let NodeData::Call { a, f, generics, is_method, .. } = 
        data.clone() else { panic!(); };

    let data = hlr.tree.get(og_call_id);

    let (statement_id, block_id) = hlr.tree.statement_and_block(og_call_id);

    let call_var = VarName::from(&*(og_call_id.to_string() + "call_out"));

    let make_var_id = hlr.tree.make_one_space(block_id);
    let none = hlr
        .tree
        .insert(make_var_id, NodeData::Number { value: 0, size: 0 });
    hlr.tree.replace(
        make_var_id,
        NodeData::MakeVar {
            var_type: data.ret_type(),
            name: call_var.clone(),
            rhs: none,
        },
    );

    let new_call_id = hlr.tree.make_one_space(block_id);
    let ref_id = hlr.tree.make_one_space(og_call_id);
    let new_call_arg_id = hlr.tree.insert(
        ref_id,
        NodeData::Ident {
            var_type: data.ret_type(),
            name: call_var.clone(),
        },
    );
    hlr.tree.replace(
        ref_id,
        NodeData::UnarOp {
            ret_type: data.ret_type().clone().get_ref(),
            op: Opcode::Ref(1),
            hs: new_call_arg_id,
        },
    );
    let a = vec![ref_id].drain(..).chain({a}.drain(..)).collect();
    hlr.tree.replace(
        new_call_id,
        NodeData::Call {
            ret_type: Type::never(),
            f,
            generics,
            a,
            is_method,
        },
    );

    hlr.tree.replace(og_call_id, NodeData::Ident { 
        var_type: data.ret_type(), 
        name: call_var.clone() 
    });

    let mut block = hlr.tree.get(block_id);

    if let NodeData::Block { ref mut stmts, .. } = &mut block {
        let block_pos = stmts.iter().position(|s_id| *s_id == statement_id).unwrap();
        stmts.insert(block_pos, new_call_id);
        stmts.insert(block_pos, make_var_id);
    } else {
        panic!();
    }

    hlr.tree.replace(block_id, block);
}
