use super::hlr_data::DataFlowInfo;
use super::prelude::*;
use crate::lex::VarName;
use crate::parse::*;
use crate::{typ::FuncType, unit::CompData, Kind, Type, TypeEnum};
use std::{collections::HashMap, rc::Rc};

pub fn handle_large_returns(hlr: &mut FuncRep) { 
    handle_own_return(hlr); 
    handle_other_calls(hlr);
}

fn handle_own_return(hlr: &mut FuncRep) {
    if hlr.ret_type.size() <= 16 {
        return;
    }

    let output_var = VarName::from("T_output");

    hlr.move_up_arg_indices();
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
    for og_id in hlr.tree.ids() {
        let data = hlr.tree.get(og_id);

        let NodeData::Call { mut a, f, generics, is_method, .. } = 
            data.clone() else { continue; };
        if data.ret_type().size() <= 16 { continue };

        let (statement_id, block_id) = hlr.tree.statement_and_block(og_id);

        let call_var = VarName::from(&*(og_id.to_string() + "call_out"));

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

        let call_id = hlr.tree.make_one_space(block_id);
        let ref_id = hlr.tree.make_one_space(call_id);
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
        a = vec![ref_id].drain(..).chain(a.drain(..)).collect();
        hlr.tree.replace(
            call_id,
            NodeData::Call {
                ret_type: Type::never(),
                f,
                generics,
                a,
                is_method,
                return_by_ref: true,
            },
        );

        hlr.tree.replace(og_id, NodeData::Ident { 
            var_type: data.ret_type(), 
            name: call_var.clone() 
        });

        let mut block = hlr.tree.get(block_id);

        if let NodeData::Block { ref mut stmts, .. } = &mut block {
            let block_pos = stmts.iter().position(|s_id| *s_id == statement_id).unwrap();
            stmts.insert(block_pos, call_id);
            stmts.insert(block_pos, make_var_id);
        } else {
            panic!();
        }

        hlr.tree.replace(block_id, block);
    }
}
