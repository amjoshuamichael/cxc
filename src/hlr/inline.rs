use std::collections::HashMap;

use slotmap::SecondaryMap;

use crate::{hlr::expr_tree::{HNodeData, SetGen, RefGen}, typ::ArgStyle, ABI};

use super::{hlr_data::{ArgIndex, FuncRep, VarID}, expr_tree::{ExprID, ExprTree}, hlr_data_output::HLR};

pub fn inline(
    new_tree: &HLR, 
    base_tree: &mut FuncRep,
    arguments: HashMap<ArgIndex, ExprID>,
    put_into: ExprID,
) {
    let mut inline_info = InlineInfo {
        new_tree,
        base_tree,
        arguments: &arguments,
        variable_remap: SecondaryMap::new(),
    };

    let new_data = inline_node(new_tree.tree.root, put_into, &mut inline_info);
    inline_info.base_tree.tree.replace(put_into, new_data);
}

// these are packed into a struct in order to make the recursion cleaner
struct InlineInfo<'a, 'b> {
    new_tree: &'a HLR,
    base_tree: &'a mut FuncRep<'b>,
    arguments: &'a HashMap<ArgIndex, ExprID>,
    variable_remap: SecondaryMap<VarID, VarID>,
}

pub fn inline_node(
    og_id: ExprID,
    new_id: ExprID,
    inline_info: &mut InlineInfo,
) -> HNodeData {
    use HNodeData::*;
    let mut node_data = inline_info.new_tree.tree.get(og_id);
    match &mut node_data {
        Lit { .. } | 
        GlobalLoad { .. } | 
        AccessAlias(_) | 
        GotoLabel(_) | 
        Goto(_) => {},
        StructLit { fields, .. } => {
            for (_, field_id) in fields {
                replace_with_new_id(field_id, new_id, inline_info);
            }
        },
        ArrayLit { parts: many, .. } |
        Block { stmts: many, .. } => {
            for id in many {
                replace_with_new_id(id, new_id, inline_info);
            }
        },
        Set { lhs, rhs } => {
            replace_with_new_id(lhs, new_id, inline_info);
            replace_with_new_id(rhs, new_id, inline_info);
        },
        Call { a, sret, .. } => {
            for a in a {
                replace_with_new_id(a, new_id, inline_info);
            }
            if let Some(sret) = sret {
                replace_with_new_id(sret, new_id, inline_info);
            }
        },
        UnarOp { hs: x, .. } | Transform { hs: x, .. } | Member { object: x, .. } => {
            replace_with_new_id(x, new_id, inline_info);
        }
        BinOp { lhs: l, rhs: r, .. } | 
        Set { lhs: l, rhs: r, .. } | 
        While { w: l, d: r, .. } | 
        Index { object: l, index: r, .. } => {
            replace_with_new_id(l, new_id, inline_info);
            replace_with_new_id(r, new_id, inline_info);
        }
        IfThenElse { i, t, e, .. } => {
            replace_with_new_id(i, new_id, inline_info);
            replace_with_new_id(t, new_id, inline_info);
            replace_with_new_id(e, new_id, inline_info);
        },
        Return { ret_type, to_return } => {
            if let Some(to_return) = to_return {
                panic!();
            } else {
                // TODO: this is a hack
                node_data = HNodeData::new_block();
            }
        },
        Ident { var_type, var_id } => {
            let arg_index = inline_info.new_tree.variables[*var_id].arg_index;

            if let Some(existing_remap) = inline_info.variable_remap.get(*var_id) {
                *var_id = *existing_remap;
            } else if let Some(argument) = inline_info.arguments.get(&arg_index) {
                let argument_type = inline_info.base_tree.tree.get_ref(*argument).ret_type();
                match argument_type.arg_style(ABI::C) {
                    ArgStyle::Direct | ArgStyle::Ints(..) | ArgStyle::Floats(..) | ArgStyle::Void => {
                        node_data = inline_info.base_tree.tree.get(*argument);
                    },
                    ArgStyle::Pointer => {
                        let new_var = inline_info.base_tree.add_variable(var_type);
                        inline_info.base_tree.insert_statement_before(
                            new_id,
                            SetGen {
                                lhs: new_var,
                                rhs: RefGen(*argument),
                            }
                        );
                        inline_info.variable_remap.insert(*var_id, new_var);
                        *var_id = new_var;
                    },
                }
            } else {
                let new_var = inline_info.base_tree.add_variable(var_type);
                inline_info.variable_remap.insert(*var_id, new_var);
                *var_id = new_var;
            }
        },
    }

    node_data
}

fn replace_with_new_id(id: &mut ExprID, parent: ExprID, inline_info: &mut InlineInfo) {
    let og_id = *id;
    let space = inline_info.base_tree.tree.make_one_space(parent);
    *id = space;
    let new_data = inline_node(og_id, space, inline_info);
    inline_info.base_tree.tree.replace(space, new_data);
}
