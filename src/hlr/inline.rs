use std::collections::HashMap;

use slotmap::SecondaryMap;

use crate::hlr::expr_tree::HNodeData;

use super::{hlr_data::{ArgIndex, FuncRep, VarID}, expr_tree::{ExprID, ExprTree}, hlr_data_output::HLR};

pub fn inline(
    new_tree: &HLR, 
    base_tree: &mut FuncRep,
    arguments: HashMap<ArgIndex, ExprID>,
) -> HNodeData {
    let mut inline_info = InlineInfo {
        new_tree,
        base_tree,
        arguments: &arguments,
        variable_remap: SecondaryMap::new(),
    };

    inline_node(new_tree.tree.root, &mut inline_info)
}

// these are packed into a struct in order to make the recursion cleaner
struct InlineInfo<'a, 'b> {
    new_tree: &'a HLR,
    base_tree: &'a mut FuncRep<'b>,
    arguments: &'a HashMap<ArgIndex, ExprID>,
    variable_remap: SecondaryMap<VarID, VarID>,
}

pub fn inline_node(
    node: ExprID,
    inline_info: &mut InlineInfo,
) -> HNodeData {
    use HNodeData::*;
    let mut node_data = inline_info.new_tree.tree.get(node);
    match &mut node_data {
        Number { .. } | 
        Float { .. } | 
        Bool { .. } | 
        GlobalLoad { .. } | 
        AccessAlias(_) | 
        GotoLabel(_) | 
        Goto(_) => {},
        StructLit { fields, .. } => {
            for (_, field_id) in fields {
                replace_with_new_id(field_id, node, inline_info);
            }
        },
        ArrayLit { parts: many, .. } |
        Block { stmts: many, .. } => {
            for id in many {
                replace_with_new_id(id, node, inline_info);
            }
        },
        Set { lhs, rhs } => {
            replace_with_new_id(lhs, node, inline_info);
            replace_with_new_id(rhs, node, inline_info);
        },
        Call { a, sret, .. } => {
            for a in a {
                replace_with_new_id(a, node, inline_info);
            }
            if let Some(sret) = sret {
                replace_with_new_id(sret, node, inline_info);
            }
        },
        IndirectCall { a, sret, .. } => {
            for a in a {
                replace_with_new_id(a, node, inline_info);
            }
            if let Some(sret) = sret {
                replace_with_new_id(sret, node, inline_info);
            }
        },
        UnarOp { hs: x, .. } | Transform { hs: x, .. } | Member { object: x, .. } => {
            replace_with_new_id(x, node, inline_info);
        }
        BinOp { lhs: l, rhs: r, .. } | 
        Set { lhs: l, rhs: r, .. } | 
        While { w: l, d: r, .. } | 
        Index { object: l, index: r, .. } => {
            replace_with_new_id(l, node, inline_info);
            replace_with_new_id(r, node, inline_info);
        }
        IfThenElse { i, t, e, .. } => {
            replace_with_new_id(i, node, inline_info);
            replace_with_new_id(t, node, inline_info);
            replace_with_new_id(e, node, inline_info);
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

            if let Some(argument) = inline_info.arguments.get(&arg_index) {
                node_data = inline_info.base_tree.tree.get(*argument);
            } else if let Some(existing_remap) = inline_info.variable_remap.get(*var_id) {
                *var_id = *existing_remap;
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
    let new_field_data = inline_node(*id, inline_info);
    *id = inline_info.base_tree.tree.insert(parent, new_field_data);
}
