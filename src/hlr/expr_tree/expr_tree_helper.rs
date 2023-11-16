use crate::errors::CResultMany;
use crate::hlr::hlr_data::{FuncRep, VariableInfo, ArgIndex, VarID, GotoLabelID};
use crate::{Type, VarName};

use super::{ExprID, ExprTree, HNodeData, NodeDataGen, SetGen};
use super::{ExprNode, HNodeData::*};

impl ExprTree {
    pub fn iter_mut<'a>(
        &'a mut self,
    ) -> Box<dyn Iterator<Item = (ExprID, &mut HNodeData)> + 'a> {
        Box::new(self.nodes.iter_mut().map(|(id, node)| (id, &mut node.data)))
    }

    pub fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = (ExprID, &HNodeData)> + 'a> {
        Box::new(self.nodes.iter().map(|(id, node)| (id, &node.data)))
    }

    pub fn ids_in_order(&self) -> Vec<ExprID> {
        let mut ids = Vec::with_capacity(self.nodes.len());
        self.ids_of(self.root, &mut ids);
        ids
    }

    pub fn ids_of(&self, id: ExprID, ids: &mut Vec<ExprID>) {
        use HNodeData::*;
        ids.push(id);
        let rest = match self.get_ref(id) {
            Number { .. } 
            | Float { .. } 
            | Bool { .. } 
            | GlobalLoad { .. }
            | Ident { .. } 
            | AccessAlias(_) 
            | Goto(_) 
            | GotoLabel(_) => {},
            StructLit { fields, .. } => {
                for field in fields {
                    self.ids_of(field.1, ids);
                }
            }
            ArrayLit { parts: many, .. }
            | Call { a: many, .. }
            | Block { stmts: many, .. } => {
                for id in many {
                    self.ids_of(*id, ids);
                }
            },
            IndirectCall {
                f: one, a: many, ..
            } => {
                self.ids_of(*one, ids);

                for id in many {
                    self.ids_of(*id, ids);
                }
            },
            BinOp { lhs: l, rhs: r, .. }
            | Set { lhs: l, rhs: r, .. }
            | While { w: l, d: r, .. }
            | Index {
                object: l,
                index: r,
                ..
            } => {
                self.ids_of(*l, ids);
                self.ids_of(*r, ids);
            },
            UnarOp { hs: one, .. }
            | Transform { hs: one, .. }
            | Member { object: one, .. } => self.ids_of(*one, ids),
            IfThenElse { i, t, e, .. } => {
                self.ids_of(*i, ids);
                self.ids_of(*t, ids);
                self.ids_of(*e, ids);
            }
            Return { to_return, .. } => {
                if let Some(to_return) = to_return {
                    self.ids_of(*to_return, ids)
                }
            },
        };
    }
    
    pub fn ids_unordered(&self) -> slotmap::basic::Keys<ExprID, ExprNode> {
        self.nodes.keys()
    }

    pub fn insert(&mut self, parent: ExprID, data: HNodeData) -> ExprID {
        self.nodes.insert(ExprNode { parent, data })
    }

    pub fn replace(&mut self, at: ExprID, with: HNodeData) {
        self.nodes.get_mut(at).unwrap().data = with;
    }

    pub fn make_one_space(&mut self, parent: ExprID) -> ExprID {
        self.nodes.insert(ExprNode {
            parent,
            data: HNodeData::Number {
                value: 0,
                lit_type: Type::i(32),
            },
        })
    }

    pub fn get(&self, at: ExprID) -> HNodeData { self.nodes[at].data.clone() }

    pub fn get_ref(&self, at: ExprID) -> &HNodeData { &self.nodes[at].data }

    pub fn get_mut(&mut self, at: ExprID) -> &mut HNodeData { &mut self.nodes[at].data }

    pub fn parent(&self, of: ExprID) -> ExprID { self.nodes[of].parent }

    pub fn statement_and_block(&self, of: ExprID) -> (ExprID, ExprID) {
        if of == self.root {
            return (of, of);
        }

        let parent = self.parent(of);

        if matches!(self.get_ref(parent), Block { .. }) {
            (of, parent)
        } else {
            self.statement_and_block(parent)
        }
    }

    pub fn block_of(&self, of: ExprID) -> ExprID {
        if matches!(self.get_ref(of), HNodeData::Block { .. }) {
            return of;
        }

        self.statement_and_block(of).1
    }

    pub fn count(&self) -> usize { self.nodes.len() }

    pub fn remove_node(&mut self, remove_id: ExprID) -> Result<(), ()> {
        let parent = self.get_mut(self.parent(remove_id));
        match dbg!(parent) {
            Block { stmts, .. } => {
                let old_len = stmts.len();
                stmts.retain(|id| *id != remove_id);
                assert_eq!(stmts.len(), old_len - 1);
            }
            _ => return Err(()),
        };
        Ok(())
    }

    pub fn prune(&mut self) {
        let ids = self.ids_in_order();
        self.nodes.retain(|key, _| ids.contains(&key));
    }
}

impl<'a> FuncRep<'a> {
    pub fn modify_many(
        &mut self,
        mut modifier: impl FnMut(ExprID, &mut FuncRep) -> CResultMany<()>,
    ) -> CResultMany<()> {
        self.modify_many_inner(self.tree.ids_in_order().drain(..), |a, c| { modifier(a, c) })
    }

    pub fn modify_many_rev(
        &mut self,
        mut modifier: impl FnMut(ExprID, &mut FuncRep) -> CResultMany<()>,
    ) -> CResultMany<()> {
        self.modify_many_inner(self.tree.ids_in_order().drain(..).rev(), |a, c| { modifier(a, c) })
    }

    pub fn modify_many_infallible(
        &mut self,
        mut modifier: impl FnMut(ExprID, &mut FuncRep),
    ) {
        self.modify_many(|a, c| { modifier(a, c); Ok(()) }).unwrap();
    }

    pub fn modify_many_infallible_rev(
        &mut self,
        mut modifier: impl FnMut(ExprID, &mut FuncRep),
    ) {
        self.modify_many_rev(|a, c| { modifier(a, c); Ok(()) }).unwrap();
    }

    fn modify_many_inner(
        &mut self,
        id_iterator: impl Iterator<Item = ExprID>,
        mut modifier: impl FnMut(ExprID, &mut FuncRep) -> CResultMany<()>,
    ) -> CResultMany<()> {
        for id in id_iterator {
            modifier(id, self)?;
        }

        Ok(())
    }

    pub fn insert_statement_before<'ptr>(
        &'ptr mut self,
        statement_origin: ExprID,
        new_data: impl NodeDataGen,
    ) -> InsertionData<'ptr, 'a> {
        let new_statement = self.insert_statement_inner(statement_origin, new_data, 0);
        InsertionData(self, new_statement)
    }

    pub fn insert_statement_after<'ptr>(
        &'ptr mut self,
        statement_origin: ExprID,
        new_data: impl NodeDataGen,
    ) -> InsertionData<'ptr, 'a> {
        let new_statement = self.insert_statement_inner(statement_origin, new_data, 1);
        InsertionData(self, new_statement)
    }

    fn insert_statement_inner(
        &mut self,
        statement_origin: ExprID,
        new_data: impl NodeDataGen,
        offset: usize,
    ) -> ExprID {
        let (statement, block) = self.tree.statement_and_block(statement_origin);

        let new = self.insert_quick(block, new_data);

        let HNodeData::Block { ref mut stmts, .. } = self.tree.get_mut(block)
            else { unreachable!() };

        let block_pos = stmts.iter().position(|s_id| *s_id == statement).unwrap() + offset;
        stmts.insert(block_pos, new);

        new
    }

    pub fn add_variable(&mut self, typ: &Type) -> VarID {
        let var = self.variables.insert(VariableInfo {
            typ: typ.clone(),
            arg_index: ArgIndex::None,
            ..Default::default()
        });

        let HNodeData::Block { ref mut declared, .. } = self.tree.get_mut(self.tree.root)
            else { unreachable!() };
        declared.insert(var);

        var
    }

    pub fn add_goto_label(&mut self, name: VarName, use_block_of: ExprID) -> GotoLabelID {
        let label = self.goto_labels.insert(use_block_of);

        let block = if matches!(self.tree.get_ref(use_block_of), HNodeData::Block { .. }) {
            use_block_of
        } else {
            self.tree.statement_and_block(use_block_of).1
        };

        let HNodeData::Block { ref mut goto_labels, .. } = self.tree.get_mut(block)
            else { unreachable!() };
        goto_labels.insert(name, label);

        label
    }

    pub fn separate_expression(&mut self, expression: ExprID) -> ExprID {
        let expr_data = self.tree.get(expression);
        
        use HNodeData::*;
        if matches!(expr_data, Ident { .. } | Number { .. } | Float { .. } | Bool { .. }) {
            return expression;
        }

        let new_var = self.add_variable(&expr_data.ret_type());

        self.insert_statement_before(expression, SetGen {
            lhs: new_var,
            rhs: expr_data,
        });
        self.replace_quick(expression, new_var);
        self.insert_quick(expression, new_var)
    }
}

pub struct InsertionData<'ptr, 'a>(&'ptr mut FuncRep<'a>, ExprID);

impl<'ptr, 'a> InsertionData<'ptr, 'a> {
    pub fn after_that(self, new_data: impl NodeDataGen) -> Self {
        self.0.insert_statement_after(self.1, new_data)
    }

    pub fn inserted_id(self) -> ExprID { self.1 }
}
