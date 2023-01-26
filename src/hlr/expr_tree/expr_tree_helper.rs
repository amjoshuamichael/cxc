use crate::hlr::hlr_data::FuncRep;
use crate::{Type, UniqueFuncInfo};

use super::{ExprID, ExprTree, NodeData, NodeDataGen};
use super::{ExprNode, NodeData::*};

impl ExprTree {
    pub fn iter_mut<'a>(
        &'a mut self,
    ) -> Box<dyn Iterator<Item = (ExprID, &mut NodeData)> + 'a> {
        box self.nodes.iter_mut().map(|(id, node)| (id, &mut node.data))
    }

    pub fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = (ExprID, &NodeData)> + 'a> {
        box self.nodes.iter().map(|(id, node)| (id, &node.data))
    }

    pub fn ids_in_order(&self) -> Vec<ExprID> {
        use NodeData::*;

        fn ids_of<'a>(tree: &'a ExprTree, id: ExprID) -> Vec<ExprID> {
            let rest = match tree.get(id) {
                Number { .. } | Float { .. } | Bool { .. } | Ident { .. } => Vec::new(),
                StructLit { fields, .. } => fields
                    .iter()
                    .map(|(_, id)| ids_of(tree, *id))
                    .flatten()
                    .collect(),
                ArrayLit { parts: many, .. }
                | Call { a: many, .. }
                | Block { stmts: many, .. } => {
                    many.iter().map(|id| ids_of(tree, *id)).flatten().collect()
                },
                FirstClassCall {
                    f: one, a: many, ..
                } => many
                    .iter()
                    .map(|id| ids_of(tree, *id))
                    .flatten()
                    .chain(ids_of(tree, one).drain(..))
                    .collect(),
                BinOp { lhs: l, rhs: r, .. }
                | Set { lhs: l, rhs: r, .. }
                | IfThen { i: l, t: r, .. }
                | While { w: l, d: r, .. }
                | Index {
                    object: l,
                    index: r,
                    ..
                } => ids_of(tree, l)
                    .drain(..)
                    .chain(ids_of(tree, r).drain(..))
                    .collect(),
                MakeVar { rhs: one, .. }
                | UnarOp { hs: one, .. }
                | Member { object: one, .. } => ids_of(tree, one),
                IfThenElse { i, t, e, .. } => ids_of(tree, i)
                    .drain(..)
                    .chain(ids_of(tree, t).drain(..))
                    .chain(ids_of(tree, e).drain(..))
                    .collect(),
                Return { to_return, .. } => {
                    if let Some(to_return) = to_return {
                        ids_of(tree, to_return)
                    } else {
                        Vec::new()
                    }
                },
            };

            [id].into_iter().chain(rest.into_iter()).collect()
        }

        ids_of(self, self.root)
    }

    pub fn insert(&mut self, parent: ExprID, data: NodeData) -> ExprID {
        self.nodes.insert(ExprNode { parent, data })
    }

    pub fn replace(&mut self, at: ExprID, with: NodeData) {
        self.nodes.get_mut(at).unwrap().data = with;
    }

    pub fn make_one_space(&mut self, parent: ExprID) -> ExprID {
        self.nodes.insert(ExprNode {
            parent,
            data: NodeData::Number { value: 0, size: 32 },
        })
    }

    pub fn get(&self, at: ExprID) -> NodeData { self.nodes[at].data.clone() }

    pub fn get_ref(&self, at: ExprID) -> &NodeData { &self.nodes[at].data }

    pub fn get_mut(&mut self, at: ExprID) -> &mut NodeData { &mut self.nodes[at].data }

    pub fn parent(&self, of: ExprID) -> ExprID { self.nodes[of].parent }

    pub fn statement_and_block(&self, of: ExprID) -> (ExprID, ExprID) {
        let parent = self.parent(of);

        if parent == self.root {
            return (of, parent);
        }

        if matches!(self.get(parent), Block { .. }) {
            (of, parent)
        } else {
            self.statement_and_block(parent)
        }
    }

    pub fn with_space(
        &mut self,
        parent: ExprID,
        closure: impl Fn(ExprID, &mut Self) -> NodeData,
    ) -> ExprID {
        let new_space = self.make_one_space(parent);
        let new_data = closure(new_space, self);
        self.replace(new_space, new_data);
        new_space
    }

    pub fn return_type(&self) -> Type { self.get(self.root).ret_type() }

    pub fn unique_func_info_of_call(&self, call: &NodeData) -> UniqueFuncInfo {
        let NodeData::Call { f, generics, relation, .. } = call.clone()
            else { panic!() };

        UniqueFuncInfo {
            name: f,
            relation,
            own_generics: generics,
        }
    }
}

impl FuncRep {
    pub fn modify_many(
        &mut self,
        filter: impl Fn(&NodeData) -> bool,
        modifier: impl Fn(ExprID, &mut NodeData, &mut FuncRep),
    ) {
        self.modify_many_inner(self.tree.ids_in_order().drain(..), filter, modifier)
    }

    pub fn modify_many_rev(
        &mut self,
        filter: impl Fn(&NodeData) -> bool,
        modifier: impl Fn(ExprID, &mut NodeData, &mut FuncRep),
    ) {
        self.modify_many_inner(self.tree.ids_in_order().drain(..).rev(), filter, modifier)
    }

    fn modify_many_inner(
        &mut self,
        id_iterator: impl Iterator<Item = ExprID>,
        filter: impl Fn(&NodeData) -> bool,
        modifier: impl Fn(ExprID, &mut NodeData, &mut FuncRep),
    ) {
        for index in id_iterator {
            let data_ref = &self.tree.get_ref(index);

            if filter(data_ref) {
                let mut data_copy = self.tree.get(index);

                modifier(index, &mut data_copy, self);

                self.tree.replace(index, data_copy);
            }
        }
    }

    pub fn insert_statement_before<'a>(
        &'a mut self,
        statement_origin: ExprID,
        new_data: impl NodeDataGen,
    ) -> InsertionData<'a> {
        let new_statement = self.insert_statement_inner(statement_origin, new_data, 0);
        InsertionData(self, new_statement)
    }

    pub fn insert_statement_after<'a>(
        &'a mut self,
        statement_origin: ExprID,
        new_data: impl NodeDataGen,
    ) -> InsertionData<'a> {
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

        let NodeData::Block { ref mut stmts, .. } = self.tree.get_mut(block)
            else { unreachable!() };

        let block_pos = stmts.iter().position(|s_id| *s_id == statement).unwrap() + offset;
        stmts.insert(block_pos, new);

        new
    }
}

pub struct InsertionData<'a>(&'a mut FuncRep, ExprID);

impl<'a> InsertionData<'a> {
    pub fn after_that(self, new_data: impl NodeDataGen) -> InsertionData<'a> {
        self.0.insert_statement_after(self.1, new_data)
    }

    pub fn inserted_id(self) -> ExprID { self.1 }
}
