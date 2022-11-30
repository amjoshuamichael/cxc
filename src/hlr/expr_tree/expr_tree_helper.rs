use crate::hlr::hlr_data::FuncRep;
use crate::{Type, UniqueFuncInfo};

use super::{ExprID, ExprTree, NodeData, NodeDataGen};
use super::{ExprNode, NodeData::*};

impl ExprTree {
    pub fn iter_mut<'a>(
        &'a mut self,
    ) -> Box<dyn DoubleEndedIterator<Item = (ExprID, &mut NodeData)> + 'a> {
        Box::new(
            self.nodes
                .iter_mut()
                .enumerate()
                .map(|(id, node)| (ExprID(id), &mut node.data)),
        )
    }

    pub fn iter<'a>(
        &'a self,
    ) -> Box<dyn DoubleEndedIterator<Item = (ExprID, &NodeData)> + 'a> {
        Box::new(
            self.nodes
                .iter()
                .enumerate()
                .map(|(id, node)| (ExprID(id), &node.data)),
        )
    }

    pub fn ids(&self) -> impl DoubleEndedIterator<Item = ExprID> {
        (0..self.node_count()).map(ExprID)
    }

    pub fn insert(&mut self, parent: ExprID, data: NodeData) -> ExprID {
        self.nodes.push(ExprNode { parent, data });
        ExprID(self.nodes.len() - 1)
    }

    pub fn replace(&mut self, at: ExprID, with: NodeData) {
        self.nodes[at.0].data = with;
    }

    pub fn make_one_space(&mut self, parent: ExprID) -> ExprID {
        self.nodes.push(ExprNode {
            parent,
            data: NodeData::Empty,
        });
        ExprID(self.nodes.len() - 1)
    }

    pub fn get(&self, at: ExprID) -> NodeData { self.nodes[at.0].data.clone() }

    pub fn get_ref(&self, at: ExprID) -> &NodeData { &self.nodes[at.0].data }

    pub fn get_mut(&mut self, at: ExprID) -> &mut NodeData {
        &mut self.nodes[at.0].data
    }

    pub fn parent(&self, of: ExprID) -> ExprID { self.nodes[of.0].parent }

    pub fn statement_and_block(&self, of: ExprID) -> (ExprID, ExprID) {
        let parent = self.parent(of);

        if parent == ExprID::ROOT {
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

    pub fn return_type(&self) -> Type { self.get(ExprID::ROOT).ret_type() }

    fn node_count(&self) -> usize { self.nodes.len() }

    pub fn unique_func_info_of_call(&self, call: &NodeData) -> UniqueFuncInfo {
        let NodeData::Call { f, generics, relation, .. } = call.clone()
            else { panic!() };

        UniqueFuncInfo {
            name: f,
            relation,
            generics,
        }
    }
}

impl<'a> FuncRep<'a> {
    pub fn modify_many(
        &mut self,
        filter: impl Fn(&NodeData) -> bool,
        modifier: impl Fn(ExprID, &mut NodeData, &mut FuncRep),
    ) {
        self.modify_many_inner(self.tree.ids(), filter, modifier)
    }

    pub fn modify_many_rev(
        &mut self,
        filter: impl Fn(&NodeData) -> bool,
        modifier: impl Fn(ExprID, &mut NodeData, &mut FuncRep),
    ) {
        self.modify_many_inner(self.tree.ids().rev(), filter, modifier)
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

    pub fn insert_statement_before<'b>(
        &'b mut self,
        statement_origin: ExprID,
        new_data: impl NodeDataGen,
    ) -> InsertionData<'b, 'a> {
        let new_statement =
            self.insert_statement_inner(statement_origin, new_data, 0);
        InsertionData(self, new_statement)
    }

    pub fn insert_statement_after<'b>(
        &'b mut self,
        statement_origin: ExprID,
        new_data: impl NodeDataGen,
    ) -> InsertionData<'b, 'a> {
        let new_statement =
            self.insert_statement_inner(statement_origin, new_data, 1);
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

        let block_pos =
            stmts.iter().position(|s_id| *s_id == statement).unwrap() + offset;
        stmts.insert(block_pos, new);

        new
    }
}

pub struct InsertionData<'a, 'b>(&'a mut FuncRep<'b>, ExprID);

impl<'a, 'b> InsertionData<'a, 'b> {
    pub fn after_that(self, new_data: impl NodeDataGen) -> InsertionData<'a, 'b>
    where
        'b: 'a,
    {
        self.0.insert_statement_after(self.1, new_data)
    }

    pub fn inserted_id(self) -> ExprID { self.1 }
}
