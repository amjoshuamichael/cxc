use crate::errors::CResultMany;
use crate::hlr::hlr_data::{FuncRep, VariableInfo, ArgIndex};
use crate::{Type, UniqueFuncInfo, VarName};

use super::{ExprID, ExprTree, HNodeData, NodeDataGen};
use super::{ExprNode, HNodeData::*};

impl ExprTree {
    pub fn iter_mut<'a>(
        &'a mut self,
    ) -> Box<dyn Iterator<Item = (ExprID, &mut HNodeData)> + 'a> {
        box self.nodes.iter_mut().map(|(id, node)| (id, &mut node.data))
    }

    pub fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = (ExprID, &HNodeData)> + 'a> {
        box self.nodes.iter().map(|(id, node)| (id, &node.data))
    }

    pub fn ids_in_order(&self) -> Vec<ExprID> {
        use HNodeData::*;

        fn ids_of(tree: &ExprTree, id: ExprID) -> Vec<ExprID> {
            let rest = match tree.get(id) {
                Number { .. } | Float { .. } | Bool { .. } | Ident { .. } => Vec::new(),
                StructLit { fields, .. } => fields
                    .iter()
                    .flat_map(|(_, id)| ids_of(tree, *id))
                    .collect(),
                ArrayLit { parts: many, .. }
                | Call { a: many, .. }
                | Block { stmts: many, .. } => {
                    many.iter().flat_map(|id| ids_of(tree, *id)).collect()
                },
                IndirectCall {
                    f: one, a: many, ..
                } => many
                    .iter()
                    .flat_map(|id| ids_of(tree, *id))
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
                UnarOp { hs: one, .. }
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
    pub fn set_parent(&mut self, of: ExprID, to: ExprID) { self.nodes[of].parent = to }

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
        closure: impl Fn(ExprID, &mut Self) -> HNodeData,
    ) -> ExprID {
        let new_space = self.make_one_space(parent);
        let new_data = closure(new_space, self);
        self.replace(new_space, new_data);
        new_space
    }

    pub fn unique_func_info_of_call(&self, call: &HNodeData) -> UniqueFuncInfo {
        let HNodeData::Call { f, generics, relation, .. } = call.clone()
            else { panic!() };

        UniqueFuncInfo {
            name: f,
            relation,
            generics,
            ..Default::default()
        }
    }
}

impl<'a> FuncRep<'a> {
    pub fn modify_many(
        &mut self,
        modifier: impl Fn(ExprID, &mut HNodeData, &mut FuncRep) -> CResultMany<()>,
    ) -> CResultMany<()> {
        self.modify_many_inner(self.tree.ids_in_order().drain(..), |a, b, c| { modifier(a, b, c) })
    }

    pub fn modify_many_rev(
        &mut self,
        modifier: impl Fn(ExprID, &mut HNodeData, &mut FuncRep) -> CResultMany<()>,
    ) -> CResultMany<()> {
        self.modify_many_inner(self.tree.ids_in_order().drain(..).rev(), |a, b, c| { modifier(a, b, c) })
    }

    pub fn modify_many_infallible(
        &mut self,
        modifier: impl Fn(ExprID, &mut HNodeData, &mut FuncRep),
    ) {
        self.modify_many(|a, b, c| { modifier(a, b, c); Ok(()) }).unwrap();
    }

    pub fn modify_many_infallible_rev(
        &mut self,
        modifier: impl Fn(ExprID, &mut HNodeData, &mut FuncRep),
    ) {
        self.modify_many_rev(|a, b, c| { modifier(a, b, c); Ok(()) }).unwrap();
    }

    fn modify_many_inner(
        &mut self,
        id_iterator: impl Iterator<Item = ExprID>,
        modifier: impl Fn(ExprID, &mut HNodeData, &mut FuncRep) -> CResultMany<()>,
    ) -> CResultMany<()> {
        for index in id_iterator {
            let mut data_copy = self.tree.get(index);

            modifier(index, &mut data_copy, self)?;

            // TODO: remove this??
            self.tree.replace(index, data_copy);
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

    pub fn add_variable(&mut self, name: &str, typ: &Type) -> VarName {
        let new_name = self.uniqueify_varname(name);

        self.variables.insert(new_name.clone(), VariableInfo {
            typ: typ.clone(),
            arg_index: ArgIndex::None,
        });

        new_name.clone()
    }

    fn uniqueify_varname(&mut self, name: &str) -> VarName {
        let name = VarName::from(name);
        let mut uniqueified = name.clone();
        let mut unique_id = 0;

        while self.variables.contains_key(&uniqueified) {
            uniqueified = VarName::from(&*format!("{name}{unique_id}"));
            unique_id += 1;
        }

        uniqueified
    }
}

pub struct InsertionData<'ptr, 'a>(&'ptr mut FuncRep<'a>, ExprID);

impl<'ptr, 'a> InsertionData<'ptr, 'a> {
    pub fn after_that(self, new_data: impl NodeDataGen) -> Self {
        self.0.insert_statement_after(self.1, new_data)
    }

    pub fn inserted_id(self) -> ExprID { self.1 }
}
