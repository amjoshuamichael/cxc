use std::collections::HashSet;

use crate::{lex::VarName, unit::UniqueFuncInfo};

use super::expr_tree::{ExprTree, NodeData};

#[derive(Debug)]
pub struct FuncOutput {
    pub(super) tree: Option<ExprTree>,
    pub(super) arg_names: Option<Vec<VarName>>,
    pub(super) info: Option<UniqueFuncInfo>,
}

impl FuncOutput {
    pub fn get_func_dependencies(&self) -> HashSet<UniqueFuncInfo> {
        let mut output = HashSet::<UniqueFuncInfo>::new();

        let tree = self.tree_ref();

        for (_, node_data) in tree.iter() {
            let NodeData::Call { .. } = node_data
                else { continue; };

            let func_info = tree.unique_func_info_of_call(node_data);
            output.insert(func_info);
        }

        output
    }

    pub fn tree_ref(&self) -> &ExprTree { self.tree.as_ref().unwrap() }
    pub fn arg_names_ref(&self) -> &Vec<VarName> { self.arg_names.as_ref().unwrap() }
    pub fn info_ref(&self) -> &UniqueFuncInfo { self.info.as_ref().unwrap() }

    pub fn take_tree(&mut self) -> ExprTree { self.tree.take().unwrap() }
    pub fn take_arg_names(&mut self) -> Vec<VarName> {
        self.arg_names.take().unwrap()
    }
    pub fn take_info(&mut self) -> UniqueFuncInfo { self.info.take().unwrap() }
}
