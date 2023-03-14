use std::collections::BTreeSet;

use crate::{lex::VarName, unit::UniqueFuncInfo, Type};

use super::{
    expr_tree::{ExprTree, NodeData},
    hlr_data::DataFlow,
};

#[derive(Debug)]
pub struct FuncOutput {
    pub(super) tree: Option<ExprTree>,
    pub(super) data_flow: Option<DataFlow>,
    pub(super) arg_names: Option<Vec<VarName>>,
    pub(super) info: Option<UniqueFuncInfo>,
    pub func_type: Type,
}

impl FuncOutput {
    pub fn get_func_dependencies(&self) -> BTreeSet<UniqueFuncInfo> {
        let mut output = BTreeSet::<UniqueFuncInfo>::new();

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
    pub fn take_arg_names(&mut self) -> Vec<VarName> { self.arg_names.take().unwrap() }
    pub fn take_info(&mut self) -> UniqueFuncInfo { self.info.take().unwrap() }
    pub fn take_data_flow(&mut self) -> DataFlow { self.data_flow.take().unwrap() }
}
