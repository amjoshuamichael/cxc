use std::collections::BTreeSet;

use crate::{lex::VarName, unit::UniqueFuncInfo, Type, FuncType};

use super::{
    expr_tree::{ExprTree, HNodeData},
    hlr_data::Variables,
};

#[derive(Clone, Debug)]
pub struct FuncOutput {
    pub tree: ExprTree,
    pub data_flow: Variables,
    pub arg_names: Vec<VarName>,
    pub info: UniqueFuncInfo,
    pub func_type: FuncType,
}

impl FuncOutput {
    pub fn get_func_dependencies(&self) -> BTreeSet<UniqueFuncInfo> {
        let mut output = BTreeSet::<UniqueFuncInfo>::new();

        for (_, node_data) in self.tree.iter() {
            let HNodeData::Call { .. } = node_data
                else { continue; };

            let func_info = self.tree.unique_func_info_of_call(node_data);
            output.insert(func_info);
        }

        output
    }
}
