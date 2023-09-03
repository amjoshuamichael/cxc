use std::collections::BTreeSet;

use crate::{lex::VarName, unit::FuncQuery, FuncType, TypeRelation, Type};

use super::{
    expr_tree::{ExprTree, HNodeData},
    hlr_data::Variables,
};

#[derive(Clone, Debug)]
pub struct HLR {
    pub tree: ExprTree,
    pub data_flow: Variables,
    pub arg_names: Vec<VarName>,
    pub func_type: FuncType,
}
