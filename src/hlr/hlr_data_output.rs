use std::collections::{HashSet};

use crate::{lex::VarName, unit::FuncQuery, FuncType};

use super::{
    expr_tree::{ExprTree},
    hlr_data::Variables,
};

#[derive(Clone, Debug)]
pub struct HLR {
    pub tree: ExprTree,
    pub data_flow: Variables,
    pub arg_names: Vec<VarName>,
    pub func_type: FuncType,
    pub dependencies: HashSet<FuncQuery>,
}
