use std::collections::{HashSet};

use slotmap::SlotMap;

use crate::{unit::FuncQuery, FuncType};

use super::{
    expr_tree::ExprTree,
    hlr_data::{VarID, VariableInfo},
};

#[derive(Clone, Debug)]
pub struct HLR {
    pub tree: ExprTree,
    pub data_flow: SlotMap<VarID, VariableInfo>,
    pub func_type: FuncType,
    pub dependencies: HashSet<FuncQuery>,
}
