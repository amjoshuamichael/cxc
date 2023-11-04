use std::{collections::HashSet, sync::Arc};

use slotmap::SlotMap;

use crate::{unit::FuncQuery, FuncType, parse::Expr};

use super::{
    expr_tree::ExprTree,
    hlr_data::{VarID, VariableInfo},
};

#[derive(Clone, Debug)]
pub struct HLR {
    pub from: Arc<Expr>,
    pub tree: ExprTree,
    pub variables: SlotMap<VarID, VariableInfo>,
    pub func_type: FuncType,
    pub dependencies: HashSet<FuncQuery>,
}
