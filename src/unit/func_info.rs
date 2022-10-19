use crate::lex::VarName;
use crate::Type;

use super::UniqueFuncInfo;

/// Helper struct to define all information about a function.
#[derive(Clone)]
pub struct FuncInfo {
    pub name: VarName,
    pub arg_types: Vec<Type>,
    pub ret_type: Type,
    pub is_method: bool,
}

impl FuncInfo {
    pub fn from(
        name: &VarName,
        arg_types: &Vec<Type>,
        ret_type: &Type,
        is_method: bool,
    ) -> FuncInfo {
        FuncInfo {
            name: name.clone(),
            arg_types: arg_types.clone(),
            ret_type: ret_type.clone(),
            is_method,
        }
    }

    pub fn unique_name(&self) -> String { self.to_unique_func_info().to_string() }

    pub fn to_unique_func_info(&self) -> UniqueFuncInfo {
        UniqueFuncInfo::from(&self.name, &self.arg_types, self.is_method)
    }

    pub fn typ(&self) -> Type { self.ret_type().func_with_args(self.arg_types()) }

    pub fn name(&self) -> VarName { self.name.clone() }
    pub fn arg_types(&self) -> Vec<Type> { self.arg_types.clone() }
    pub fn ret_type(&self) -> Type { self.ret_type.clone() }
}
