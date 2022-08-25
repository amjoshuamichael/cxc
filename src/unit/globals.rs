use crate::hlr::prelude::*;
use inkwell::values::CallableValue;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Default, Debug)]
pub struct Functions<'a>(HashMap<FunctionDef, FuncValAndType<'a>>);

#[derive(Debug)]
pub struct FuncValAndType<'a> {
    pub val: CallableValue<'a>,
    pub ret_type: Type,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct FunctionDef {
    pub name: String,
    pub arg_types: Vec<Type>,
}

impl<'a> Functions<'a> {
    pub fn insert(
        &mut self,
        function_def: FunctionDef,
        ret_type: Type,
        val: CallableValue<'a>,
    ) {
        self.0
            .insert(function_def, FuncValAndType { ret_type, val });
    }

    pub fn name_exists(&self, name: String) -> bool {
        self.funcs_with_name(name).len() >= 0
    }

    pub fn count_with_name(&self, name: String) -> usize {
        self.funcs_with_name(name).len()
    }

    pub fn funcs_with_name(&self, name: String) -> Vec<&FunctionDef> {
        self.0.keys().filter(|def| def.name == name).collect()
    }

    pub fn get_value(&self, def: FunctionDef) -> Option<CallableValue<'a>> {
        Some(self.0.get(&def)?.val.clone())
    }

    pub fn get_type(&self, def: FunctionDef) -> Option<Type> {
        Some(self.0.get(&def)?.ret_type.clone())
    }
}
