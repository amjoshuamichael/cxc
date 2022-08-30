use crate::hlr::prelude::*;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::CallableValue;
use std::collections::HashMap;
use std::sync::Arc;

type ValAndType<'a> = (Type, CallableValue<'a>);

#[derive(Default, Debug)]
pub struct Functions<'a> {
    compiled: HashMap<UniqueFuncInfo, ValAndType<'a>>,
    generics: HashMap<String, GenFuncDecl>,
}

impl<'a> Functions<'a> {
    pub fn insert(&mut self, func_info: FuncInfo, val: CallableValue<'a>) {
        self.compiled
            .insert(func_info.to_unique_func_info(), (func_info.ret_type(), val));
    }

    pub fn insert_placeholder(
        &mut self,
        unique_name: UniqueFuncInfo,
        ret_type: Type,
        context: &'a Context,
        module: &Module<'a>,
    ) {
        let function = module.add_function(
            &*unique_name.to_string(),
            context.void_type().fn_type(&[], false),
            None,
        );

        self.compiled
            .insert(unique_name, (ret_type, CallableValue::from(function)));
    }

    pub fn insert_generic(&mut self, decl: GenFuncDecl) {
        self.generics.insert(decl.name.clone(), decl);
    }

    pub fn get_generic(&mut self, dep: GenFuncDependency) -> Option<FuncDecl> {
        let gen_decl = self.generics.get(&dep.name)?.clone();
        let func_decl = gen_decl.realize(dep.types);
        Some(func_decl)
    }

    pub fn name_exists(&self, name: String) -> bool {
        self.funcs_with_name(name).len() > 0
    }

    pub fn count_with_name(&self, name: String) -> usize {
        self.funcs_with_name(name).len()
    }

    pub fn funcs_with_name(&self, name: String) -> Vec<&UniqueFuncInfo> {
        self.compiled
            .keys()
            .filter(|u_name| u_name.og_name() == name)
            .collect()
    }

    pub fn get_value(&'a self, name: UniqueFuncInfo) -> Option<CallableValue<'a>> {
        self.get_func(name).map(|t| t.1.clone())
    }

    pub fn get_type(&self, name: UniqueFuncInfo) -> Option<Type> {
        self.get_func(name).map(|t| t.0.clone())
    }

    fn get_func(&self, name: UniqueFuncInfo) -> Option<&ValAndType> {
        self.compiled.get(&name)
    }
}

use std::hash::Hash;

use super::*;

#[derive(Debug, Clone)]
pub struct UniqueFuncInfo {
    og_name: String,
    arg_types: Vec<Type>,
    is_method: bool,
}

impl Hash for UniqueFuncInfo {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.to_string().hash(state);
    }
}

impl PartialEq for UniqueFuncInfo {
    fn eq(&self, other: &Self) -> bool { self.to_string() == other.to_string() }
    fn ne(&self, other: &Self) -> bool { self.to_string() != other.to_string() }
}

impl Eq for UniqueFuncInfo {}

impl ToString for UniqueFuncInfo {
    fn to_string(&self) -> String {
        let prefix = if self.is_method { "_MTHD_" } else { "" };
        format!("{prefix}{:?}{:?}", self.og_name, self.arg_types)
            .chars()
            .filter(|c| c.is_alphanumeric() || matches!(c, '_'))
            .collect()
    }
}

impl UniqueFuncInfo {
    pub fn from(
        og_name: &String,
        arg_types: &Vec<Type>,
        is_method: bool,
    ) -> UniqueFuncInfo {
        UniqueFuncInfo {
            og_name: og_name.clone(),
            arg_types: arg_types.clone(),
            is_method,
        }
    }

    pub fn og_name(&self) -> String { self.og_name.clone() }
    pub fn arg_types(&self) -> Vec<Type> { self.arg_types.clone() }
}
