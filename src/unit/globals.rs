use inkwell::values::CallableValue;
use std::collections::HashMap;

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

    pub fn get_value(&'a self, info: UniqueFuncInfo) -> Option<CallableValue<'a>> {
        self.get_func(info).map(|t| t.1.clone())
    }

    pub fn get_type(&self, info: UniqueFuncInfo) -> Option<Type> {
        match &*(info.og_name()) {
            "alloc" => return Some(info.arg_types[1].clone()),
            "free" => return Some(Type::never()),
            "memmove" => return Some(Type::never()),
            "size_of" => return Some(Type::i(64)),
            _ => {},
        };

        self.get_func(info).map(|t| t.0.clone())
    }

    fn get_func(&self, info: UniqueFuncInfo) -> Option<&ValAndType> {
        self.compiled.get(&info)
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
