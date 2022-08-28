use crate::hlr::prelude::*;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::CallableValue;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Default, Debug)]
pub struct Functions<'a>(HashMap<FunctionDef, FuncValAndType<'a>>);

#[derive(Debug)]
pub struct FuncValAndType<'a> {
    pub val: Option<CallableValue<'a>>,
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
        self.0.insert(
            function_def,
            FuncValAndType {
                ret_type,
                val: Some(val),
            },
        );
    }

    pub fn replace(
        &mut self,
        function_def: FunctionDef,
        ret_type: Type,
        val: CallableValue<'a>,
    ) {
        *(self.0.get_mut(&function_def).unwrap()) = FuncValAndType {
            ret_type,
            val: Some(val),
        };
    }

    pub fn insert_placeholder(
        &mut self,
        function_def: FunctionDef,
        ret_type: Type,
        context: &'a Context,
        module: &Module<'a>,
    ) {
        let function = module.add_function(
            &*function_def.name,
            context.void_type().fn_type(&[], false),
            None,
        );

        self.0.insert(
            function_def,
            FuncValAndType {
                ret_type,
                val: Some(CallableValue::from(function)),
            },
        );
    }

    pub fn name_exists(&self, name: String) -> bool {
        self.funcs_with_name(name).len() > 0
    }

    pub fn count_with_name(&self, name: String) -> usize {
        self.funcs_with_name(name).len()
    }

    pub fn funcs_with_name(&self, name: String) -> Vec<&FunctionDef> {
        self.0.keys().filter(|def| def.name == name).collect()
    }

    pub fn get_value(&self, def: FunctionDef) -> Option<CallableValue<'a>> {
        self.get_func(def).map(|t| t.val.clone())?
    }

    pub fn get_type(&self, mut def: FunctionDef) -> Option<Type> {
        self.get_func(def).map(|t| t.ret_type.clone())
    }

    pub fn get_func(&self, mut def: FunctionDef) -> Option<&FuncValAndType<'a>> {
        if let Some(func) = self.0.get(&def) {
            Some(func)
        } else {
            match def.arg_types.last()?.as_type_enum() {
                TypeEnum::Struct(st) => {
                    def.name = st.get_full_method_name(&def.name)?.clone();
                    self.0.get(&def)
                },
                _ => None,
            }
        }
    }
}
