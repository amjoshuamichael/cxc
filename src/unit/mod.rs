use crate::hlr::prelude::*;
use crate::lex::prelude::*;
use crate::parse::prelude::*;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::types::*;
use inkwell::values::*;
use std::collections::HashMap;
use std::sync::Arc;

pub struct Unit<'u> {
    pub execution_engine: ExecutionEngine<'u>,
    pub types: TypeGroup,
    pub context: &'u Context,
    pub module: Module<'u>,
}

struct FunctionCompilationState<'f> {
    pub tree: ExprTree,
    pub variables: HashMap<Arc<str>, PointerValue<'f>>,
    pub function: FunctionValue<'f>,
    pub builder: Builder<'f>,
    pub context: &'f Context,
}

impl<'u> Unit<'u> {
    fn push_script(&mut self, script: &str) {
        let lexed = lex(script);
        let parsed = parse(lexed);

        for decl in parsed.0 {
            match decl {
                Declaration::Function { name, args, code } => {
                    let arg_types: Vec<Type> = args
                        .iter()
                        .map(|arg| self.types.get_spec(&arg.1.unwrap()).unwrap())
                        .collect();
                    let comp_state = self.new_func_comp_state(name);
                }
            }
        }
    }

    fn new_func_comp_state(
        &'u mut self,
        name: &str,
        typ: Type,
        arg_types: Vec<Type>,
        tree: ExprTree,
    ) -> FunctionCompilationState<'u> {
        let arg_types: Vec<BasicMetadataTypeEnum> =
            arg_types.iter().map(|typ| self.to_basic_type(&typ).into()).collect();
        let fn_type = self.to_basic_type(&typ).fn_type(&arg_types[..], false);
        let function = self.module.add_function(name, fn_type, None);

        FunctionCompilationState {
            tree,
            variables: HashMap::new(),
            function,
            builder: self.context.create_builder(),
            context: self.context,
        }
    }

    fn to_basic_type(&self, typ: &Type) -> BasicTypeEnum {
        match typ.gen_ret_type() {
            PrimInt => self.context.i32_type().into(),
            PrimFloat => self.context.f32_type().into(),
            _ => todo!(),
        }
    }
}
