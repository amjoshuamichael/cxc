use crate::hlr::prelude::*;
use crate::lex::*;
use crate::parse::*;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::context::ContextRef;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::execution_engine::UnsafeFunctionPointer;
use inkwell::module::Module;
use inkwell::targets::CodeModel;
use inkwell::targets::FileType;
use inkwell::targets::InitializationConfig;
use inkwell::targets::RelocMode;
use inkwell::targets::Target;
use inkwell::targets::TargetData;
use inkwell::targets::TargetMachine;
use inkwell::targets::TargetTriple;
use inkwell::types::*;
use inkwell::values::*;
use inkwell::AddressSpace;
use inkwell::OptimizationLevel;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::Arc;
mod globals;

use crate::to_llvm::*;
pub use globals::Globals;

pub struct Unit<'u> {
    pub execution_engine: ExecutionEngine<'u>,
    pub types: TypeGroup,
    pub context: &'u Context,
    pub module: Module<'u>,
    pub globals: Globals<'u>,
    pub machine: TargetMachine,
}

type NumGeneratorFunc = unsafe extern "C" fn(&mut i32) -> usize;

impl<'u> Unit<'u> {
    pub fn new(context: &'u Context) -> Self {
        let module = Context::create_module(context, "new_module");

        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::Aggressive)
            .unwrap();

        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple).unwrap();
        let machine = target
            .create_target_machine(
                &triple,
                &*TargetMachine::get_host_cpu_name().to_string(),
                &*TargetMachine::get_host_cpu_features().to_string(),
                OptimizationLevel::Aggressive,
                RelocMode::Default,
                CodeModel::Default,
            )
            .unwrap();

        Self {
            context,
            types: TypeGroup::with_core_lib(),
            execution_engine,
            module,
            globals: Globals::default(),
            machine,
        }
    }

    pub fn push_script<'s>(&'s mut self, script: &str) {
        let lexed = lex(script);
        let parsed = crate::parse::file(
            lexed
                .map(|token| {
                    if crate::DEBUG {
                        println!("lexing: {:?}", token);
                    }

                    token
                })
                .peekable(),
        );

        for decl in parsed.0 {
            match decl {
                Declaration::Function { name, args, code } => {
                    let hlr = hlr(args, code, &self.globals, &self.types);

                    let mut arg_types: Vec<Type> = Vec::new();
                    let mut arg_names: Vec<Arc<str>> = Vec::new();

                    for (var_name, var) in hlr.data_flow.iter() {
                        if var.is_func_param {
                            arg_types.push(var.typ.clone());
                            arg_names.push(var_name.clone());
                        }
                    }

                    let func_ret_type =
                        hlr.types.get_spec(&name.type_spec.unwrap()).unwrap();

                    let mut fcs = self.new_func_comp_state(
                        &*name.var_name.clone(),
                        func_ret_type.clone(),
                        arg_types.clone(),
                        arg_names,
                        hlr.tree,
                    );

                    let basic_block =
                        fcs.context.append_basic_block(fcs.function, "entry");
                    fcs.builder.position_at_end(basic_block);

                    let output = compile(&mut fcs, ExprID::ROOT).unwrap();
                    let output: BasicValueEnum = output.try_into().unwrap();

                    fcs.builder.build_return(Some(&output));
                    fcs.delete();

                    if crate::DEBUG {
                        self.module.print_to_stderr();
                    }

                    let function =
                        self.module.get_function(&*name.var_name).unwrap();

                    let function_type = func_ret_type.func_with_args(arg_types);

                    self.globals.insert(
                        Arc::from(&*name.var_name),
                        function.into(),
                        function_type,
                    );
                },
                Declaration::Struct { name, fields } => {
                    self.types.add(BaseType::new_struct(
                        name,
                        fields.iter().map(|f| {
                            let spec = f.type_spec.as_ref().unwrap();
                            let typ = self.types.get_spec(spec).unwrap();
                            (f.var_name.clone(), typ)
                        }),
                    ));
                },
            }
        }
    }

    fn new_func_comp_state<'s>(
        &'s self,
        name: &str,
        ret_type: Type,
        arg_types: Vec<Type>,
        arg_names: Vec<Arc<str>>,
        tree: ExprTree,
    ) -> FunctionCompilationState<'s> {
        let arg_types: Vec<BasicMetadataTypeEnum> = arg_types
            .iter()
            .map(|typ| typ.to_basic_type(self.context).into())
            .collect();
        let fn_type = ret_type
            .to_basic_type(self.context)
            .fn_type(&arg_types[..], false);
        let function = self.module.add_function(name, fn_type, None);

        FunctionCompilationState {
            tree,
            variables: HashMap::new(),
            function,
            builder: self.context.create_builder(),
            context: &self.context,
            globals: &self.globals,
            llvm_ir_uuid: RefCell::new(0),
            arg_names,
        }
    }

    pub fn get_fn<I, O>(&self, name: &str) -> unsafe extern "C" fn(_: I, ...) -> O {
        unsafe {
            let func_addr =
                self.execution_engine.get_function_address(name).unwrap();
            let function = std::mem::transmute::<
                usize,
                unsafe extern "C" fn(_: I, ...) -> O,
            >(func_addr);

            function
        }
    }

    pub fn run_fn<I, O: Copy>(&self, name: &str, mut args: I) -> O {
        type Func = unsafe extern "C" fn(usize) -> usize;

        unsafe {
            let params = std::mem::transmute::<&mut I, &mut usize>(&mut args);
            let func_addr =
                self.execution_engine.get_function_address(name).unwrap();
            let function = std::mem::transmute::<usize, Func>(func_addr);

            let mut output = function(*params);

            *std::mem::transmute::<&mut usize, &mut O>(&mut output)
        }
    }
}
