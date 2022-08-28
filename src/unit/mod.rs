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
use std::fmt::{Debug, Display};
use std::path::Path;
use std::sync::Arc;
mod globals;

use crate::to_llvm::*;
pub use globals::{FunctionDef, Functions};

pub struct Unit<'u> {
    pub execution_engine: ExecutionEngine<'u>,
    pub types: TypeGroup,
    pub context: &'u Context,
    pub module: Module<'u>,
    pub globals: Functions<'u>,
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
            types: TypeGroup::default(),
            execution_engine,
            module,
            globals: Functions::default(),
            machine,
        }
    }

    pub fn push_script<'s>(&'s mut self, script: &str) {
        let lexed = lex(script);
        let script = crate::parse::file(lexed);

        let mut types_to_compile: HashSet<String> = script
            .0
            .iter()
            .filter(|d| matches!(d, Declaration::Type { .. }))
            .map(|d| d.name())
            .collect();

        for decl in script.0.iter() {
            self.push_declaration(&script, decl.clone(), &mut types_to_compile)
        }
    }

    pub fn push_declaration(
        &mut self,
        script: &Script,
        decl: Declaration,
        types_to_compile: &mut HashSet<String>,
    ) {
        match decl {
            Declaration::Function {
                name,
                args,
                code,
                ret_type,
            } => {
                let hlr = hlr(args, code, &self.globals, &self.types);

                let mut arg_types: Vec<Type> = Vec::new();
                let mut arg_names: Vec<Arc<str>> = Vec::new();

                for (var_name, var) in hlr.data_flow.iter() {
                    if var.is_func_param {
                        arg_types.push(var.typ.clone());
                        arg_names.push(var_name.clone());
                    }
                }

                let function_name: String = llvm_function_name(&name, &arg_types);
                let func_ret_type = hlr.types.get_spec(&ret_type).unwrap();

                let mut fcs = self.new_func_comp_state(
                    &*function_name,
                    func_ret_type.clone(),
                    arg_types.clone(),
                    arg_names,
                    hlr.tree,
                );

                let basic_block =
                    fcs.context.append_basic_block(fcs.function, "entry");
                fcs.builder.position_at_end(basic_block);

                let output = compile(&mut fcs, ExprID::ROOT);
                fcs.delete();

                if crate::DEBUG {
                    self.module.print_to_stderr();
                }

                let function = self.module.get_function(&*function_name).unwrap();

                self.globals.insert(
                    FunctionDef { name, arg_types },
                    func_ret_type,
                    CallableValue::from(function),
                );
            },
            Declaration::Type {
                name,
                typ,
                contains_generics,
                dependencies,
            } => {
                if !types_to_compile.contains(&name) {
                    return;
                }

                let uncompiled_dependencies: HashSet<String> = dependencies
                    .intersection(&types_to_compile)
                    .map(|s| s.clone())
                    .collect();

                for typ in uncompiled_dependencies {
                    self.push_declaration(
                        script,
                        script.get_type(typ.clone()).unwrap().clone(),
                        types_to_compile,
                    );
                }

                types_to_compile.remove(&name);

                if contains_generics {
                    self.types.add_generic_alias(name, typ);
                } else {
                    self.types.add(name, self.types.get_spec(&typ).unwrap());
                }
            },
        }
    }

    pub fn add_external_function(
        &mut self,
        name: &str,
        function: *const usize,
        arg_types: Vec<Type>,
        ret_type: Type,
    ) {
        let function_address = function as u64;

        let ink_arg_types: Vec<BasicMetadataTypeEnum> = arg_types
            .iter()
            .map(|t| t.to_basic_type(self.context).into())
            .collect();

        let fn_type = match ret_type.to_any_type(self.context) {
            AnyTypeEnum::VoidType(void) => void.fn_type(&*ink_arg_types, false),
            other_any_type => {
                let basic: BasicTypeEnum = other_any_type.try_into().unwrap();
                basic.fn_type(&*ink_arg_types, false)
            },
        }
        .ptr_type(AddressSpace::Global);
        let function_pointer = self
            .context
            .i64_type()
            .const_int(function_address, false)
            .const_to_pointer(fn_type);
        let callable_value: CallableValue =
            CallableValue::try_from(function_pointer).unwrap();

        self.globals.insert(
            FunctionDef {
                name: String::from(name),
                arg_types,
            },
            ret_type,
            callable_value,
        );
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
        let arg_types = &self.globals.funcs_with_name(name.into())[0].arg_types;
        let func_name = llvm_function_name(&name.into(), arg_types);

        unsafe {
            let func_addr = self
                .execution_engine
                .get_function_address(&*func_name)
                .unwrap();
            let function = std::mem::transmute::<
                usize,
                unsafe extern "C" fn(_: I, ...) -> O,
            >(func_addr);

            function
        }
    }

    // TODO: this is only being used for testing functions.
    // Other rust functions should be added as well.
    pub fn add_std_lib(&mut self) {
        self.add_external_function(
            "stdprint",
            print::<i32> as *const usize,
            vec![Type::int_of_size(32)],
            Type::never(),
        );

        self.add_external_function(
            "stdprint",
            print::<f32> as *const usize,
            vec![Type::float_of_size(32)],
            Type::never(),
        );

        self.add_external_function(
            "assert_eq",
            assert::<i32> as *const usize,
            vec![Type::int_of_size(32), Type::int_of_size(32)],
            Type::never(),
        );

        self.add_external_function(
            "assert_eq",
            assert::<f32> as *const usize,
            vec![Type::float_of_size(32), Type::float_of_size(32)],
            Type::never(),
        );

        self.add_external_function(
            "sqrt",
            f32::sqrt as *const usize,
            vec![Type::float_of_size(32)],
            Type::float_of_size(32),
        )
    }
}

fn print<T: Display>(num: T) { println!("{num}") }
fn assert<T: PartialEq + Debug>(lhs: T, rhs: T) { assert_eq!(lhs, rhs) }

pub fn llvm_function_name(og_name: &String, arg_types: &Vec<Type>) -> String {
    // TODO: make these nescessarily unique
    format!("{:?}{:?}", og_name, arg_types)
        .chars()
        .filter(|c| c.is_alphanumeric() || matches!(c, '_'))
        .collect()
}
