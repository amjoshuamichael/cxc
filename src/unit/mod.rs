use crate::hlr::prelude::*;
use crate::lex::*;
use crate::parse::file;
use crate::parse::*;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::targets::CodeModel;
use inkwell::targets::RelocMode;
use inkwell::targets::Target;
use inkwell::targets::TargetMachine;
use inkwell::types::*;
use inkwell::values::*;
use inkwell::AddressSpace;
use inkwell::OptimizationLevel;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::{Debug, Display};
use std::sync::Arc;

mod func_info;
mod globals;

use crate::to_llvm::*;
use func_info::FuncInfo;
pub use globals::{Functions, UniqueFuncInfo};

pub struct Unit<'u> {
    pub execution_engine: ExecutionEngine<'u>,
    pub types: TypeGroup,
    pub context: &'u Context,
    pub module: Module<'u>,
    pub functions: Functions<'u>,
    pub machine: TargetMachine,
}

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
            functions: Functions::default(),
            machine,
        }
    }

    pub fn push_script<'s>(&'s mut self, script: &str) {
        let lexed = lex(script);
        let mut script = file(lexed);

        let mut types_to_compile: HashSet<String> =
            script.types_iter().map(|d| d.name.clone()).collect();

        for decl in script.types_iter() {
            if !types_to_compile.contains(&decl.name) {
                continue;
            }

            self.add_type_and_deps(&script, &decl.name, &mut types_to_compile);
        }

        for decl in script.gen_funcs_iter() {
            self.functions.insert_generic(decl.clone());
        }

        let dependencies: HashSet<GenFuncDependency> = script
            .funcs_iter()
            .map(|d| d.dependencies.clone())
            .flatten()
            .collect();

        for dep in dependencies {
            let decl = self.functions.get_generic(dep).unwrap();
            let unique_info = self.get_function_info(&decl).to_unique_func_info();

            if self.functions.get_value(unique_info).is_none() {
                script.0.push(Declaration::Func(decl));
            }
        }

        let mut function_placeholders = Vec::new();

        for decl in script.funcs_iter() {
            let func_info = self.insert_placeholder_for_function(decl);
            function_placeholders.push(func_info);
        }

        for (decl_index, decl) in script.funcs_iter().enumerate() {
            let func_info = function_placeholders[decl_index].clone();
            self.add_function(func_info, decl);
        }

        if crate::DEBUG {
            println!("{}", self.module.print_to_string().to_string());
        }
    }

    pub fn add_type_and_deps(
        &mut self,
        script: &Script,
        typ_name: &String,
        types_to_compile: &mut HashSet<String>,
    ) {
        let decl = script.get_type(typ_name.clone()).unwrap().clone();

        let uncompiled_dependencies: HashSet<String> = decl
            .dependencies
            .intersection(types_to_compile)
            .map(|s| s.clone())
            .collect();

        for dep_name in uncompiled_dependencies {
            self.add_type_and_deps(script, &dep_name, types_to_compile);
        }

        types_to_compile.remove(&decl.name);

        if decl.contains_generics {
            self.types
                .add_generic_alias(decl.name.clone(), decl.typ.clone());
        } else {
            self.types
                .add(decl.name.clone(), self.types.get_spec(&decl.typ).unwrap());
        }
    }

    pub fn insert_placeholder_for_function(&mut self, decl: &FuncDecl) -> FuncInfo {
        let func_info = self.get_function_info(decl);

        let function_name = func_info.unique_name();

        let function_type = func_info
            .ret_type
            .clone()
            .func_with_args(func_info.arg_types());

        let function = self.module.add_function(
            &*function_name,
            function_type.to_any_type(self.context).into_function_type(),
            None,
        );

        self.functions
            .insert(func_info.clone(), CallableValue::from(function));

        func_info
    }

    pub fn add_function(&mut self, func_info: FuncInfo, decl: &FuncDecl) {
        let hlr = hlr(
            decl.args.clone(),
            decl.code.clone(),
            &self.functions,
            &self.types,
            decl.generics.clone(),
        );

        let function = self
            .functions
            .get_value(func_info.to_unique_func_info())
            .unwrap();

        let arg_names = decl.args.iter().map(|v| Arc::from(&*v.var_name)).collect();

        let mut fcs = self.new_func_comp_state(
            hlr.tree,
            function.into_function_value(),
            arg_names,
        );

        let basic_block = fcs.context.append_basic_block(fcs.function, "entry");
        fcs.builder.position_at_end(basic_block);

        compile(&mut fcs, ExprID::ROOT);

        fcs.delete();
    }

    pub fn get_function_info(&self, decl: &FuncDecl) -> FuncInfo {
        let mut arg_types: Vec<Type> = Vec::new();
        let mut arg_names: Vec<Arc<str>> = Vec::new();

        for arg in decl.args.iter() {
            let var_type = self
                .types
                .get_gen_spec(arg.type_spec.as_ref().unwrap(), &decl.generics)
                .unwrap()
                .clone();
            arg_types.push(var_type);

            let var_name: Arc<str> = Arc::from(&*arg.var_name);
            arg_names.push(var_name);
        }

        let ret_type = self
            .types
            .get_gen_spec(&decl.ret_type, &decl.generics)
            .unwrap();

        FuncInfo::from(&decl.name, &arg_types, &ret_type, decl.is_method)
    }

    pub fn add_external_function(
        &mut self,
        name: &str,
        function: *const usize,
        arg_types: Vec<Type>,
        ret_type: Type,
    ) {
        let name = String::from(name);

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

        self.functions.insert(
            FuncInfo::from(&name, &arg_types, &ret_type, false),
            callable_value,
        );
    }

    fn new_func_comp_state<'s>(
        &'s self,
        tree: ExprTree,
        function: FunctionValue<'s>,
        arg_names: Vec<Arc<str>>,
    ) -> FunctionCompilationState<'s> {
        FunctionCompilationState {
            tree,
            variables: HashMap::new(),
            function,
            builder: self.context.create_builder(),
            context: &self.context,
            globals: &self.functions,
            llvm_ir_uuid: RefCell::new(0),
            arg_names,
        }
    }

    pub fn get_fn<I, O>(&self, name: &str) -> unsafe extern "C" fn(_: I, ...) -> O {
        let func_name = &self.functions.funcs_with_name(name.into())[0].to_string();

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

    pub fn add_test_lib(&mut self) {
        self.add_external_function(
            "print",
            print::<i32> as *const usize,
            vec![Type::i(32)],
            Type::never(),
        );

        self.add_external_function(
            "print",
            print::<i64> as *const usize,
            vec![Type::i(64)],
            Type::never(),
        );

        self.add_external_function(
            "print",
            print::<f32> as *const usize,
            vec![Type::f(32)],
            Type::never(),
        );

        self.add_external_function(
            "assert_eq",
            assert::<i32> as *const usize,
            vec![Type::i(32), Type::i(32)],
            Type::never(),
        );

        self.add_external_function(
            "assert_eq",
            assert::<i64> as *const usize,
            vec![Type::i(64), Type::i(64)],
            Type::never(),
        );

        self.add_external_function(
            "assert_eq",
            assert::<f32> as *const usize,
            vec![Type::f(32), Type::f(32)],
            Type::never(),
        );

        self.add_external_function(
            "sqrt",
            f32::sqrt as *const usize,
            vec![Type::f(32)],
            Type::f(32),
        );

        self.add_external_function(
            "panic",
            panic as *const usize,
            vec![],
            Type::never(),
        );

        self.add_external_function(
            "to_i64",
            to_i64 as *const usize,
            vec![Type::i(32)],
            Type::i(64),
        );
    }
}

fn panic() { panic!() }
fn print<T: Display>(num: T) { println!("{num}") }
fn assert<T: PartialEq + Debug>(lhs: T, rhs: T) { assert_eq!(lhs, rhs) }
fn to_i64(input: i32) -> i64 { input as i64 }
