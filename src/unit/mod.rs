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
pub use globals::{Functions, UniqueFuncData};

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

        for decl in script.types_iter() {
            self.add_type(&script, decl, &mut types_to_compile);
        }

        let mut functions = HashMap::new();
        for (decl_index, decl) in script.funcs_iter().enumerate() {
            let (fn_type, func_def) = self.insert_placeholder_for_function(decl);

            functions.insert(decl_index, (fn_type, func_def));
        }

        for (decl_index, decl) in script.funcs_iter().enumerate() {
            let required_decl_info =
                (decl.name.clone(), decl.ret_type.clone(), decl.args.clone());

            let (fn_type, func_def) = functions.get(&decl_index).unwrap();

            self.add_function(fn_type.clone(), func_def.clone(), decl);
        }
    }

    pub fn add_type(
        &mut self,
        script: &Script,
        decl: &TypeDecl,
        types_to_compile: &mut HashSet<String>,
    ) {
        if !types_to_compile.contains(&decl.name) {
            return;
        }

        let uncompiled_dependencies: HashSet<String> = decl
            .dependencies
            .intersection(&types_to_compile)
            .map(|s| s.clone())
            .collect();

        for dep_name in uncompiled_dependencies {
            let typ_decl = script.get_type(dep_name.clone()).unwrap().clone();

            self.add_type(script, &typ_decl, types_to_compile);
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

    pub fn insert_placeholder_for_function(
        &mut self,
        decl: &FuncDecl,
    ) -> (Type, UniqueFuncData) {
        let (unique_data, func_ret_type) = self.get_function_info(decl);

        let function_name = unique_data.to_string();

        let function_type = func_ret_type
            .clone()
            .func_with_args(unique_data.arg_types().clone());

        let function = self.module.add_function(
            &*function_name,
            function_type.to_any_type(self.context).into_function_type(),
            None,
        );

        self.globals.insert(
            unique_data.clone(),
            func_ret_type,
            CallableValue::from(function),
        );

        (function_type, unique_data)
    }

    pub fn add_function(
        &mut self,
        fn_type: Type,
        function_def: UniqueFuncData,
        decl: &FuncDecl,
    ) {
        let hlr =
            hlr(decl.args.clone(), decl.code.clone(), &self.globals, &self.types);

        let function = self.globals.get_value(function_def.clone()).unwrap();

        let arg_names = decl.args.iter().map(|v| Arc::from(&*v.var_name)).collect();

        let mut fcs = self.new_func_comp_state(
            hlr.tree,
            function.into_function_value(),
            arg_names,
        );

        let basic_block = fcs.context.append_basic_block(fcs.function, "entry");
        fcs.builder.position_at_end(basic_block);

        let output = compile(&mut fcs, ExprID::ROOT);
        fcs.delete();

        if crate::DEBUG {
            self.module.print_to_stderr();
        }
    }

    pub fn get_function_info(&self, decl: &FuncDecl) -> (UniqueFuncData, Type) {
        let mut arg_types: Vec<Type> = Vec::new();
        let mut arg_names: Vec<Arc<str>> = Vec::new();

        for arg in decl.args.iter() {
            let var_type = self
                .types
                .get_spec(arg.type_spec.as_ref().unwrap())
                .unwrap()
                .clone();
            arg_types.push(var_type);

            let var_name: Arc<str> = Arc::from(&*arg.var_name);
            arg_names.push(var_name);
        }

        // let name: String = llvm_function_name(&name, &arg_types);
        let func_ret_type = self.types.get_spec(&decl.ret_type).unwrap();

        (UniqueFuncData::from(&decl.name, &arg_types, decl.is_method), func_ret_type)
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

        self.globals.insert(
            UniqueFuncData::from(&name, &arg_types, false),
            ret_type,
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
            globals: &self.globals,
            llvm_ir_uuid: RefCell::new(0),
            arg_names,
        }
    }

    pub fn get_fn<I, O>(&self, name: &str) -> unsafe extern "C" fn(_: I, ...) -> O {
        let func_name = &self.globals.funcs_with_name(name.into())[0].to_string();

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
