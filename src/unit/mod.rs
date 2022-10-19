use crate::hlr::prelude::*;
use crate::lex::*;
use crate::parse;
use crate::parse::*;
use crate::typ::Kind;
use crate::{Type, TypeEnum};
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
use once_cell::sync::Lazy;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::{Debug, Display};
use std::mem::size_of;
use std::mem::transmute;

mod func_info;
mod globals;
pub mod output_api;
pub mod value_api;

use crate::to_llvm::*;
use func_info::FuncInfo;
pub use globals::{Functions, UniqueFuncInfo};

pub use self::output_api::Compiled;
pub use self::output_api::CompiledFunc;
pub use self::value_api::Value;

pub type IOFunc<I, O> = unsafe extern "C" fn(_: I, ...) -> O;

thread_local! {
    static LLVM: Lazy<Context> = Lazy::new(|| Context::create());
}

pub struct LLVMContext {
    context: Context,
}

impl LLVMContext {
    pub fn new() -> Self {
        LLVMContext {
            context: Context::create(),
        }
    }
}

pub struct Unit<'u> {
    pub(crate) execution_engine: ExecutionEngine<'u>,
    pub(crate) types: TypeGroup,
    pub(crate) module: Module<'u>,
    pub(crate) functions: Functions<'u>,
    pub(crate) machine: TargetMachine,
    pub(crate) context: &'u Context,
}

impl<'u> Unit<'u> {
    pub fn new(context: &'u LLVMContext) -> Self {
        let module = Context::create_module(&context.context, "new_module");
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
            types: TypeGroup::default(),
            execution_engine,
            module,
            functions: Functions::default(),
            machine,
            context: &context.context,
        }
    }

    pub fn clear(&mut self) {
        for func in self.functions.func_info_iter() {
            if self.functions.is_extern(func).unwrap() {
                continue;
            }

            let llvm_func = self.module.get_function(&*func.to_string()).unwrap();
            self.execution_engine.free_fn_machine_code(llvm_func);
            unsafe { llvm_func.delete() }
        }

        self.execution_engine.remove_module(&self.module).unwrap();

        self.execution_engine = self
            .module
            .create_jit_execution_engine(OptimizationLevel::Aggressive)
            .unwrap();

        self.functions.clear();
    }

    pub fn push_script<'s>(&'s mut self, script: &str) -> Vec<Compiled> {
        let lexed = lex(script);
        let mut script = match parse::file(lexed) {
            Ok(file) => file,
            Err(err) => {
                dbg!(&err);
                return Vec::new();
            },
        };

        let mut types_to_compile: HashSet<TypeName> =
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
                script.0.push(Decl::Func(decl));
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

        let compiled = script
            .funcs_iter()
            .map(|decl| {
                let full_func_name =
                    self.functions.funcs_with_name(decl.name.clone())[0].to_string();

                let func_address = self
                    .execution_engine
                    .get_function_address(&*full_func_name)
                    .unwrap();

                Compiled::Func(CompiledFunc {
                    address: func_address,
                    name: decl.name.to_string(),
                })
            })
            .collect();

        if crate::DEBUG {
            println!("{}", self.module.print_to_string().to_string());
        }

        compiled
    }

    pub fn get_value(&self, of: &str) -> Value {
        // TODO: create UUID
        let temp_name = "temp";

        let expr = {
            let mut lexed = lex(of);
            let mut context = lexed.split(VarName::empty(), HashMap::new());

            parse::parse_expr(&mut context).unwrap()
        };

        let func_rep = hlr(
            Vec::new(),
            Expr::Return(box expr.clone()),
            &self.functions,
            &self.types,
            Vec::new(),
        );

        let val_type = match func_rep.tree.get(ExprID::ROOT) {
            NodeData::Return { to_return, .. } => {
                func_rep.tree.get(to_return).ret_type()
            },
            _ => unreachable!(),
        };

        let get_via_ref = val_type.size(self.context) > size_of::<usize>();

        let (func_rep, ret_type) = if !get_via_ref {
            (func_rep, val_type.clone())
        } else {
            let func_rep = hlr(
                Vec::new(),
                Expr::Return(box Expr::UnarOp(Opcode::Ref(1), box expr)),
                &self.functions,
                &self.types,
                Vec::new(),
            );

            let ret_type = val_type.clone().get_ref();

            (func_rep, ret_type)
        };

        let mut fcs = {
            let func_type = ret_type.clone().func_with_args(Vec::new());

            let function = self.module.add_function(
                temp_name,
                func_type.to_any_type(self.context).into_function_type(),
                None,
            );

            self.new_func_comp_state(func_rep.tree, function, Vec::new())
        };

        {
            let block = fcs.context.append_basic_block(fcs.function, "");
            fcs.builder.position_at_end(block);
            compile(&mut fcs, ExprID::ROOT);
        };

        let func_addr = self
            .execution_engine
            .get_function_address(temp_name)
            .unwrap();

        let value = if get_via_ref {
            let comp: fn() -> *const [u8; 16] = unsafe { transmute(func_addr) };
            Value::new(val_type, unsafe { *comp() }, &self.context)
        } else {
            let comp: fn() -> [u8; 8] = unsafe { transmute(func_addr) };
            Value::new(val_type, comp(), &self.context)
        };

        self.execution_engine.free_fn_machine_code(fcs.function);
        unsafe { fcs.function.delete() };

        value
    }

    fn add_type_and_deps(
        &mut self,
        script: &Script,
        typ_name: &TypeName,
        types_to_compile: &mut HashSet<TypeName>,
    ) {
        let decl = script.get_type(typ_name.clone()).unwrap().clone();

        let uncompiled_dependencies: HashSet<TypeName> = decl
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

    fn insert_placeholder_for_function(&mut self, decl: &FuncDecl) -> FuncInfo {
        let func_info = self.get_function_info(decl);

        let function_name = func_info.unique_name();

        let function_type = func_info
            .ret_type
            .clone()
            .func_with_args(func_info.arg_types());

        let function = self.module.add_function(
            &*function_name.to_string(),
            function_type.to_any_type(self.context).into_function_type(),
            None,
        );

        self.functions
            .insert(func_info.clone(), CallableValue::from(function));

        func_info
    }

    fn add_function(&mut self, func_info: FuncInfo, decl: &FuncDecl) {
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

        let arg_names = decl.args.iter().map(|v| v.var_name.clone()).collect();

        let mut fcs = self.new_func_comp_state(
            hlr.tree,
            function.into_function_value(),
            arg_names,
        );

        let basic_block = fcs.context.append_basic_block(fcs.function, "entry");
        fcs.builder.position_at_end(basic_block);

        compile(&mut fcs, ExprID::ROOT);
    }

    fn get_function_info(&self, decl: &FuncDecl) -> FuncInfo {
        let mut arg_types: Vec<Type> = Vec::new();
        let mut arg_names: Vec<VarName> = Vec::new();

        for arg in decl.args.iter() {
            let var_type = self
                .types
                .get_gen_spec(arg.type_spec.as_ref().unwrap(), &decl.generics)
                .unwrap()
                .clone();

            arg_types.push(var_type);
            arg_names.push(arg.var_name.clone());
        }

        let ret_type = self
            .types
            .get_gen_spec(&decl.ret_type, &decl.generics)
            .unwrap();

        FuncInfo::from(&decl.name, &arg_types, &ret_type, decl.is_method)
    }

    pub fn add_rust_func<A, R>(
        &mut self,
        name: &str,
        function: [fn(A) -> R; 1],
    ) -> &mut Self {
        let func_type = self.types.type_of(&function[0]);

        let function_ptr: *const usize = unsafe { transmute(function[0]) };

        self.add_rust_func_explicit(name, function_ptr, func_type)
    }

    pub fn add_rust_func_explicit(
        &mut self,
        name: &str,
        function_ptr: *const usize,
        func_type: Type,
    ) -> &mut Self {
        let ink_func_type = func_type.to_any_type(&self.context);
        let ink_func_ptr = ink_func_type
            .into_function_type()
            .ptr_type(AddressSpace::Global);

        let name = VarName::from(name);

        let function_address = function_ptr as u64;

        let function_pointer = self
            .context
            .i64_type()
            .const_int(function_address, false)
            .const_to_pointer(ink_func_ptr);
        let callable_value: CallableValue =
            CallableValue::try_from(function_pointer).unwrap();

        let TypeEnum::Func(func_type) = func_type.as_type_enum() else { panic!() };

        self.functions.insert(
            FuncInfo::from(&name, &func_type.args, &func_type.return_type, false),
            callable_value,
        );

        self
    }

    fn new_func_comp_state<'s>(
        &'s self,
        tree: ExprTree,
        function: FunctionValue<'s>,
        arg_names: Vec<VarName>,
    ) -> FunctionCompilationState<'s> {
        FunctionCompilationState {
            tree,
            variables: HashMap::new(),
            function,
            builder: self.context.create_builder(),
            context: self.context,
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
            let function =
                transmute::<usize, unsafe extern "C" fn(_: I, ...) -> O>(func_addr);

            function
        }
    }

    pub fn add_test_lib(&mut self) -> &mut Self {
        self.add_rust_func("print", [print::<i32>])
            .add_rust_func("print", [print::<i64>])
            .add_rust_func("print", [print::<f32>])
            .add_rust_func_explicit(
                "assert_eq",
                assert::<i32> as *const usize,
                Type::never().func_with_args(vec![Type::i(32), Type::i(32)]),
            )
            .add_rust_func_explicit(
                "assert_eq",
                assert::<i64> as *const usize,
                Type::never().func_with_args(vec![Type::i(64), Type::i(64)]),
            )
            .add_rust_func_explicit(
                "assert_eq",
                assert::<f32> as *const usize,
                Type::never().func_with_args(vec![Type::f32(), Type::f32()]),
            )
            .add_rust_func("sqrt", [f32::sqrt])
            .add_rust_func_explicit(
                "panic",
                panic as *const usize,
                Type::never().func_with_args(vec![]),
            )
            .add_rust_func("to_i64", [to_i64]);

        self
    }
}

fn panic(_: ()) { panic!() }
fn print<T: Display>(val: T) { println!("{val}") }
fn assert<T: PartialEq + Debug>(lhs: T, rhs: T) { assert_eq!(lhs, rhs) }
fn to_i64(input: i32) -> i64 { input as i64 }
