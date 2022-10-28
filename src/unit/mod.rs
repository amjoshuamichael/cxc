use crate::hlr::hlr_anonymous;
use crate::hlr::hlr_data_output::FuncOutput;
use crate::hlr::prelude::*;
use crate::lex::*;
use crate::libraries::Library;
use crate::parse;
use crate::parse::*;
use crate::typ::Kind;
use crate::Type;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::targets::CodeModel;
use inkwell::targets::RelocMode;
use inkwell::targets::Target;
use inkwell::targets::TargetMachine;
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
use std::rc::Rc;

mod func_info;
mod functions;
pub mod output_api;
pub mod value_api;

use crate::to_llvm::*;
pub use functions::UniqueFuncInfo;

pub use self::functions::FuncDeclInfo;
pub use self::value_api::Value;
pub use output_api::FuncRef;

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
    pub(crate) comp_data: Rc<CompData<'u>>,
    pub(crate) module: Module<'u>,
    pub(crate) machine: TargetMachine,
    pub(crate) context: &'u Context,
}

#[derive(Clone)]
pub struct CompData<'u> {
    pub(crate) types: HashMap<TypeName, Type>,
    pub(crate) aliases: HashMap<TypeName, TypeAlias>,
    compiled: HashMap<UniqueFuncInfo, FunctionValue<'u>>,
    func_types: HashMap<UniqueFuncInfo, Type>,
    func_code: HashMap<FuncDeclInfo, FuncCode>,
    derivers: HashMap<VarName, fn(&CompData, TypeName) -> Option<FuncCode>>,
}

impl<'u> Debug for CompData<'u> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "comp_data: working on it!")
    }
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
            comp_data: Rc::new(CompData::new()),
            execution_engine,
            module,
            machine,
            context: &context.context,
        }
    }

    pub fn clear(&mut self) {
        for func in self.comp_data.unique_func_info_iter() {
            if self.comp_data.is_extern(&func.clone().into()) {
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

        self.comp_data = Rc::new(CompData::new());
    }

    pub fn push_script<'s>(&'s mut self, script: &str) -> Vec<UniqueFuncInfo> {
        let lexed = lex(script);
        let script = match parse::file(lexed) {
            Ok(file) => file,
            Err(err) => {
                dbg!(&err);
                return Vec::new();
            },
        };

        let funcs_to_compile: Vec<UniqueFuncInfo> = script
            .funcs_iter()
            .filter(|func| !func.is_generic())
            .map(|d| d.to_unique_func_info())
            .collect();

        {
            let comp_data = Rc::get_mut(&mut self.comp_data).unwrap();

            for decl in script.types_iter().cloned() {
                comp_data.add_type_alias(decl.name, decl.typ);
            }

            for decl in script.funcs_iter() {
                comp_data.insert_code(decl.clone());
            }

            for func in &funcs_to_compile {
                comp_data.create_func_placeholder(
                    func,
                    &mut self.context,
                    &mut self.module,
                );
            }
        }

        let output = funcs_to_compile.clone();

        let func_reps: Vec<FuncOutput> = { funcs_to_compile }
            .drain(..)
            .map(|info| hlr(info, self.comp_data.clone()))
            .collect();

        let dependencies_to_compile: HashSet<UniqueFuncInfo> = func_reps
            .iter()
            .map(|f| f.get_func_dependencies())
            .flatten()
            .filter(|f| !self.comp_data.has_been_compiled(f))
            .collect();

        {
            let comp_data = Rc::get_mut(&mut self.comp_data).unwrap();

            for func in &dependencies_to_compile {
                comp_data.create_func_placeholder(
                    func,
                    &mut self.context,
                    &mut self.module,
                );
            }
        }

        let dependency_reps: Vec<FuncOutput> = { dependencies_to_compile }
            .drain()
            .map(|info| hlr(info, self.comp_data.clone()))
            .collect();

        let mut all_funcs_to_compile: Vec<FuncOutput> = { dependency_reps }
            .drain(..)
            .chain({ func_reps }.drain(..))
            .collect();

        for output in &mut all_funcs_to_compile {
            self.add_function(output);
        }

        if crate::DEBUG {
            println!("{}", self.module.print_to_string().to_string());
        }

        output
    }

    pub fn get_value(&self, of: &str) -> Value {
        let temp_name = "temp";

        let expr = {
            let mut lexed = lex(of);
            let mut context = lexed.split(VarName::temp(), HashMap::new());

            parse::parse_expr(&mut context).unwrap()
        };

        let code = FuncCode::from_expr(Expr::Return(box expr.clone()));

        let info = UniqueFuncInfo {
            name: VarName::temp(),
            method_of: None,
            generics: Vec::new(),
        };

        let func_rep = hlr_anonymous(info.clone(), self.comp_data.clone(), code);

        let val_type = match func_rep.tree_ref().get(ExprID::ROOT) {
            NodeData::Return { to_return, .. } => {
                func_rep.tree_ref().get(to_return).ret_type()
            },
            _ => unreachable!(),
        };

        let get_via_ref = val_type.size(self.context) > size_of::<usize>();

        let (mut func_rep, ret_type) = if !get_via_ref {
            (func_rep, val_type.clone())
        } else {
            let new_expr = Expr::Return(box Expr::UnarOp(Opcode::Ref(1), box expr));
            let new_code = FuncCode::from_expr(new_expr.clone());

            let new_func_output =
                hlr_anonymous(info, self.comp_data.clone(), new_code);

            let new_ret_type = val_type.clone().get_ref();

            (new_func_output, new_ret_type)
        };

        let mut fcs = {
            let func_type = ret_type.clone().func_with_args(Vec::new());

            let function = self.module.add_function(
                temp_name,
                func_type.to_any_type(self.context).into_function_type(),
                None,
            );

            self.new_func_comp_state(func_rep.take_tree(), function, Vec::new())
        };

        {
            let block = fcs.context.append_basic_block(fcs.function, "");
            fcs.builder.position_at_end(block);
            compile_routine(&mut fcs);
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

    fn add_function(&mut self, output: &mut FuncOutput) {
        let function = self.comp_data.get_func_value(output.info_ref()).unwrap();

        let mut fcs = self.new_func_comp_state(
            output.take_tree(),
            function,
            output.take_arg_names(),
        );

        let basic_block = fcs.context.append_basic_block(fcs.function, "entry");
        fcs.builder.position_at_end(basic_block);

        compile_routine(&mut fcs);
    }

    pub fn add_rust_func<A, R>(
        &mut self,
        name: &str,
        function: [fn(A) -> R; 1],
    ) -> &mut Self {
        let func_type = self.comp_data.type_of(&function[0]);

        let function_ptr: *const usize = unsafe { transmute(function[0]) };

        self.add_rust_func_explicit(name, function_ptr, func_type, None, Vec::new())
    }

    pub fn add_rust_func_explicit(
        &mut self,
        name: &str,
        function_ptr: *const usize,
        func_type: Type,
        method_of: Option<TypeName>,
        generics: Vec<Type>,
    ) -> &mut Self {
        let ink_func_type =
            func_type.to_any_type(&self.context).into_function_type();
        let ink_func_ptr = ink_func_type.ptr_type(AddressSpace::Global);

        let func_info = UniqueFuncInfo {
            name: name.into(),
            method_of,
            generics,
        };

        let function =
            self.module
                .add_function(&*func_info.to_string(), ink_func_type, None);

        let builder = self.context.create_builder();

        {
            let block = self.context.append_basic_block(function, "link");
            builder.position_at_end(block);
        }

        let callable_value = {
            let function_pointer = self
                .context
                .i64_type()
                .const_int(function_ptr as u64, false)
                .const_to_pointer(ink_func_ptr);

            CallableValue::try_from(function_pointer).unwrap()
        };

        let param_count = function.count_params();
        let arg_vals: Vec<BasicMetadataValueEnum> = (0..param_count)
            .map(|p| function.get_nth_param(p).unwrap().try_into().unwrap())
            .collect();

        let out = builder.build_call(callable_value, &*arg_vals, "call");
        builder.build_return(Some(&out.try_as_basic_value().unwrap_left()));

        let function = self.module.get_function(&*func_info.to_string()).unwrap();
        let comp_data = Rc::get_mut(&mut self.comp_data).unwrap();

        comp_data.compiled.insert(func_info.clone(), function);
        comp_data.func_types.insert(func_info, func_type);

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
            comp_data: self.comp_data.clone(),
            llvm_ir_uuid: RefCell::new(0),
            arg_names,
        }
    }

    pub fn get_fn<I, O>(&self, name: &str) -> unsafe extern "C" fn(_: I, ...) -> O {
        let func_info = UniqueFuncInfo {
            name: name.into(),
            method_of: None,
            generics: Vec::new(),
        };

        unsafe {
            let func_addr = self
                .execution_engine
                .get_function_address(&*func_info.to_string())
                .unwrap();
            let function =
                transmute::<usize, unsafe extern "C" fn(_: I, ...) -> O>(func_addr);

            function
        }
    }

    pub fn add_lib(&mut self, lib: impl Library) -> &mut Self {
        lib.add_to_unit(self);
        self
    }

    pub fn add_deriver(
        &mut self,
        func_name: VarName,
        func: fn(&CompData, TypeName) -> Option<FuncCode>,
    ) {
        let comp_data = Rc::get_mut(&mut self.comp_data).unwrap();
        comp_data.add_deriver(func_name, func);
    }
}
