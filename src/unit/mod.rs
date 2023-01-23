use crate::hlr::hlr_data_output::FuncOutput;
use crate::hlr::prelude::*;
use crate::lex::*;
use crate::libraries::Library;
use crate::parse;
use crate::parse::*;
use crate::Type;
use crate::TypeEnum;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::values::*;
use inkwell::OptimizationLevel;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Debug;
use std::mem::transmute;
use std::rc::Rc;

mod add_external;
mod func_api;
mod functions;
mod reflect;
mod value_api;

use crate::to_llvm::*;
pub use functions::UniqueFuncInfo;

pub use self::func_api::Func;
use self::functions::DeriverFunc;
use self::functions::DeriverInfo;
pub use self::functions::FuncDeclInfo;
use self::functions::TypeLevelFunc;
pub use self::value_api::XcValue;
pub use add_external::ExternalFuncAdd;
pub use reflect::XcReflect;

pub type XcFunc<I, O> = unsafe extern "C" fn(_: I, ...) -> O;

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
    pub comp_data: Rc<CompData<'u>>,
    pub(crate) execution_engine: Rc<RefCell<ExecutionEngine<'u>>>,
    pub(crate) module: Module<'u>,
    pub(crate) context: &'u Context,
}

#[derive(Clone, Default)]
pub struct CompData<'u> {
    pub(crate) types: HashMap<TypeName, Type>,
    pub(crate) aliases: HashMap<TypeName, TypeSpec>,
    pub(crate) globals: HashMap<VarName, (Type, GlobalValue<'u>)>,
    compiled: HashMap<UniqueFuncInfo, FunctionValue<'u>>,
    func_types: HashMap<UniqueFuncInfo, Type>,
    pub(crate) func_code: HashMap<FuncDeclInfo, FuncCode>,
    decl_names: HashMap<VarName, Vec<FuncDeclInfo>>,
    derivers: HashMap<DeriverInfo, DeriverFunc>,
    pub(super) type_level_funcs: HashMap<TypeName, TypeLevelFunc>,
    intrinsics: HashSet<FuncDeclInfo>,
    reflect_arg_types: HashMap<UniqueFuncInfo, Vec<bool>>,
}

impl<'u> Debug for CompData<'u> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "compilation data holding {} types", self.types.len())
    }
}

impl<'u> Unit<'u> {
    pub fn new(context: &'u LLVMContext) -> Self {
        let module = Context::create_module(&context.context, "new_module");
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::Aggressive)
            .unwrap();

        Self {
            comp_data: Rc::new(CompData::new()),
            execution_engine: Rc::new(RefCell::new(execution_engine)),
            module,
            context: &context.context,
        }
    }

    pub fn reset_execution_engine(&self) {
        self.execution_engine
            .borrow()
            .remove_module(&self.module)
            .unwrap();
        self.execution_engine.replace(
            self.module
                .create_jit_execution_engine(OptimizationLevel::Aggressive)
                .unwrap(),
        );
    }

    pub fn clear(&mut self) {
        for func in self.comp_data.unique_func_info_iter() {
            let llvm_func = self.module.get_function(&*func.to_string()).unwrap();
            self.execution_engine
                .borrow()
                .free_fn_machine_code(llvm_func);
            unsafe { llvm_func.delete() }
        }

        self.reset_execution_engine();
        self.comp_data = Rc::new(CompData::new());
    }

    pub fn push_script<'s>(&'s mut self, script: &str) -> Vec<Func<'u>> {
        let lexed = lex(script);
        let parsed = match parse::file(lexed) {
            Ok(file) => file,
            Err(err) => {
                dbg!(&err);
                panic!();
            },
        };

        let funcs_to_process = {
            let comp_data = Rc::get_mut(&mut self.comp_data).unwrap();

            for decl in parsed.types_iter().cloned() {
                comp_data.add_type_alias(decl.name, decl.typ);
            }

            let mut declarations = Vec::new();

            for code in parsed.funcs_iter() {
                let is_generic = code.has_generics();

                let decl = comp_data.insert_code(code.clone());

                if !is_generic {
                    declarations.push(decl);
                }
            }

            let funcs_to_compile: Vec<UniqueFuncInfo> = { declarations }
                .drain(..)
                .map(|decl| comp_data.to_unique_func_info(decl, Vec::new()))
                .collect();

            funcs_to_compile
        };

        let top_level_functions = funcs_to_process.clone();

        self.compile_func_set(funcs_to_process);

        top_level_functions
            .iter()
            .map(|info| self.get_func_ref_by_info(info))
            .collect()
    }

    pub fn compile_func_set(&mut self, mut set: Vec<UniqueFuncInfo>) {
        set = set
            .drain(..)
            .filter(|info| !self.comp_data.has_been_compiled(info))
            .collect();

        if set.len() == 0 {
            return;
        }

        self.reset_execution_engine();

        let mut all_funcs_to_compile = Vec::new();
        let mut all_func_infos = Vec::new();

        while !set.is_empty() {
            all_func_infos.extend(set.clone().drain(..));

            {
                let comp_data = Rc::get_mut(&mut self.comp_data).unwrap();

                for func in &set {
                    comp_data.create_func_placeholder(
                        func,
                        &mut self.context,
                        &mut self.module,
                    );
                }
            }

            let func_reps: Vec<FuncOutput> = set
                .drain(..)
                .map(|info| {
                    let code = self.comp_data.get_code(info.clone()).unwrap();
                    hlr(info, self.comp_data.clone(), code)
                })
                .collect();

            set = func_reps
                .iter()
                .map(|f| f.get_func_dependencies())
                .flatten()
                .filter(|f| !self.comp_data.has_been_compiled(f))
                .collect();

            all_funcs_to_compile.extend({ func_reps }.drain(..));
        }

        for func_output in &mut all_funcs_to_compile {
            self.compile(func_output);
        }

        if crate::DEBUG && !crate::BLOCK_LLVM {
            println!("compiled these: ");
            println!("{}", self.module.print_to_string().to_string());
        }

        // let default_render_node_builder = UniqueFuncInfo {
        //    name: VarName::from("default"),
        //    relation: TypeRelation::Static(
        //        self.comp_data
        //            .get_by_name(&TypeName::Other("RenderNodeBuilder".into()))
        //            .unwrap(),
        //    ),
        //    own_generics: Vec::new(),
        //};
        // if let Some(func) =
        // self.comp_data.get_func_value(&default_render_node_builder) {
        //    func.print_to_stderr()
        //}
    }

    fn compile(&mut self, output: &mut FuncOutput) {
        let function = self.comp_data.get_func_value(output.info_ref()).unwrap();

        let mut fcs =
            self.new_func_comp_state(output.take_tree(), function, output.take_arg_names());

        let basic_block = fcs.context.append_basic_block(fcs.function, "entry");
        fcs.builder.position_at_end(basic_block);

        compile_routine(&mut fcs);
    }

    pub fn add_opaque_type<T>(&mut self) -> Type {
        let name = {
            let name = std::any::type_name::<T>();
            let last_colon_index = name.rfind(":").map(|p| p + 1).unwrap_or(0);
            &name[last_colon_index..]
        };

        self.add_named_opaque_type::<T>(name)
    }

    pub fn add_named_opaque_type<T>(&mut self, name: &str) -> Type {
        let opaque_type = Type::opaque_with_size(std::mem::size_of::<T>() as u32, name);
        let comp_data = Rc::get_mut(&mut self.comp_data).unwrap();

        comp_data
            .types
            .insert(TypeName::from(name), opaque_type.clone());

        opaque_type
    }

    fn new_func_comp_state<'s>(
        &'s self,
        tree: ExprTree,
        function: FunctionValue<'s>,
        arg_names: Vec<VarName>,
    ) -> FunctionCompilationState<'s> {
        FunctionCompilationState {
            ret_type: tree.return_type(),
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

    pub unsafe fn get_fn_by_name<I, O>(
        &self,
        name: &str,
    ) -> unsafe extern "C" fn(_: I, ...) -> O {
        let func_info = UniqueFuncInfo {
            name: name.into(),
            ..Default::default()
        };

        self.get_fn_by_info::<I, O>(&func_info)
    }

    pub unsafe fn get_fn_by_info<I, O>(
        &self,
        info: &UniqueFuncInfo,
    ) -> unsafe extern "C" fn(_: I, ...) -> O {
        assert!(
            self.
                module
                .get_function(&*info.to_string())
                .unwrap()
                .get_param_iter()
                .all(|param_type| !param_type.is_array_value()),
            "Cannot run function that has array value as parameter. Pass in an array pointer instead."
        );

        let func_addr = self
            .execution_engine
            .borrow()
            .get_function_address(&*info.to_string())
            .unwrap();

        transmute::<usize, unsafe extern "C" fn(_: I, ...) -> O>(func_addr)
    }

    pub fn get_func_ref_by_info(&self, info: &UniqueFuncInfo) -> Func<'u> {
        Func {
            execution_engine: self.execution_engine.clone(),
            name: info.to_string(),
        }
    }

    pub fn add_lib(&mut self, lib: impl Library) -> &mut Self {
        lib.add_to_unit(self);
        self
    }

    pub fn add_method_deriver(&mut self, func_name: VarName, func: DeriverFunc) {
        let comp_data = Rc::get_mut(&mut self.comp_data).unwrap();
        comp_data.derivers.insert(
            DeriverInfo {
                func_name,
                is_static: false,
            },
            func,
        );
    }

    pub fn add_static_deriver(&mut self, func_name: VarName, func: DeriverFunc) {
        let comp_data = Rc::get_mut(&mut self.comp_data).unwrap();
        comp_data.derivers.insert(
            DeriverInfo {
                func_name,
                is_static: true,
            },
            func,
        );
    }

    pub fn add_type_level_func(&mut self, func_name: TypeName, func: TypeLevelFunc) {
        let comp_data = Rc::get_mut(&mut self.comp_data).unwrap();
        comp_data.type_level_funcs.insert(func_name, func);
    }
}
