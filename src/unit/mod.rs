use self::functions::DeriverFunc;
use self::functions::DeriverInfo;
use self::functions::Func;
pub use self::functions::FuncDeclInfo;
use self::functions::TypeLevelFunc;
pub use self::value_api::XcValue;
use crate::errors::CErr;
use crate::errors::CResult;
use crate::errors::CResultMany;
use crate::hlr::hlr_data::DataFlow;
use crate::hlr::hlr_data_output::FuncOutput;
use crate::hlr::prelude::*;
use crate::lex::*;
use crate::libraries::Library;
use crate::parse;
use crate::parse::*;
use crate::to_llvm::*;
use crate::Type;
use crate::TypeEnum;
pub use add_external::ExternalFuncAdd;
pub use functions::{Gen, UniqueFuncInfo};
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::values::*;
use inkwell::OptimizationLevel;
pub use reflect::XcReflect;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::fmt::Debug;
use std::rc::Rc;

mod add_external;
pub mod callable;
mod functions;
pub mod get_type_spec;
mod reflect;
mod rust_type_name_conversion;
mod value_api;

pub struct Unit {
    pub comp_data: Rc<CompData>,
    pub(crate) execution_engine: Rc<RefCell<ExecutionEngine<'static>>>,
    pub(crate) module: Module<'static>,
    pub(crate) context: &'static Context,
}

#[derive(Clone, Default)]
pub struct CompData {
    aliases: BTreeMap<TypeName, TypeSpec>,
    pub(crate) type_level_funcs: BTreeMap<TypeName, TypeLevelFunc>,
    pub(crate) globals: BTreeMap<VarName, (Type, GlobalValue<'static>)>,
    compiled: BTreeMap<UniqueFuncInfo, Func>,
    pub(crate) func_code: BTreeMap<FuncDeclInfo, FuncCode>,
    decl_names: BTreeMap<VarName, Vec<FuncDeclInfo>>,
    derivers: BTreeMap<DeriverInfo, DeriverFunc>,
    intrinsics: BTreeSet<FuncDeclInfo>,
    generations: Vec<Gen>,
}

impl Debug for CompData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CompData {{ ... }}")
    }
}

impl Unit {
    pub fn new() -> Self {
        let context: &'static _ =
            unsafe { std::mem::transmute(Box::leak(box Context::create())) };

        let random_module_name = format!("cxc_{:x}", rand::random::<u64>());
        let module = Context::create_module(&context, &*random_module_name);

        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .expect("unable to create execution engine");

        Self {
            comp_data: Rc::new(CompData::new()),
            execution_engine: Rc::new(RefCell::new(execution_engine)),
            module,
            context: &context,
        }
    }

    pub fn reset_execution_engine(&self) {
        self.execution_engine
            .borrow()
            .remove_module(&self.module)
            .expect("unable to remove execution engine");
        self.execution_engine.replace(
            self.module
                .create_jit_execution_engine(OptimizationLevel::None)
                .expect("unable to recreate execution engine"),
        );
    }

    pub fn push_script<'s>(&'s mut self, script: &str) -> CResultMany<Vec<UniqueFuncInfo>> {
        let lexed = lex(script);

        let parsed = parse::parse(lexed).map_err(|errs| {
            if crate::XC_DEBUG {
                for err in &errs {
                    println!("{err}");
                }
            }

            { errs }.drain(..).map(CErr::Parse).collect::<Vec<_>>()
        })?;

        let funcs_to_process = {
            let comp_data = Rc::get_mut(&mut self.comp_data).unwrap();

            comp_data.new_generation();

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
                .map(|decl| {
                    let relation = decl
                        .relation
                        .clone()
                        .map_inner_type(|spec| comp_data.get_spec(&spec, &()).unwrap());

                    comp_data.update_func_info(
                        UniqueFuncInfo {
                            name: decl.name.clone(),
                            relation,
                            ..Default::default()
                        },
                        &self.module,
                    )
                })
                .collect();

            funcs_to_compile
        };

        self.compile_func_set(funcs_to_process.clone())
            .map_err(|x| vec![x])?;

        Ok(funcs_to_process)
    }

    fn comp_data_mut<'a>(&'a mut self) -> &'a mut CompData {
        Rc::get_mut(&mut self.comp_data).unwrap()
    }

    pub fn compile_func_set(&mut self, mut set: Vec<UniqueFuncInfo>) -> CResult<()> {
        set = set
            .into_iter()
            .filter(|info| !self.comp_data.has_been_compiled(info))
            .collect();

        if set.len() == 0 {
            return Ok(());
        }

        self.reset_execution_engine();

        let mut all_funcs_to_compile = Vec::new();
        let mut all_func_infos = Vec::new();

        while !set.is_empty() {
            all_func_infos.extend(set.clone().drain(..));

            let func_reps: Vec<FuncOutput> = { set }
                .drain(..)
                .map(|info| {
                    let code = self.comp_data.get_code(info.clone()).unwrap();
                    let hlr = hlr(info.clone(), self.comp_data.clone(), code);

                    let comp_data = Rc::get_mut(&mut self.comp_data).unwrap();
                    comp_data.create_func_placeholder(
                        &info,
                        &mut self.context,
                        &mut self.module,
                    )?;

                    hlr
                })
                .collect::<CResult<Vec<FuncOutput>>>()?;

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

        for func_info in &all_func_infos {
            self.comp_data.compiled.get(func_info).unwrap().set_pointer(
                self.execution_engine
                    .borrow()
                    .get_function_address(&func_info.to_string())
                    .expect("unable to get function address") as *const usize,
            );
        }

        if crate::LLVM_DEBUG {
            println!("compiled these: ");
            println!("{}", self.module.print_to_string().to_string());
        }

        Ok(())
    }

    fn compile(&mut self, output: &mut FuncOutput) {
        let function = self.get_func_value(output.info_ref()).unwrap();

        let mut fcs = self.new_func_comp_state(
            output.take_tree(),
            output.take_data_flow(),
            function,
            output.take_arg_names(),
        );

        let basic_block = fcs.context.append_basic_block(fcs.function, "entry");
        fcs.builder.position_at_end(basic_block);

        compile_routine(&mut fcs, &self.module);
    }

    fn get_func_value(&self, func_info: &UniqueFuncInfo) -> Option<FunctionValue<'static>> {
        self.module.get_function(&*func_info.to_string())
    }

    fn new_func_comp_state<'f>(
        &'f self,
        tree: ExprTree,
        data_flow: DataFlow,
        function: FunctionValue<'static>,
        arg_names: Vec<VarName>,
    ) -> FunctionCompilationState {
        FunctionCompilationState {
            ret_type: tree.return_type(),
            tree,
            data_flow,
            variables: HashMap::new(),
            used_functions: HashMap::new(),
            function,
            builder: self.context.create_builder(),
            context: self.context,
            comp_data: self.comp_data.clone(),
            llvm_ir_uuid: RefCell::new(0),
            arg_names,
        }
    }

    pub fn get_fn(&self, with: impl Into<UniqueFuncInfo>) -> Option<&Func> {
        let info = with.into();

        assert!(
            self.
                module
                .get_function(&*info.to_string())?
                .get_param_iter()
                .all(|param_type| !param_type.is_array_value()),
            "Cannot run function that has array value as parameter. Pass in an array pointer instead."
        );

        self.comp_data.compiled.get(&info)
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
