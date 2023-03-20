use self::functions::DeriverFunc;
use self::functions::DeriverInfo;
pub use self::functions::FuncDeclInfo;
use self::functions::TypeLevelFunc;
pub use self::functions::{Func, FuncDowncasted};
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
use crate::Kind;
use crate::Type;
use crate::TypeEnum;
pub use add_external::ExternalFuncAdd;
pub use functions::{Gen, UniqueFuncInfo};
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::types::BasicType;
use inkwell::values::*;
use inkwell::AddressSpace;
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
    pub comp_data: CompData,
    pub(crate) execution_engine: Rc<RefCell<ExecutionEngine<'static>>>,
    pub(crate) module: Module<'static>,
    pub(crate) context: &'static Context,
}

#[derive(Clone, Default)]
pub struct CompData {
    aliases: BTreeMap<TypeName, TypeSpec>,
    pub(crate) type_level_funcs: BTreeMap<TypeName, TypeLevelFunc>,
    pub(crate) globals: BTreeMap<VarName, (Type, PointerValue<'static>)>,
    compiled: BTreeMap<UniqueFuncInfo, Func>,
    pub(crate) func_code: BTreeMap<FuncDeclInfo, FuncCode>,
    decl_names: BTreeMap<VarName, Vec<FuncDeclInfo>>,
    derivers: BTreeMap<DeriverInfo, DeriverFunc>,
    intrinsics: BTreeSet<FuncDeclInfo>,
    generations: Vec<Gen>,
    dependencies: BTreeMap<UniqueFuncInfo, BTreeSet<UniqueFuncInfo>>,
}

impl Debug for CompData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CompData {{ ... }}")
    }
}

impl Default for Unit {
    fn default() -> Self { Self::new() }
}

pub(crate) fn make_context() -> &'static Context {
    unsafe { std::mem::transmute(Box::leak(box Context::create())) }
}

impl Unit {
    pub fn new() -> Self {
        let context: &'static _ = make_context();

        let random_module_name = format!("cxc_{:x}", rand::random::<u64>());
        let module = Context::create_module(context, &random_module_name);

        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .expect("unable to create execution engine");

        Self {
            comp_data: CompData::new(),
            execution_engine: Rc::new(RefCell::new(execution_engine)),
            module,
            context,
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

    pub fn push_script(&mut self, script: &str) -> CResultMany<Vec<UniqueFuncInfo>> {
        let lexed = lex(script);

        let parsed = parse::parse(lexed).map_err(|errs| {
            if crate::XC_DEBUG {
                for err in &errs {
                    println!("{err}");
                }
            }

            { errs }.drain(..).map(CErr::Parse).collect::<Vec<_>>()
        })?;

        let has_comp_script = parsed.comp_script.is_some();

        let funcs_to_process = {
            self.comp_data.new_generation();

            for decl in parsed.types.iter().cloned() {
                self.comp_data.add_type_alias(decl.name, decl.typ);
            }

            let mut declarations = Vec::new();

            for code in parsed.funcs.iter() {
                let is_generic = code.has_generics();

                let decl = self.comp_data.insert_code(code.clone());

                if !is_generic {
                    declarations.push(decl);
                }
            }

            if let Some(comp_script) = parsed.comp_script {
                declarations.push(self.comp_data.insert_code(comp_script));
            }

            let funcs_to_compile: Vec<UniqueFuncInfo> = { declarations }
                .drain(..)
                .map(|decl| {
                    let relation = decl
                        .relation
                        .clone()
                        .map_inner_type(|spec| self.comp_data.get_spec(&spec, &()).unwrap());

                    let func_info = UniqueFuncInfo {
                        name: decl.name,
                        relation,
                        ..Default::default()
                    };

                    self.comp_data.update_func_info(func_info.clone(), &self.module);

                    func_info
                })
                .collect();

            funcs_to_compile
        };

        self.compile_func_set(funcs_to_process.clone())
            .map_err(|x| vec![x])?;

        if has_comp_script {
            self.run_comp_script();
        }

        Ok(funcs_to_process)
    }

    pub fn compile_func_set(&mut self, mut set: Vec<UniqueFuncInfo>) -> CResult<()> {
        set.retain(|info| !self.comp_data.has_been_compiled(info));

        if set.is_empty() {
            return Ok(());
        }

        self.reset_execution_engine();

        let mut all_funcs_to_compile = Vec::new();
        let mut all_func_infos = Vec::new();

        while !set.is_empty() {
            all_func_infos.extend(set.clone().into_iter());

            let func_reps: Vec<FuncOutput> = { set }
                .drain(..)
                .map(|info| {
                    let code = self.comp_data.get_code(info.clone()).unwrap();
                    let hlr = hlr(info.clone(), &self.comp_data, code);

                    self.comp_data.create_func_placeholder(&info, self.context, &self.module)?;

                    hlr
                })
                .collect::<CResult<Vec<FuncOutput>>>()?;

            set = func_reps
                .iter()
                .flat_map(|f| f.get_func_dependencies().into_iter())
                .filter(|f| !self.comp_data.has_been_compiled(f))
                .collect();

            {
                for func in func_reps.iter() {
                    let depended_on_by = self.comp_data
                        .dependencies
                        .get(func.info_ref())
                        .cloned()
                        .unwrap_or_default();

                    let depended_on_by = depended_on_by
                        .into_iter()
                        .filter(|f| self.comp_data.update_func_info(f.clone(), &self.module));

                    set.extend(depended_on_by);
                }
            }

            all_funcs_to_compile.extend(func_reps.into_iter());
        }

        {
            for func in &all_funcs_to_compile {
                for depends_on in func.get_func_dependencies() {
                    let calling_set = self.comp_data.dependencies.entry(depends_on).or_default();
                    calling_set.insert(func.info_ref().clone());
                }
            }
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
    
    fn run_comp_script(&self) {
        let func = self.execution_engine.borrow().get_function_address(&*UniqueFuncInfo {
            name: VarName::temp(),
            ..Default::default()
        }.to_string()).unwrap();

        unsafe {
            let func: unsafe extern "C" fn() = std::mem::transmute(func);
            func()
        }
    }

    fn get_func_value(&self, func_info: &UniqueFuncInfo) -> Option<FunctionValue<'static>> {
        self.module.get_function(&func_info.to_string())
    }

    fn new_func_comp_state(
        &self,
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
            comp_data: &self.comp_data,
            llvm_ir_uuid: RefCell::new(0),
            arg_names,
        }
    }

    pub fn get_fn(&self, with: impl Into<UniqueFuncInfo>) -> Option<&Func> {
        let info = with.into();

        #[cfg(feature = "ffi-assertions")]
        {
            assert!(
                self.
                    module
                    .get_function(&info.to_string())?
                    .get_param_iter()
                    .all(|param_type| !param_type.is_array_value()),
                "Cannot run function that has array value as parameter. Pass in an array pointer instead."
            );
        }

        self.comp_data.compiled.get(&info)
    }

    pub fn add_lib(&mut self, lib: impl Library) -> &mut Self {
        lib.add_to_unit(self);
        self
    }

    pub fn add_method_deriver(&mut self, func_name: VarName, func: DeriverFunc) {
        self.comp_data.derivers.insert(
            DeriverInfo {
                func_name,
                is_static: false,
            },
            func,
        );
    }

    pub fn add_static_deriver(&mut self, func_name: VarName, func: DeriverFunc) {
        self.comp_data.derivers.insert(
            DeriverInfo {
                func_name,
                is_static: true,
            },
            func,
        );
    }

    pub fn add_type_level_func(&mut self, func_name: TypeName, func: TypeLevelFunc) {
        self.comp_data.type_level_funcs.insert(func_name, func);
    }

    pub fn add_global<T: XcReflect>(&mut self, name: VarName, val: *mut T) {
        let as_int = val as usize;

        let typ = self.get_reflect_type::<T>().unwrap();

        let as_ptr_val = self
            .context
            .i64_type()
            .const_int(as_int as u64, false)
            .const_to_pointer(
                typ.to_basic_type(self.context)
                    .ptr_type(AddressSpace::default()),
            );

        self.comp_data.globals.insert(name, (typ.get_ref(), as_ptr_val));
    }
}
