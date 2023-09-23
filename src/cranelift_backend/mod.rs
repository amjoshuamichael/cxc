use std::{collections::{BTreeMap, HashSet}, sync::Arc};

use crate::{unit::{backends::IsBackend, callable::CallInput, FuncId, ProcessedFuncInfo}, FuncDowncasted, Func, FuncType, mir::MIR, VarName, Type};

use cranelift::prelude::{*, isa::{TargetIsa, TargetFrontendConfig}};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Module, FuncId as ClFuncId, Linkage, default_libcall_names};
use slotmap::SecondaryMap;

use self::{to_cl_type::func_type_to_signature, external_function::ExternalFuncData, alloc_and_free::AllocAndFree};

mod to_cl_type;
mod compile_function;
mod variables_in;
mod global;
mod external_function;
mod alloc_and_free;

pub struct CraneliftBackend { 
    // function data needed during compilation
    cl_function_data: SecondaryMap<FuncId, ClFunctionData>,
    
    // function pointers generated after compilation
    func_pointers: SecondaryMap<FuncId, Func>,

    to_recompile: HashSet<FuncId>,
    func_signatures: BTreeMap<FuncType, Signature>,
    globals: BTreeMap<VarName, *const usize>,
    module: JITModule,
    isa: Box<Arc<dyn TargetIsa>>,
    frontend_config: TargetFrontendConfig,
    external_functions: SecondaryMap<FuncId, ExternalFuncData>,
    alloc_and_free: AllocAndFree,
    func_counter: u32,
}

pub struct ClFunctionData {
    ctx: codegen::Context,
    cl_func_id: ClFuncId,
    name: String,
}

impl IsBackend for CraneliftBackend {
    type CallableFuncRef<A, R> = FuncDowncasted<A, R> where A: CallInput<R>;
    type LowerableFuncRef = Func;

    fn create() -> Self {
        let isa = make_proper_isa();
        let module = make_proper_module(&isa);

        let new = Self {
            cl_function_data: SecondaryMap::new(),
            func_pointers: SecondaryMap::new(),
            to_recompile: HashSet::new(),
            func_signatures: BTreeMap::new(),
            globals: BTreeMap::new(),
            alloc_and_free: AllocAndFree::new(&module),
            module,
            frontend_config: isa.frontend_config(),
            isa: Box::new(isa),
            external_functions: SecondaryMap::new(),
            func_counter: 0,
        };

        new
    }

    fn begin_compilation_round(&mut self) {
        #[cfg(feature = "backend-debug")]
        println!("---beginning compilation round---");

        self.module = make_proper_module(&*self.isa);

        for (_id, ClFunctionData { ctx, name, cl_func_id, .. }) in self.cl_function_data.iter_mut() {
            let new_id = self.module.declare_function(
                &*name, 
                Linkage::Local, 
                &ctx.func.signature,
            ).unwrap();
            self.module.define_function(new_id, ctx).unwrap();
            *cl_func_id = new_id;
        }
    }

    fn register_function(&mut self, func_id: FuncId, func_info: &ProcessedFuncInfo) {
        let mut ctx = 
            if let Some(ClFunctionData { mut ctx, .. }) = 
                self.cl_function_data.remove(func_id) {
                ctx.func.clear();
                ctx
            } else {
                self.module.make_context()
            };

        func_type_to_signature(&func_info.typ, &mut ctx.func.signature, false);

        let name = func_info.to_string(func_id) + &*self.func_counter.to_string();
        self.func_counter += 1;

        let cl_func_id = self.module.declare_function(
            &*name,
            Linkage::Local,
            &ctx.func.signature,
        ).unwrap();

        self.cl_function_data.insert(
            func_id, 
            ClFunctionData { ctx, cl_func_id, name }
        );

        if !self.func_pointers.contains_key(func_id) {
            self.func_pointers.insert(func_id, Func::new_compiled(func_info.typ.clone()));
        }
    }

    fn mark_to_recompile(&mut self, id: FuncId) {
        self.to_recompile.insert(id);
    }

    fn compile_function(&mut self, func_id: FuncId, mir: MIR) {
        let mut cl_data = self.cl_function_data.remove(func_id).unwrap();

        #[cfg(feature = "backend-debug")]
        println!("compiling {}", &cl_data.name);

        let mut func_builder_context = FunctionBuilderContext::new();

        let (fcs, lines) = compile_function::make_fcs(
            self, 
            &mut func_builder_context,
            &mut cl_data.ctx, 
            mir,
        );

        compile_function::compile(fcs, lines);

        #[cfg(feature = "backend-debug")]
        println!("func {:?}:\n{}", &cl_data.name, &cl_data.ctx.func);

        self.module.define_function(cl_data.cl_func_id, &mut cl_data.ctx).unwrap();

        cl_data.ctx.compile(&**self.isa).unwrap();

        self.cl_function_data.insert(func_id, cl_data);
    }

    fn end_compilation_round(&mut self) {
        self.module.finalize_definitions().unwrap();
        self.to_recompile.clear();

        for func_id in self.func_pointers.keys() {
            if let Some(cl_data) = self.cl_function_data.get(func_id) {
                let cl_id = cl_data.cl_func_id;
                let address = self.module.get_finalized_function(cl_id) as *const usize;
                self.func_pointers[func_id].set_pointer(address);
            }
        }
    }

    fn has_been_compiled(&self, id: FuncId) -> bool {
        if self.to_recompile.contains(&id) { return false; }

        if let Some(func) = self.func_pointers.get(id) {
            func.code().pointer().is_some()
        } else {
            self.external_functions.contains_key(id)
        }
    }

    fn get_function(&self, id: FuncId) -> &Self::LowerableFuncRef {
        &self.func_pointers[id]
    }

    fn compiled_iter<'a>(&'a self) -> 
        Box<dyn Iterator<Item = (FuncId, &Self::LowerableFuncRef)> + 'a> {
        Box::new(self.func_pointers.iter())
    }

    fn add_global(&mut self, name: VarName, _: Type, ptr: *mut usize) {
        self.globals.insert(name, ptr);
    }

    fn add_external_func(
        &mut self, 
        func_id: FuncId, 
        func_type: FuncType, 
        _: &ProcessedFuncInfo,
        ptr: *const usize
    ) {
        external_function::add_external_func(self, func_id, func_type, ptr);
    }
}

pub fn make_proper_isa() -> Arc<dyn TargetIsa> {
    let mut flag_builder = settings::builder();
    flag_builder.set("use_colocated_libcalls", "false").unwrap();
    flag_builder.set("is_pic", "false").unwrap();

    let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
        panic!("host machine is not supported: {}", msg);
    });

    isa_builder.finish(settings::Flags::new(flag_builder)).unwrap()
}

pub fn make_proper_module(isa: &Arc<dyn TargetIsa>) -> JITModule {
    let builder = JITBuilder::with_isa(isa.clone(), default_libcall_names());

    JITModule::new(builder)
}
