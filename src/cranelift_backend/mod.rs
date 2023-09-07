use std::{collections::{BTreeMap, HashSet}, sync::Arc};

use crate::{unit::{backends::IsBackend, callable::CallInput, FuncId, ProcessedFuncInfo}, FuncDowncasted, Func, FuncType, mir::MIR, VarName, Type};

use cranelift::prelude::{*, isa::{TargetIsa, TargetFrontendConfig}};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Module, FuncId as ClFuncId, Linkage, default_libcall_names};
use slotmap::SecondaryMap;

use self::{to_cl_type::func_type_to_signature, external_function::ExternalFuncData, alloc_and_free::AllocAndFree};

mod to_cl_type;
mod compile_function;
mod variables_in_mline;
mod global;
mod external_function;
mod alloc_and_free;

pub struct CraneliftBackend { 
    // function data needed during compilation
    cl_function_data: SecondaryMap<FuncId, ClFunctionData>,
    
    // function pointers generated after compilation
    func_pointers: SecondaryMap<FuncId, Func>,

    marked_as_uncompiled: HashSet<FuncId>,
    func_signatures: BTreeMap<FuncType, Signature>,
    globals: BTreeMap<VarName, Global>,
    module: JITModule,
    isa: Box<Arc<dyn TargetIsa>>,
    frontend_config: TargetFrontendConfig,
    external_functions: SecondaryMap<FuncId, ExternalFuncData>,
    alloc_and_free: AllocAndFree,
    func_counter: u32,
}

pub enum Global {
    Value {
        typ: Type,
        ptr: *const usize,
    },
    Func(FuncId),
}

pub struct ClFunctionData {
    ctx: codegen::Context,
    cl_func_id: ClFuncId,
    name: String,
    is_first_pass: bool,
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
            marked_as_uncompiled: HashSet::new(),
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
    }

    fn register_function(&mut self, func_id: FuncId, func_info: &ProcessedFuncInfo) {
        let (mut ctx, is_first_pass) = 
            if let Some(ClFunctionData { mut ctx, .. }) = 
                self.cl_function_data.remove(func_id) {
                ctx.func.clear();
                (ctx, false)
            } else {
                (self.module.make_context(), true)
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
            ClFunctionData { ctx, cl_func_id, name, is_first_pass }
        );
        self.globals.insert(func_info.name.clone(), Global::Func(func_id));

        if !self.func_pointers.contains_key(func_id) {
            self.func_pointers.insert(func_id, Func::new_compiled(func_info.typ.clone()));
        }
    }

    fn mark_as_uncompiled(&mut self, id: FuncId) {
        self.marked_as_uncompiled.insert(id);
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
        self.marked_as_uncompiled.clear();

        for func_id in self.func_pointers.keys() {
            if let Some(cl_data) = self.cl_function_data.get(func_id) {
                let cl_id = cl_data.cl_func_id;
                let address = self.module.get_finalized_function(cl_id) as *const usize;
                self.func_pointers[func_id].set_pointer(address);
            }
        }
    }

    fn has_been_compiled(&self, id: FuncId) -> bool {
        if self.marked_as_uncompiled.contains(&id) {
            return false;
        }

        if let Some(func) = self.func_pointers.get(id) {
            func.code().pointer().is_some()
        } else {
            self.external_functions.contains_key(id)
        }
    }

    fn get_function(&self, id: FuncId) -> Option<&Self::LowerableFuncRef> {
        self.func_pointers.get(id)
    }

    fn compiled_iter<'a>(&'a self) -> 
        Box<dyn Iterator<Item = (FuncId, &Self::LowerableFuncRef)> + 'a> {
        Box::new(self.func_pointers.iter())
    }

    fn add_global(&mut self, name: VarName, typ: Type, ptr: *mut usize) {
        self.globals.insert(name, Global::Value { typ, ptr });
    }

    fn add_external_func(
        &mut self, 
        info: FuncId, 
        func_type: FuncType, 
        ptr: *const usize
    ) {
        external_function::add_external_func(self, info, func_type, ptr);
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
    let mut builder = JITBuilder::with_isa(isa.clone(), default_libcall_names());

    JITModule::new(builder)
}
