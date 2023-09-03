use std::{collections::BTreeMap, sync::Arc};

use crate::{unit::{backends::IsBackend, callable::CallInput, Generations, FuncId, ProcessedFuncInfo}, FuncDowncasted, Func, FuncQuery, FuncType, mir::MIR, VarName, Type};

use cranelift::{prelude::{*, isa::{TargetIsa, TargetFrontendConfig}}, codegen::ir::SigRef};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Module, FuncId as ClFuncId, Linkage, default_libcall_names};
use indexmap::IndexMap;
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
    temp_function_data: SecondaryMap<FuncId, TemporaryFunctionData>,
    
    // function pointers generated after compilation
    func_pointers: SecondaryMap<FuncId, Func>,

    func_signatures: BTreeMap<FuncType, Signature>,
    globals: BTreeMap<VarName, Global>,
    module: JITModule,
    isa: Box<Arc<dyn TargetIsa>>,
    frontend_config: TargetFrontendConfig,
    generations: Generations,
    external_functions: SecondaryMap<FuncId, ExternalFuncData>,
    alloc_and_free: AllocAndFree,
}

pub enum Global {
    Value {
        typ: Type,
        ptr: *const usize,
    },
    Func(FuncId),
}

pub struct TemporaryFunctionData {
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
            temp_function_data: SecondaryMap::new(),
            func_pointers: SecondaryMap::new(),
            func_signatures: BTreeMap::new(),
            globals: BTreeMap::new(),
            alloc_and_free: AllocAndFree::new(&module),
            module,
            frontend_config: isa.frontend_config(),
            isa: Box::new(isa),
            generations: Generations::default(),
            external_functions: SecondaryMap::new(),
        };

        new
    }

    fn begin_compilation_round(&mut self) {
        #[cfg(feature = "backend-debug")]
        println!("---beginning compilation round---");

        if self.temp_function_data.len() == 0 { return; }

        self.module = make_proper_module(&*self.isa);

        for (_, TemporaryFunctionData { ctx, cl_func_id, name, }) 
            in self.temp_function_data.iter_mut() {
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
        self.generations.update(func_id);

        let mut ctx = self.module.make_context();

        func_type_to_signature(&func_info.typ, &mut ctx.func.signature, false);

        let name = func_info.to_string(&self.generations, func_id);
        let cl_func_id = self.module.declare_function(
            &*name,
            Linkage::Local,
            &ctx.func.signature,
        ).unwrap();

        self.temp_function_data.insert(
            func_id, 
            TemporaryFunctionData { ctx, cl_func_id, name, }
        );
        self.globals.insert(func_info.name.clone(), Global::Func(func_id));

        if !self.func_pointers.contains_key(func_id) {
            self.func_pointers.insert(func_id, Func::new_compiled(func_info.typ.clone()));
        }
    }

    fn compile_function(&mut self, func_id: FuncId, mir: MIR) {
        let mut temp_data = self.temp_function_data.remove(func_id).unwrap();

        #[cfg(feature = "backend-debug")]
        println!("compiling {}", &temp_data.name);

        let mut func_builder_context = FunctionBuilderContext::new();

        let (fcs, lines) = compile_function::make_fcs(
            self, 
            &mut func_builder_context,
            &mut temp_data.ctx, 
            mir,
        );

        compile_function::compile(fcs, lines);

        #[cfg(feature = "backend-debug")]
        println!("func {:?}:\n{}", &temp_data.name, &temp_data.ctx.func);

        self.module.define_function(temp_data.cl_func_id, &mut temp_data.ctx).unwrap();
        temp_data.ctx.compile(&**self.isa).unwrap();

        self.temp_function_data.insert(func_id, temp_data);
    }

    fn end_compilation_round(&mut self) {
        self.module.finalize_definitions().unwrap();

        for func_id in self.func_pointers.keys() {
            if let Some(temp_data) = self.temp_function_data.get(func_id) {
                let cl_id = temp_data.cl_func_id;
                let address = self.module.get_finalized_function(cl_id) as *const usize;
                self.func_pointers[func_id].set_pointer(address);
            }
        }
    }

    fn has_been_compiled(&self, id: FuncId) -> bool {
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
    let flag_builder = settings::builder();
    let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
        panic!("host machine is not supported: {}", msg);
    });
    isa_builder.finish(settings::Flags::new(flag_builder)).unwrap()
}

pub fn make_proper_module(isa: &Arc<dyn TargetIsa>) -> JITModule {
    let builder = JITBuilder::with_isa(isa.clone(), default_libcall_names());
    JITModule::new(builder)
}
