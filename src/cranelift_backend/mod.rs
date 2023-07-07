use std::{collections::BTreeMap, sync::Arc};

use crate::{unit::{backends::IsBackend, callable::CallInput, Generations}, FuncDowncasted, Func, UniqueFuncInfo, FuncType, mir::MIR, VarName, Type};

use cranelift::prelude::{*, isa::{TargetIsa, TargetFrontendConfig}};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Module, FuncId, Linkage, default_libcall_names};
use indexmap::IndexMap;

use self::{to_cl_type::{func_type_to_signature}, external_function::ExternalFuncData};

mod to_cl_type;
mod compile_function;
mod variables_in_mline;
mod global;
mod external_function;

pub struct CraneliftBackend { 
    cl_function_data: IndexMap<UniqueFuncInfo, (codegen::Context, FuncId)>,
    func_pointers: BTreeMap<UniqueFuncInfo, Func>,
    func_signatures: BTreeMap<FuncType, Signature>,
    globals: BTreeMap<VarName, (Type, *const usize)>,
    module: JITModule,
    isa: Box<Arc<dyn TargetIsa>>,
    frontend_config: TargetFrontendConfig,
    generations: Generations,
    external_functions: BTreeMap<UniqueFuncInfo, ExternalFuncData>,
}

impl IsBackend for CraneliftBackend {
    type CallableFuncRef<A, R> = FuncDowncasted<A, R> where A: CallInput<R>;
    type LowerableFuncRef = Func;

    fn create() -> Self {
        let isa = make_proper_isa();

        let mut new = Self {
            cl_function_data: IndexMap::new(),
            func_pointers: BTreeMap::new(),
            func_signatures: BTreeMap::new(),
            globals: BTreeMap::new(),
            module: make_proper_module(&isa),
            frontend_config: isa.frontend_config(),
            isa: Box::new(isa),
            generations: Generations::default(),
            external_functions: BTreeMap::new(),
        };

        unsafe fn alloc_x_bytes(x: i32) -> *const u8 {
            use std::alloc::*;
            // TODO: use unchecked
            alloc(Layout::from_size_align(x as usize, 4).unwrap())
        }

        new.add_external_func(
            UniqueFuncInfo { name: "$alloc".into(), ..Default::default() },
            FuncType { args: vec![Type::i(64)], ret: Type::u(8).get_ref() },
            alloc_x_bytes as *const usize,
        );

        unsafe fn free(ptr: *mut u8) {
            use std::alloc::*;
            // this is undefined behavior!! we're using a layout that is only the size of 
            // the type of the pointer. if the user allocates more than one of a type, 
            // then this layout is wrong, which is undefined behavior under the rust 
            // allocation API. because the cxc allocation api does not require that the 
            // user specify the size of their deallocation, the only way to fix this in 
            // this backend is to stash the size of allocation for every allocation.
            let possibly_wrong_layout = Layout::from_size_align(4 as usize, 4).unwrap();
            dealloc(ptr, possibly_wrong_layout)
        }

        new.add_external_func(
            UniqueFuncInfo { name: "$free".into(), ..Default::default() },
            FuncType { args: vec![Type::u(8).get_ref()], ret: Type::void() },
            free as *const usize,
        );

        new
    }

    fn begin_compilation_round(&mut self) {
        if self.cl_function_data.len() == 0 { return; }

        self.module = make_proper_module(&*self.isa);

        for (info, (ctx, ref mut func_id)) in self.cl_function_data.iter_mut() {
            let new_id = self.module.declare_function(
                &*info.to_string(&self.generations), 
                Linkage::Local, 
                &ctx.func.signature,
            ).unwrap();
            self.module.define_function(new_id, ctx).unwrap();
            *func_id = new_id;
        }
    }

    fn register_function(&mut self, info: UniqueFuncInfo, func_type: FuncType) {
        self.generations.update(info.clone());

        let mut ctx = self.module.make_context();

        func_type_to_signature(&func_type, &mut ctx.func.signature, false);

        let id = self.module.declare_function(
            &*info.to_string(&self.generations),
            Linkage::Local,
            &ctx.func.signature,
        ).unwrap();

        self.cl_function_data.insert(info.clone(), (ctx, id));
        self.func_pointers
            .entry(info.clone())
            .or_insert_with(|| Func::new_compiled(info, func_type));
    }

    fn compile_function(&mut self, mir: MIR) {
        #[cfg(feature = "backend-debug")]
        println!("compiling {}", mir.info.to_string(&self.generations));

        let info = mir.info.clone();
        let (mut context, func_id) = self.cl_function_data.remove(&mir.info).unwrap();

        let mut func_builder_context = FunctionBuilderContext::new();

        let (fcs, lines) = compile_function::make_fcs(
            self, 
            &mut func_builder_context,
            &mut context, 
            mir,
        );

        compile_function::compile(fcs, lines);

        #[cfg(feature = "backend-debug")]
        println!("func {:?}:\n{}", info, context.func);

        self.module.define_function(func_id, &mut context).unwrap();
        context.compile(&**self.isa).unwrap();

        self.cl_function_data.insert(info, (context, func_id));
    }

    fn end_compilation_round(&mut self) {
        self.module.finalize_definitions().unwrap();

        for func_info in self.func_pointers.keys() {
            let func_id = self.cl_function_data[func_info].1;
            let address = self.module.get_finalized_function(func_id) as *const usize;
            self.func_pointers[&func_info].set_pointer(address);
        }
    }

    fn has_been_compiled(&self, info: &UniqueFuncInfo) -> bool {
        if let Some(func) = self.func_pointers.get(info) {
            func.code().pointer().is_some()
        } else {
            self.external_functions.contains_key(info)
        }
    }

    fn get_function(&self, info: impl Into<UniqueFuncInfo>) -> Option<&Self::LowerableFuncRef> {
        self.func_pointers.get(&info.into())
    }

    fn compiled_iter<'a>(&'a self) -> 
        Box<dyn Iterator<Item = (&UniqueFuncInfo, &Self::LowerableFuncRef)> + 'a> {
        Box::new(self.func_pointers.iter())
    }

    fn add_global(&mut self, name: VarName, typ: Type, address: *mut usize) {
        self.globals.insert(name, (typ, address));
    }

    fn add_external_func(
        &mut self, 
        info: UniqueFuncInfo, 
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
