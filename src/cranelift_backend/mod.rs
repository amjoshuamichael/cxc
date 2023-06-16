use std::{collections::BTreeMap, sync::Arc};

use crate::{unit::{backends::IsBackend, callable::CallInput, Generations}, FuncDowncasted, Func, UniqueFuncInfo, FuncType, mir::{MIR, MLine, MExpr}, VarName, Type};

use cranelift::{prelude::{*, isa::{TargetIsa, TargetFrontendConfig}, types as cl_types}, codegen::ir::SigRef};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Module, FuncId, Linkage};

use self::to_cl_type::{ToCLType, func_type_to_signature};

mod to_cl_type;
mod compile_function;
mod variables_in_mline;
mod global;

pub struct CraneliftBackend { 
    builder_context: FunctionBuilderContext,
    cl_function_data: BTreeMap<UniqueFuncInfo, (codegen::Context, FuncId)>,
    func_pointers: BTreeMap<UniqueFuncInfo, Func>,
    func_signatures: BTreeMap<FuncType, Signature>,
    globals: BTreeMap<VarName, (Type, *const usize)>,
    data_ctx: DataContext,
    module: JITModule,
    isa: Box<Arc<dyn TargetIsa>>,
    frontend_config: TargetFrontendConfig,
    generations: Generations,
}

impl IsBackend for CraneliftBackend {
    type CallableFuncRef<A, R> = FuncDowncasted<A, R> where A: CallInput<R>;
    type LowerableFuncRef = Func;

    fn create() -> Self {
        let mut flag_builder = settings::builder();
        // TODO: is this nescessary?
        flag_builder.set("use_colocated_libcalls", "true").unwrap();

        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let builder = JITBuilder::with_isa(isa.clone(), cranelift_module::default_libcall_names());

        let module = JITModule::new(builder);

        let mut new = Self {
            builder_context: FunctionBuilderContext::new(),
            cl_function_data: BTreeMap::new(),
            func_pointers: BTreeMap::new(),
            func_signatures: BTreeMap::new(),
            globals: BTreeMap::new(),
            data_ctx: DataContext::new(),
            module,
            frontend_config: isa.frontend_config(),
            isa: Box::new(isa),
            generations: Generations::default(),
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

        unsafe fn free(ptr: *mut u8, size: i32) {
            use std::alloc::*;
            // this is undefined behavior!! we're using a layout that is only the size of 
            // the type of the pointer. if the user allocates more than one of a type, 
            // then this layout is wrong, which is undefined behavior under the rust 
            // allocation API. because the cxc allocation api does not require that the 
            // user specify the size of their deallocation, the only way to fix this in 
            // this backend is to stash the size of allocation for every allocation.
            let possibly_wrong_layout = Layout::from_size_align(size as usize, 4).unwrap();
            dealloc(ptr, possibly_wrong_layout)
        }

        new.add_external_func(
            UniqueFuncInfo { name: "$free".into(), ..Default::default() },
            FuncType { args: vec![Type::u(8).get_ref()], ret: Type::void() },
            free as *const usize,
        );

        new
    }

    fn begin_compilation_round(&self) {

    }

    fn register_function(&mut self, info: UniqueFuncInfo, func_type: FuncType) {
        let mut ctx = self.module.make_context();

        func_type_to_signature(&func_type, &mut ctx.func.signature);

        let id = self.module.declare_function(
            &*info.to_string(&self.generations),
            Linkage::Local,
            &ctx.func.signature,
        ).unwrap();

        self.cl_function_data.insert(info.clone(), (ctx, id));
        self.func_pointers.insert(info.clone(), Func::new_compiled(info, func_type));
    }

    fn compile_function(&mut self, mir: MIR) {
        let info = mir.info.clone();
        let (mut context, func_id) = self.cl_function_data.remove(&info).unwrap();

        let used_functions = mir.dependencies.iter().filter_map(|info| {
            let func_id = 
                if let Some((_, func_id)) = self.cl_function_data.get(&info) {
                    *func_id
                } else if &*info.name.to_string() == "alloc" {
                    let alloc_info = UniqueFuncInfo::from(VarName::from("$alloc"));
                    self.cl_function_data[&alloc_info].1
                } else if &*info.name.to_string() == "free" {
                    let free_info = UniqueFuncInfo::from(VarName::from("$free"));
                    self.cl_function_data[&free_info].1
                } else {
                    return None
                };

            let func_ref = self.module.declare_func_in_func(func_id, &mut context.func);
            Some((info.clone(), func_ref))
        }).collect();

        let mut function_builder_context = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut context.func, &mut function_builder_context);

        let entry_block = builder.create_block();
        // get function paramaters into the entry block
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        // tell cranelift this block will have no predecessors, because it is the entry
        builder.seal_block(entry_block);

        let used_globals = {
            let mut used_globals = BTreeMap::<VarName, Value>::new();

            for line in &mir.lines {
                let used_vars = variables_in_mline::variables_in(line);

                for var in &used_vars {
                    if used_globals.contains_key(&var) {
                        continue;
                    }

                    let val = if !mir.variables.contains_key(var) {
                        if let Some((_, ptr)) = self.globals.get(var) {
                            builder.ins().iconst(cl_types::I64, *ptr as i64)
                        } else {
                            // global is a function
                            let info = UniqueFuncInfo::from(var.clone());

                            let (_, func_id) = &self.cl_function_data[&info];

                            let func_ref = 
                                self.module.declare_func_in_func(*func_id, builder.func);
                            builder.ins().func_addr(cl_types::I64, func_ref)
                        }
                    } else {
                        continue;
                    };

                    used_globals.insert(var.clone(), val);
                }
            }

            used_globals
        };

        for line in &mir.lines {
            let call_type = match line {
                MLine::Set { r: MExpr::Call { typ, .. }, .. } => typ,
                MLine::Expr(MExpr::Call { typ, .. }) => typ,
                _ => continue,
            };

            let mut call_sig = self.module.make_signature();
            func_type_to_signature(call_type, &mut call_sig);

            self.func_signatures.insert(call_type.clone(), call_sig);
        }

        self::compile_function::compile(
            mir, 
            entry_block, 
            builder, 
            used_functions, 
            used_globals, 
            &self.func_signatures,
            &self.frontend_config,
        );

        #[cfg(feature = "backend-debug")]
        println!("{}", context.func);

        self.module.define_function(func_id, &mut context).unwrap();
        context.compile(&**self.isa).unwrap();

        self.cl_function_data.insert(info, (context, func_id));
    }

    fn end_compilation_round(&mut self) {
        self.module.finalize_definitions().unwrap();

        for func_info in self.func_pointers.keys() {
            let func_id = self.cl_function_data[&func_info].1;
            let address = self.module.get_finalized_function(func_id) as *const usize;
            self.func_pointers[&func_info].set_pointer(address);
        }
    }

    fn has_been_compiled(&self, info: &UniqueFuncInfo) -> bool {
        let Some(func) = self.func_pointers.get(info) else { return false };
        func.code().pointer().is_some()
    }

    fn get_function(&self, info: impl Into<UniqueFuncInfo>) -> Option<&Self::LowerableFuncRef> {
        self.func_pointers.get(&info.into())
    }

    fn compiled_iter<'a>(&'a self) -> 
        Box<dyn Iterator<Item = (&UniqueFuncInfo, &Self::LowerableFuncRef)> + 'a> {
        todo!()
    }

    fn add_global(&mut self, name: VarName, typ: Type, address: *mut usize) {
        self.globals.insert(name, (typ, address));
    }

    fn add_external_func(&mut self, info: UniqueFuncInfo, func_type: FuncType, ptr: *const usize) {
        let mut ctx = self.module.make_context();

        func_type_to_signature(&func_type, &mut ctx.func.signature);

        let id = self.module.declare_function(
            &*info.to_string(&self.generations),
            Linkage::Local,
            &ctx.func.signature,
        ).unwrap();

        let mut function_builder_context = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut function_builder_context);

        let entry_block = builder.create_block();
        // get function paramaters into the entry block
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        // tell cranelift this block will have no predecessors, because it is the entry
        builder.seal_block(entry_block);

        let sigref = {
            let signature = builder.func.signature.clone();
            builder.func.import_signature(signature)
        };

        let func_val = builder.ins().iconst(cl_types::I64, ptr as i64);
        let args = builder.block_params(entry_block).to_vec();
        let call = builder.ins().call_indirect(sigref, func_val, &*args);
        let ret = builder.inst_results(call).to_vec();
        builder.ins().return_(&*ret);

        self.module.define_function(id, &mut ctx).unwrap();
        ctx.compile(&**self.isa).unwrap();

        self.cl_function_data.insert(info.clone(), (ctx, id));
        self.func_pointers.insert(info.clone(), Func::new_external(info, func_type, ptr));
    }
}
