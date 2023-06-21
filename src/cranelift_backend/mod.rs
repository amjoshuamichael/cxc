use std::{collections::BTreeMap, sync::Arc};

use crate::{unit::{backends::IsBackend, callable::CallInput, Generations}, FuncDowncasted, Func, UniqueFuncInfo, FuncType, mir::MIR, VarName, Type, typ::ReturnStyle};

use cranelift::prelude::{*, isa::{TargetIsa, TargetFrontendConfig}, types as cl_types};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Module, FuncId, Linkage, default_libcall_names};
use indexmap::IndexMap;

use self::to_cl_type::{ToCLType, func_type_to_signature};

mod to_cl_type;
mod compile_function;
mod variables_in_mline;
mod global;

pub struct CraneliftBackend { 
    cl_function_data: IndexMap<UniqueFuncInfo, (codegen::Context, FuncId)>,
    func_pointers: BTreeMap<UniqueFuncInfo, Func>,
    func_signatures: BTreeMap<FuncType, Signature>,
    globals: BTreeMap<VarName, (Type, *const usize)>,
    module: JITModule,
    isa: Box<Arc<dyn TargetIsa>>,
    frontend_config: TargetFrontendConfig,
    generations: Generations,
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
        let Some(func) = self.func_pointers.get(info) else { return false };
        func.code().pointer().is_some()
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
        self.generations.update(info.clone());

        let mut ctx = self.module.make_context();

        func_type_to_signature(&func_type, &mut ctx.func.signature, false);

        let id = self.module.declare_function(
            &*info.to_string(&self.generations),
            Linkage::Local,
            &ctx.func.signature,
        ).unwrap();

        let mut function_builder_context = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut function_builder_context);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let sigref = {
            // because of rust ABI rules, the rust function we're calling might have
            // a different return style than the equivalent cxc function
            let mut external_signature = self.module.make_signature();
            func_type_to_signature(&func_type, &mut external_signature, true);
            builder.func.import_signature(external_signature)
        };

        let func_val = builder.ins().iconst(cl_types::I64, ptr as i64);

        if func_type.ret.return_style() == ReturnStyle::Sret {
            let args = builder.block_params(entry_block).to_vec();
            
            builder.ins().call_indirect(sigref, func_val, &*args);
            builder.ins().return_(&[]);
        } else if func_type.ret.rust_return_style() == ReturnStyle::Sret {
            let stack_slot = builder.create_sized_stack_slot(StackSlotData {
                kind: StackSlotKind::ExplicitSlot,
                size: func_type.ret.size() as u32,
            });

            let stack_addr = builder.ins().stack_addr(cl_types::I64, stack_slot, 0);

            let mut args = builder.block_params(entry_block).to_vec();
            args.insert(0, stack_addr);
            builder.ins().call_indirect(sigref, func_val, &*args);

            // copy val to stack so we can return it
            
            let first_ret = builder.ins().stack_load(cl_types::I64, stack_slot, 0);

            let second_ret_type = match func_type.ret.return_style() {
                ReturnStyle::ThroughI64I32 => cl_types::I32,
                ReturnStyle::ThroughI64I64 => cl_types::I64,
                _ => unreachable!()
            };
            let second_ret = builder.ins().stack_load(second_ret_type, stack_slot, 8);

            builder.ins().return_(&[first_ret, second_ret]);
        } else if func_type.ret.return_style() != func_type.ret.rust_return_style() {
            let stack_slot = builder.create_sized_stack_slot(StackSlotData {
                kind: StackSlotKind::ExplicitSlot,
                size: func_type.ret.size() as u32,
            });

            let args = builder.block_params(entry_block).to_vec();
            let call = builder.ins().call_indirect(sigref, func_val, &*args);
            let call_ret_vals = builder.inst_results(call).to_vec();

            let mut stack_offset = 0;
            for (t, typ) in 
                func_type.ret.rust_raw_return_type().to_cl_type().into_iter().enumerate() {
                let val = call_ret_vals[t];
                builder.ins().stack_store(val, stack_slot, stack_offset as i32);
                stack_offset += typ.bytes();
            }

            let mut ret_vals = Vec::new();
            let mut stack_offset = 0;

            for typ in func_type.ret.raw_return_type().to_cl_type() {
                let val = builder.ins().stack_load(typ, stack_slot, stack_offset as i32);
                ret_vals.push(val);
                stack_offset += typ.bytes();
            }

            builder.ins().return_(&*ret_vals);
        } else {
            let args = builder.block_params(entry_block).to_vec();
            let call = builder.ins().call_indirect(sigref, func_val, &*args);
            let ret = builder.inst_results(call).to_vec();
            builder.ins().return_(&*ret);
        }
        
        #[cfg(feature = "backend-debug")]
        println!("external {}:\n{}", &info.name, ctx.func);

        self.module.define_function(id, &mut ctx).unwrap();
        ctx.compile(&**self.isa).unwrap();

        self.cl_function_data.insert(info.clone(), (ctx, id));
        self.func_pointers.insert(info.clone(), Func::new_external(info, func_type, ptr));
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
