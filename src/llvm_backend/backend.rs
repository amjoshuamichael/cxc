use std::collections::{BTreeMap, HashSet};
use inkwell::OptimizationLevel;
use inkwell::types::BasicType;

use inkwell::values::{BasicMetadataValueEnum, BasicValue};
use inkwell::{values::{PointerValue, FunctionValue}, context::Context, module::Module, execution_engine::ExecutionEngine, AddressSpace};
use slotmap::SecondaryMap;

use crate::typ::ReturnStyle;
use crate::unit::{FuncId, ProcessedFuncInfo};
use crate::{VarName, Type, unit::backends::IsBackend, mir::MIR, FuncType};

use crate::unit::backends::function::{Func, FuncDowncasted};
use super::to_llvm_type::ToLLVMType;

use super::{compile_routine, add_nescessary_attributes_to_func, add_sret_attribute_to_call_site};

pub struct LLVMBackend {
    pub globals: BTreeMap<VarName, (Type, PointerValue<'static>)>,
    pub module: Module<'static>,
    pub context: &'static Context,
    execution_engine: ExecutionEngine<'static>,
    pub compiled: SecondaryMap<FuncId, LLVMFunctionData>,
    pub to_recompile: HashSet<FuncId>,
    func_counter: u32,
}

pub struct LLVMFunctionData {
    func: Func,
    name: String,
    typ: FuncType,
    pub value: FunctionValue<'static>,
}

fn make_context() -> &'static Context {
    unsafe { std::mem::transmute(Box::leak(Box::new(Context::create()))) }
}

impl IsBackend for LLVMBackend {
    type LowerableFuncRef = Func;

    fn create() -> Self {
        let context: &'static _ = make_context();

        let random_module_name = format!("cxc");
        let module = Context::create_module(context, &random_module_name);

        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .expect("unable to create execution engine");

        Self {
            context,
            module,
            execution_engine,
            globals: BTreeMap::new(),
            compiled: SecondaryMap::new(),
            func_counter: 0,
            to_recompile: HashSet::new(),
        }
    }

    fn begin_compilation_round(&mut self) {
        self.execution_engine.remove_module(&self.module).unwrap();
        self.execution_engine = self.module
            .create_jit_execution_engine(OptimizationLevel::None)
            .expect("unable to recreate execution engine");
    }

    fn register_function(&mut self, func_id: FuncId, func_info: &ProcessedFuncInfo) {
        let name = func_info.to_string(func_id) + &*self.func_counter.to_string();
        self.func_counter += 1;

        let mut empty_function =
            self.module.add_function(
                &*name, 
                func_info.typ.llvm_func_type(self.context), 
                None
            );

        add_nescessary_attributes_to_func(&mut empty_function, &self.context, &func_info.typ);

        if let Some(LLVMFunctionData { name: ref mut old_name, ref mut value, typ: ref mut old_type, .. }) = 
            self.compiled.get_mut (func_id) {
            *value = empty_function;
            *old_name = name;
            *old_type = func_info.typ.clone();
        } else {
            self.compiled.insert(
                func_id,
                LLVMFunctionData {
                    func: Func::new_compiled(func_info.typ.clone()),
                    name,
                    value: empty_function,
                    typ: func_info.typ.clone(),
                }
            );
        }
    }

    fn compile_functions(&mut self, mirs: SecondaryMap<FuncId, MIR>) {
        for (func_id, mir) in mirs {
            let function = self.compiled[func_id].value;

            let mut fcs = self.new_func_comp_state(mir, function);

            let basic_block = fcs.context.append_basic_block(fcs.function, "entry");
            fcs.builder.position_at_end(basic_block);

            compile_routine(&mut fcs);
        }
    }

    fn end_compilation_round(&mut self) {
        for LLVMFunctionData { func, name, typ, .. } in self.compiled.values() {
            func.change(
                self.execution_engine.get_function_address(&name).unwrap() as _,
                typ.clone(),
            );
        }

        self.to_recompile.clear();
    }

    fn has_been_compiled(&self, id: FuncId) -> bool {
        if self.to_recompile.contains(&id) { return false }

        if let Some(LLVMFunctionData { func, .. }) = self.compiled.get(id) {
            func.code().pointer().is_some()
        } else { 
            false 
        }
    }

    fn get_function(&self, id: FuncId) -> &Self::LowerableFuncRef {
        &self.compiled[id].func
    }

    fn compiled_iter<'a>(&'a self) -> 
        Box<dyn Iterator<Item = (FuncId, &Self::LowerableFuncRef)> + 'a> {
        Box::new(self
            .compiled
            .iter()
            .map(|(id, LLVMFunctionData { func, .. })| (id, func))
        )
    }

    fn add_global(&mut self, name: VarName, typ: Type, address: *mut usize) {
        let as_ptr_val = self
            .context
            .i64_type()
            .const_int(address as u64, false)
            .const_to_pointer(
                typ.to_basic_type(self.context)
                    .ptr_type(AddressSpace::default()),
            );

        self.globals.insert(name, (typ, as_ptr_val));
    }

    fn add_external_func(
        &mut self, 
        func_id: FuncId, 
        func_type: FuncType, 
        func_info: &ProcessedFuncInfo,
        function_ptr: *const usize,
    ) {
        let ret_type = func_type.ret.clone();

        let calling_func_type = func_type.llvm_func_type(self.context);
        let calling_func_ptr_type = calling_func_type.ptr_type(AddressSpace::default());

        let outer_func_type = func_type.llvm_func_type(self.context);

        let name = func_info.to_string(func_id);
        let mut function =
            self.module
                .add_function(
                    &*name,
                    outer_func_type, 
                    None
                );

        add_nescessary_attributes_to_func(&mut function, self.context, &func_type);

        let builder = self.context.create_builder();

        {
            let block = self.context.append_basic_block(function, "link");
            builder.position_at_end(block);
        }

        let llvm_func_ptr = self
            .context
            .i64_type()
            .const_int(function_ptr as u64, false)
            .const_to_pointer(calling_func_ptr_type);

        let mut arg_vals: Vec<BasicMetadataValueEnum> =
            function.get_params().iter().map(|p| (*p).into()).collect();

        if ret_type.return_style(func_type.abi) == ReturnStyle::SRet {
            let mut out =
                builder.build_indirect_call(calling_func_type, llvm_func_ptr, &arg_vals, "call");
            add_sret_attribute_to_call_site(&mut out, self.context, &ret_type);
            builder.build_return(None);
        } else if ret_type.return_style(func_type.abi) == ReturnStyle::SRet {
            let i64i64 = Type::new_tuple(vec![Type::i(64); 2]).to_basic_type(self.context);

            let call_out = builder.build_alloca(i64i64, "call_out");

            arg_vals.insert(0, call_out.as_basic_value_enum().into());

            let mut call = 
                builder.build_indirect_call(
                    calling_func_type, 
                    llvm_func_ptr, 
                    &arg_vals, 
                    "call"
                );
            add_sret_attribute_to_call_site(&mut call, self.context, &ret_type);

            builder.build_return(Some(&builder.build_load(i64i64, call_out, "load")));
        } else if ret_type.is_void() {
            builder.build_indirect_call(calling_func_type, llvm_func_ptr, &arg_vals, "call");
            builder.build_return(None);
        } else {
            let out =
                builder.build_indirect_call(calling_func_type, llvm_func_ptr, &arg_vals, "call");
            let out = out.try_as_basic_value().unwrap_left();
            builder.build_return(Some(&out));
        }

        if let Some(LLVMFunctionData { name: ref mut old_name, ref mut value, typ: ref mut old_typ, .. }) = self.compiled.get_mut(func_id) {
            *value = function;
            *old_name = name;
            *old_typ = func_type;
        } else {
            self.compiled.insert(
                func_id,
                LLVMFunctionData {
                    value: function,
                    name,
                    typ: func_type.clone(),
                    func: Func::new_external(
                        func_type.clone(),
                        function_ptr,
                    ),
                }
            );
        }
    }

    fn mark_to_recompile(&mut self, id: FuncId) { 
        self.to_recompile.insert(id);
    }
}
