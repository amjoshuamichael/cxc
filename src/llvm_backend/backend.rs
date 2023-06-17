use std::{collections::BTreeMap, rc::Rc, cell::RefCell};
use inkwell::OptimizationLevel;
use inkwell::types::BasicType;

use inkwell::values::{BasicMetadataValueEnum, BasicValue};
use inkwell::{values::{PointerValue, FunctionValue}, context::Context, module::Module, execution_engine::ExecutionEngine, AddressSpace};

use crate::TypeEnum;
use crate::typ::ReturnStyle;
use crate::unit::Generations;
use crate::{VarName, Type, unit::backends::IsBackend, mir::MIR, UniqueFuncInfo, FuncType};

use crate::unit::backends::function::{Func, FuncDowncasted};
use super::to_llvm_type::ToLLVMType;

use super::{compile_routine, add_nescessary_attributes_to_func, add_sret_attribute_to_call_site};

pub struct LLVMBackend {
    pub globals: BTreeMap<VarName, (Type, PointerValue<'static>)>,
    module: Module<'static>,
    pub context: &'static Context,
    execution_engine: Rc<RefCell<ExecutionEngine<'static>>>,
    compiled: BTreeMap<UniqueFuncInfo, Func>,
    pub generations: Generations,
}

fn make_context() -> &'static Context {
    unsafe { std::mem::transmute(Box::leak(box Context::create())) }
}

impl IsBackend for LLVMBackend {
    type LowerableFuncRef = Func;

    type CallableFuncRef<A, R> = FuncDowncasted<A, R> where A: crate::unit::callable::CallInput<R>;

    fn create() -> Self {
        let context: &'static _ = make_context();

        let random_module_name = format!("cxc_{:x}", rand::random::<u64>());
        let module = Context::create_module(context, &random_module_name);

        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .expect("unable to create execution engine");

        Self {
            context,
            module,
            execution_engine: Rc::new(RefCell::new(execution_engine)),
            globals: BTreeMap::new(),
            compiled: BTreeMap::new(),
            generations: Generations::default(),
        }
    }

    fn begin_compilation_round(&mut self) {
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

    fn register_function(&mut self, info: UniqueFuncInfo, func_type: FuncType) {
        self.generations.update(info.clone());
        
        let mut empty_function =
            self.module.add_function(
                &info.to_string(&self.generations), 
                func_type.llvm_func_type(self.context, false), 
                None
            );

        add_nescessary_attributes_to_func(&mut empty_function, &self.context, &func_type);

        self.compiled
            .entry(info.clone())
            .or_insert_with(|| Func::new_compiled(info.clone(), func_type.clone()));

        self.globals.insert(
            info.name.clone(),
            (
                Type::new(TypeEnum::Func(func_type)),
                empty_function.as_global_value().as_pointer_value(),
            ),
        );
    }

    fn compile_function(&mut self, mir: MIR) {
        let function = self.get_func_value(&mir.info).unwrap();

        let mut fcs = self.new_func_comp_state(mir, function);

        let basic_block = fcs.context.append_basic_block(fcs.function, "entry");
        fcs.builder.position_at_end(basic_block);

        compile_routine(&mut fcs, &self.module);
    }

    fn end_compilation_round(&mut self) {
        for func_info in self.compiled.keys() {
            let name = func_info.to_string(&self.generations);

            self.compiled.get(func_info).unwrap().set_pointer(
                self.execution_engine
                    .borrow()
                    .get_function_address(&name)
                    .expect("unable to get function address") as *const usize,
            );
        }

        #[cfg(feature = "backend-debug")]
        println!("{}", self.module.print_to_string().to_string());
    }

    fn has_been_compiled(&self, info: &UniqueFuncInfo) -> bool {
        if let Some(func) = self.compiled.get(info) {
            func.code().pointer().is_some()
        } else { 
            false 
        }
    }

    fn get_function(&self, with: impl Into<UniqueFuncInfo>) -> Option<&Self::LowerableFuncRef> {
        let info = with.into();

        #[cfg(feature = "ffi-assertions")]
        {
            assert!(
                self.
                    module
                    .get_function(&info.to_string(&self.generations))?
                    .get_param_iter()
                    .all(|param_type| !param_type.is_array_value()),
                "Cannot run function that has array value as parameter. Pass in an array pointer instead."
            );
        }

        self.compiled.get(&info)
    }

    fn compiled_iter<'a>(&'a self) -> 
        Box<dyn Iterator<Item = (&UniqueFuncInfo, &Self::LowerableFuncRef)> + 'a> {
        Box::new(self.compiled.iter())
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
        func_info: UniqueFuncInfo, 
        func_type: FuncType, 
        function_ptr: *const usize,
    ) {
        let ret_type = func_type.ret.clone();

        let calling_func_type = func_type.llvm_func_type(self.context, true);
        let calling_func_ptr_type = calling_func_type.ptr_type(AddressSpace::default());

        let outer_func_type = func_type.llvm_func_type(self.context, false);

        self.generations.update(func_info.clone());

        let mut function =
            self.module
                .add_function(
                    &func_info.to_string(&self.generations), 
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

        if ret_type.return_style() == ReturnStyle::Sret {
            let mut out =
                builder.build_indirect_call(calling_func_type, llvm_func_ptr, &arg_vals, "call");
            add_sret_attribute_to_call_site(&mut out, self.context, &ret_type);
            builder.build_return(None);
        } else if ret_type.rust_return_style() == ReturnStyle::Sret {
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
        } else if ret_type.return_style() != ret_type.rust_return_style() {
            let xc_ret = ret_type.raw_return_type().to_basic_type(self.context);
            let rust_ret = ret_type.rust_raw_return_type().to_basic_type(self.context);

            let out_var = builder.build_alloca(rust_ret, "out_var");

            let out =
                builder.build_indirect_call(calling_func_type, llvm_func_ptr, &arg_vals, "call").try_as_basic_value().unwrap_left();
            builder.build_store(out_var, out);

            let casted = builder.build_load(xc_ret, out_var, "load");

            builder.build_return(Some(&casted));
        } else {
            let out =
                builder.build_indirect_call(calling_func_type, llvm_func_ptr, &arg_vals, "call");
            let out = out.try_as_basic_value().unwrap_left();
            builder.build_return(Some(&out));
        }

        self.compiled.insert(
            func_info.clone(),
            Func::new_external(
                func_info.clone(),
                func_type.clone(),
                function_ptr,
            ),
        );
    }
}

impl LLVMBackend {
    fn get_func_value(&self, func_info: &UniqueFuncInfo) -> Option<FunctionValue<'static>> {
        self.module.get_function(&func_info.to_string(&self.generations))
    }
}
