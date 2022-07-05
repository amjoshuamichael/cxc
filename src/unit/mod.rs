//use crate::lex::prelude::*;
//use crate::parse::prelude::*;
//use inkwell::builder::Builder;
//use inkwell::context::Context;
//use inkwell::execution_engine::ExecutionEngine;
//use inkwell::execution_engine::JitFunction;
//use inkwell::module::Module;
//use inkwell::values::*;
//use inkwell::{AddressSpace, OptimizationLevel};
//use inkwell::{FloatPredicate, IntPredicate};
//use inkwell::Pro
//
//pub struct Unit<'ctx> {
//    pub execution_engine: ExecutionEngine<'ctx>,
//    pub program_info: RefCell<ProgramInfo<'ctx>>,
//    pub context: &'ctx Context,
//    pub module: Module<'ctx>,
//    pub builder: Builder<'ctx>,
//    pub current_function: Option<FunctionValue<'ctx>>,
//}
//
//impl Unit {
//    fn push_script(&mut self, script: &str) {
//        let lexed = lex(script);
//        let parsed = parse(lexed);
//    }
//}
