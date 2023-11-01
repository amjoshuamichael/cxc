use std::fmt;

use slotmap::SecondaryMap;

use crate::{FuncType, mir::MIR, VarName, Type};

use super::{callable::CallInput, FuncId, ProcessedFuncInfo};

pub mod function;

pub trait IsBackend: Sized {
    type LowerableFuncRef;

    fn create() -> Self;

    fn begin_compilation_round(&mut self);
    fn register_function(&mut self, id: FuncId, func_type: &ProcessedFuncInfo);
    fn mark_to_recompile(&mut self, id: FuncId);
    fn compile_functions(&mut self, mirs: SecondaryMap<FuncId, MIR>);
    fn end_compilation_round(&mut self);

    fn has_been_compiled(&self, id: FuncId) -> bool;
    fn get_function(&self, id: FuncId) -> &Self::LowerableFuncRef;
    fn compiled_iter<'a>(&'a self) -> 
        Box<dyn Iterator<Item = (FuncId, &Self::LowerableFuncRef)> + 'a>;

    fn add_global(&mut self, name: VarName, typ: Type, address: *mut usize);
    fn add_external_func(
        &mut self, 
        id: FuncId, 
        func_type: FuncType, 
        func_info: &ProcessedFuncInfo, 
        ptr: *const usize
    );
}

pub trait CallableFunc<A, R> where A: CallInput<R> { }
