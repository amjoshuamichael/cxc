use std::fmt;

use crate::{FuncType, FuncQuery, mir::MIR, VarName, Type};

use super::{callable::CallInput, FuncId, ProcessedFuncInfo};

pub mod function;

pub trait IsBackend: Sized {
    type CallableFuncRef<A, R>: CallableFunc<A, R> where A: CallInput<R>;
    type LowerableFuncRef: LowerableFunc;

    fn create() -> Self;

    fn begin_compilation_round(&mut self);
    fn register_function(&mut self, id: FuncId, func_type: &ProcessedFuncInfo);
    fn compile_function(&mut self, id: FuncId, mir: MIR);
    fn end_compilation_round(&mut self);

    fn has_been_compiled(&self, id: FuncId) -> bool;
    // TODO: make this infallible??
    fn get_function(&self, id: FuncId) -> Option<&Self::LowerableFuncRef>;
    fn compiled_iter<'a>(&'a self) -> 
        Box<dyn Iterator<Item = (FuncId, &Self::LowerableFuncRef)> + 'a>;

    fn add_global(&mut self, name: VarName, typ: Type, address: *mut usize);
    fn add_external_func(&mut self, id: FuncId, func_type: FuncType, ptr: *const usize);
}

pub trait CallableFunc<A, R> where A: CallInput<R> { }

pub trait LowerableFunc: Clone + fmt::Debug {
    type LowerTo<A, R>;
    fn lower<A, R>(&self) -> Self::LowerTo<A, R> where A: CallInput<R>;
}
