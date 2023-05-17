use std::fmt;

use crate::{FuncType, UniqueFuncInfo, mir::MIR, VarName, Type, ExternalFuncAdd};

use super::callable::CallInput;

pub mod function;

pub trait IsBackend: Sized {
    type CallableFuncRef<A, R>: CallableFunc<A, R> where A: CallInput<R>;
    type LowerableFuncRef: LowerableFunc;

    fn create() -> Self;

    fn begin_compilation_round(&self);
    fn register_function(&mut self, info: UniqueFuncInfo, func_type: FuncType);
    fn compile_function(&mut self, mir: MIR);
    fn end_compilation_round(&self);

    fn has_been_compiled(&self, info: &UniqueFuncInfo) -> bool;
    fn get_function(&self, info: impl Into<UniqueFuncInfo>) -> Option<&Self::LowerableFuncRef>;
    fn compiled_iter<'a>(&'a self) -> 
        Box<dyn Iterator<Item = (&UniqueFuncInfo, &Self::LowerableFuncRef)> + 'a>;

    fn add_global(&mut self, name: VarName, typ: Type, address: *mut usize);
    fn add_external_func(&mut self, info: UniqueFuncInfo, ptr: *const usize, func_type: FuncType, ext_add: ExternalFuncAdd);
}

pub trait CallableFunc<A, R> where A: CallInput<R> { }

pub trait LowerableFunc: Clone + fmt::Debug {
    type LowerTo<A, R>;
    fn lower<A, R>(&self) -> Self::LowerTo<A, R> where A: CallInput<R>;
}
