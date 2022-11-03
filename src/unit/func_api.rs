use inkwell::execution_engine::{ExecutionEngine, FunctionLookupError};
use std::cell::RefCell;
use std::mem::transmute;
use std::rc::Rc;

pub struct Func<'u> {
    pub(super) execution_engine: Rc<RefCell<ExecutionEngine<'u>>>,
    pub(super) name: String,
}

pub enum FuncRunError {
    InvalidFunction,
}

impl<'u> Func<'u> {
    pub unsafe fn run<I, O>(&self, input: I) -> Result<O, FunctionLookupError> {
        let addr = self
            .execution_engine
            .borrow()
            .get_function_address(&*self.name)?;

        let func = transmute::<usize, unsafe extern "C" fn(_: I, ...) -> O>(addr);
        Ok(func(input))
    }
}
