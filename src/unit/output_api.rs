use super::IOFunc;
use std::mem::transmute;

pub struct CompiledFunc {
    pub address: usize,
    pub name: String,
}

impl CompiledFunc {
    pub unsafe fn get_fn<I, O>(&self) -> IOFunc<I, O> {
        transmute::<usize, IOFunc<I, O>>(self.address)
    }
}

pub enum Compiled {
    Func(CompiledFunc),
    Type,
}
