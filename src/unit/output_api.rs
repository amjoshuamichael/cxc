use super::IOFunc;
use std::mem::transmute;

pub struct FuncRef {
    address: usize,
}

impl FuncRef {
    pub unsafe fn get_fn<I, O>(&self) -> IOFunc<I, O> {
        transmute::<usize, IOFunc<I, O>>(self.address)
    }
}
