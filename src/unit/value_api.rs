use crate::typ::Kind;
use std::mem::transmute;

use inkwell::context::Context;

use crate::Type;

#[derive(Debug)]
pub struct Value {
    typ: Type,
    data: Vec<u8>,
}

impl Value {
    pub fn new<const N: usize>(typ: Type, data: [u8; N]) -> Self {
        let size = typ.size() as usize;
        let data = data[0..size].to_vec();

        Self { typ, data }
    }

    pub unsafe fn get_data_as<T>(&self) -> &T { transmute(&self.data) }

    pub fn get_size(&self) -> usize { self.data.len() }
    pub fn get_type(&self) -> &Type { &self.typ }
    pub fn get_slice(&self) -> &[u8] { &*self.data }
}
