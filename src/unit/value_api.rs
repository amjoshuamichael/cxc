use crate::{typ::Kind, Unit};
use std::mem::transmute;

use crate::Type;

use super::UniqueFuncInfo;

#[derive(Default, Debug)]
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

    pub fn to_string(&self, unit: &mut Unit) -> String {
        let info = UniqueFuncInfo {
            name: "to_string".into(),
            method_of: Some(self.typ.clone().get_ref()),
            generics: Vec::new(),
        };

        unit.compile_func_set(vec![info.clone()]);

        let mut output = String::new();
        unsafe {
            let func = unit.get_fn_by_info::<(&mut String, *const usize), ()>(&info);
            func((&mut output, self.const_ptr()))
        };
        output
    }

    pub unsafe fn get_data_as<T>(&self) -> &T { transmute(&self.data) }

    pub fn const_ptr(&self) -> *const usize {
        &*self.data as *const [u8] as *const usize
    }

    pub fn get_size(&self) -> usize { self.data.len() }
    pub fn get_type(&self) -> &Type { &self.typ }
    pub fn get_slice(&self) -> &[u8] { &*self.data }
}
