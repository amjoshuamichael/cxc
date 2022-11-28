use crate::{lex::indent_parens, parse::TypeRelation, Unit};
use std::mem::transmute;

use crate::Type;

use super::UniqueFuncInfo;

#[derive(Default, Debug)]
pub struct Value {
    typ: Type,
    data: Vec<u8>,
}

impl Value {
    pub fn new_from_arr<const N: usize>(typ: Type, data: [u8; N]) -> Self {
        let size = typ.size() as usize;
        let data = data[0..size].to_vec();

        Self { typ, data }
    }

    pub fn new_from_vec(typ: Type, data: Vec<u8>) -> Self { Self { typ, data } }

    pub fn new_opaque<T: bytemuck::NoUninit>(data: T) -> Self {
        let typ = Type::opaque_type::<T>();
        Self {
            typ,
            data: Vec::from(bytemuck::bytes_of(&data)),
        }
    }

    pub fn to_string(&self, unit: &mut Unit) -> String {
        let info = UniqueFuncInfo {
            name: "to_string".into(),
            relation: TypeRelation::MethodOf(self.typ.clone()),
            ..Default::default()
        };

        unit.compile_func_set(vec![info.clone()]);

        let mut output = String::new();

        unsafe {
            let func = unit.get_fn_by_info::<(&mut String, *const usize), ()>(&info);
            func((&mut output, self.const_ptr()));
        };

        indent_parens(output)
    }

    pub unsafe fn get_data_as<T>(&self) -> &T { transmute(&self.data) }

    pub fn const_ptr(&self) -> *const usize {
        &*self.data as *const [u8] as *const usize
    }

    pub fn get_size(&self) -> usize { self.data.len() }
    pub fn get_type(&self) -> &Type { &self.typ }
    pub fn get_slice(&self) -> &[u8] { &*self.data }
}
