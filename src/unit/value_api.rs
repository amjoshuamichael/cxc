use crate::{
    errors::CResultMany,
    hlr::hlr,
    lex::{indent_parens, lex, VarName},
    parse::{self, FuncCode, TypeRelation},
    typ::ReturnStyle,
    Unit, XcReflect, mir::mir,
};
use std::{collections::HashMap, mem::transmute, sync::Mutex};

use crate::Type;

use super::{UniqueFuncInfo, backends::IsBackend};

use crate as cxc;

#[derive(Default, Debug, XcReflect)]
pub struct Value {
    typ: Type,
    data: Vec<u8>,
}

// This MAX_BYTES static is used as an output for functions that return a value
// larger than 16 bytes. These functions really return through a pointer that is
// passed as an argument, but on ARM machines, this pointer has its own special
// register, so we need rust to put the pointer into this register. This could
// be done through inline assmebly, but it's difficult to target arm assembly on
// rust. Therefore, we use this static as a workaround, passing a pointer to it.
const MAX_VALUE_SIZE: usize = 4096;
type MaxBytes = [u8; MAX_VALUE_SIZE];

lazy_static::lazy_static! {
    static ref MAX_BYTES: Mutex<MaxBytes> = Mutex::new([0; MAX_VALUE_SIZE]);
}

impl Value {
    pub fn new_from_arr<const N: usize>(typ: Type, data: [u8; N]) -> Self {
        let size = typ.size();
        let data = data[0..size].to_vec();

        Self { typ, data }
    }

    pub fn new_from_vec(typ: Type, data: Vec<u8>) -> Self { Self { typ, data } }

    /// # Safety
    ///
    /// Ensure the pointer is pointing to a type equivalent to typ.
    pub unsafe fn new_from_ptr(typ: Type, ptr: *const u8) -> Self {
        let slice = std::slice::from_raw_parts(ptr, typ.size());

        Self {
            typ,
            data: slice.to_vec(),
        }
    }

    #[cfg(feature = "bytemuck")]
    pub fn new_reflect<T: XcReflect + bytemuck::NoUninit>(data: T, unit: &Unit) -> Self {
        let typ = unit
            .get_reflect_type::<T>()
            .expect("type contains generics");

        Self {
            typ,
            data: Vec::from(bytemuck::bytes_of(&data)),
        }
    }

    pub fn to_string(&self, unit: &mut Unit) -> Option<String> {
        let info = UniqueFuncInfo {
            name: "to_string".into(),
            relation: TypeRelation::MethodOf(self.typ.clone()),
            ..Default::default()
        };

        unit.compile_func_set(vec![info.clone()]).unwrap();

        let mut output = String::new();

        let func = unit.get_fn(info)?;
        let func = func.downcast::<(&mut String, *const usize), ()>();
        func(&mut output, self.const_ptr());

        Some(indent_parens(output))
    }

    /// # Safety
    ///
    /// Ensure the generic type 'T' is the same as the type of the value.
    pub unsafe fn get_data_as<T>(&self) -> *const T { self.data.as_ptr() as *const T }

    /// # Safety
    ///
    /// Ensure the generic type 'T' is the same as the type of the value.
    pub unsafe fn get_data(self) -> Box<[u8]> { self.data.into_boxed_slice() }


    /// # Safety
    ///
    /// Ensure the generic type 'T' is the same as the type of the value.
    pub unsafe fn consume<T: std::fmt::Debug>(mut self) -> Box<T> {
        let data_ptr = self.data.as_mut_ptr();

        // Prevent data from being dropped
        let _: (u64, u64, u64) = std::mem::transmute(self.data);

        Box::from_raw(data_ptr as *mut T)
    }

    pub fn const_ptr(&self) -> *const usize { &*self.data as *const [u8] as *const usize }

    pub fn get_size(&self) -> usize { self.data.len() }
    pub fn get_type(&self) -> &Type { &self.typ }
    pub fn get_slice(&self) -> &[u8] { &self.data }
}

impl Unit {
    pub fn get_value(&mut self, of: &str) -> CResultMany<Value> {
        if of.is_empty() {
            return Ok(Value::default());
        }

        let expr = {
            let mut lexed = lex(of);
            let mut context = lexed.split(VarName::None, HashMap::new());

            parse::parse_expr(&mut context).unwrap().wrap_in_block()
        };

        let code = FuncCode::from_expr(expr);

        let value_function_info = UniqueFuncInfo {
            name: VarName::from("$val"),
            ..Default::default()
        };

        let hlr = hlr(value_function_info.clone(), &self.comp_data, code)?;
        let mir = mir(hlr);

        let dependencies: Vec<_> = mir.dependencies
            .iter()
            .filter(|f| !self.has_been_compiled(f))
            .cloned()
            .collect();

        self.compile_func_set(dependencies)?;

        let ret_type = mir.func_type.ret.clone();

        self.backend.begin_compilation_round();
        self.backend.register_function(value_function_info.clone(), mir.func_type.clone());
        self.backend.compile_function(mir);
        self.backend.end_compilation_round();

        let func_addr = self.backend.get_function(value_function_info).unwrap();

        let value = unsafe {
            match ret_type.return_style() {
                ReturnStyle::Direct | ReturnStyle::ThroughI64 | ReturnStyle::ThroughI32 => {
                    let new_func = func_addr.downcast::<(), i64>();
                    let out: [u8; 8] = new_func().to_ne_bytes();
                    Value::new_from_arr(ret_type.clone(), out)
                },
                ReturnStyle::ThroughI32I32
                | ReturnStyle::ThroughF32F32
                | ReturnStyle::ThroughI64I32
                | ReturnStyle::ThroughI64I64
                | ReturnStyle::MoveIntoI64I64 => {
                    let new_func = func_addr.downcast::<(), (i64, i64)>();
                    let out: [u8; 16] = transmute(new_func());
                    Value::new_from_arr(ret_type.clone(), out)
                },
                ReturnStyle::MoveIntoDouble => {
                    let new_func = func_addr.downcast::<(), f64>();
                    let out: [u8; 8] = transmute(new_func());
                    Value::new_from_arr(ret_type.clone(), out)
                },
                ReturnStyle::Sret => {
                    let new_func = func_addr.downcast::<(), MaxBytes>();

                    let data_vec = {
                        let mut bytes_lock = MAX_BYTES.lock().unwrap();
                        *bytes_lock = new_func();
                        bytes_lock[..ret_type.size()].to_vec()
                    };

                    Value::new_from_vec(ret_type.clone(), data_vec)
                },
                ReturnStyle::Void => {
                    panic!("value returns none!")
                },
            }
        };

        // TODO: free function machine code

        Ok(value)
    }
}

impl<'a> IntoIterator for &'a Value {
    type Item = &'a u8;
    type IntoIter = std::slice::Iter<'a, u8>;

    fn into_iter(self) -> Self::IntoIter { self.data.iter() }
}

impl IntoIterator for Value {
    type Item = u8;
    type IntoIter = std::vec::IntoIter<u8>;

    fn into_iter(self) -> Self::IntoIter { self.data.into_iter() }
}
