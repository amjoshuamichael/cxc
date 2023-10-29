use cxc_derive::xc_opaque;

use crate::{
    errors::CResultMany,
    lex::{indent_parens, lex, VarName},
    parse::{self, FuncCode, TypeRelation, context::FuncParseData},
    typ::{ReturnStyle, IntSize, ABI},
    Unit, XcReflect, IntType, TypeEnum, StructType,
};
use std::{collections::{HashMap, HashSet}, mem::transmute, sync::Mutex};

use crate::Type;

use super::{FuncQuery, backends::IsBackend};

use crate as cxc;

#[derive(Default, Debug)]
pub struct Value {
    pub(crate) typ: Type,
    pub(crate) data: Box<[u8]>,
}

impl XcReflect for Value {
    fn spec_code() -> String {
        "Value = {
            typ: Type,
            data: { +ptr: &u8, +len: u64 },
        }".into()
    }
}

enum ValueConsumeErr {
    SizeMismatch,
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
        let data = data[0..size].to_vec().into_boxed_slice();

        Self { typ, data }
    }

    pub fn new_from_boxed_slice(typ: Type, data: Box<[u8]>) -> Self { 
        Self { typ, data } 
    }

    pub fn new_from_slice(typ: Type, data: &[u8]) -> Self { 
        Self { typ, data: data.into() } 
    }

    pub fn void() -> Self {
        Self {
            typ: Type::void(),
            data: vec![0; 0].into_boxed_slice(),
        }
    }

    /// # Safety
    ///
    /// Ensure the pointer is pointing to a type equivalent to typ.
    pub unsafe fn new_from_ptr(typ: Type, ptr: *const u8) -> Self {
        let slice = std::slice::from_raw_parts(ptr, typ.size()).to_vec().into_boxed_slice();

        Self {
            typ,
            data: slice,
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

    /// Only converts primitives to strings.
    pub fn to_string_no_unit(&self) -> Option<String> {
        macro_rules! int_to_string {
            ($int_ty:ty, $data:expr) => {{
                let arr = <[u8; std::mem::size_of::<$int_ty>()]>::try_from(&*self.data).unwrap();
                <$int_ty>::from_ne_bytes(arr).to_string()
            }}
        }

        use TypeEnum::*;
        use IntSize::*;

        let string = match self.typ.remove_wrappers().as_type_enum() {
            Int(IntType { signed: false, size: _64 }) => int_to_string!(u64, &*self.data),
            Int(IntType { signed: false, size: _32 }) => int_to_string!(u32, &*self.data),
            Int(IntType { signed: false, size: _16 }) => int_to_string!(u16, &*self.data),
            Int(IntType { signed: true, size: _64 }) => int_to_string!(i64, &*self.data),
            Int(IntType { signed: true, size: _32 }) => int_to_string!(i32, &*self.data),
            Int(IntType { signed: true, size: _16 }) => int_to_string!(i16, &*self.data),
            Ref(_) => {
                let arr = <[u8; 8]>::try_from(&*self.data).unwrap();
                let val = <usize>::from_ne_bytes(arr);
                format!("0x{val:x}")
            },
            Struct(struct_type) => {
                let mut output = String::from("{ ");

                for (f, field) in struct_type.fields.iter().enumerate() {
                    output += &*field.name; 
                    output += ": ";
                    let field_offset = struct_type.field_offset_in_bytes(f);
                    let member_value = Value {
                        typ: field.typ.clone(),
                        data: self.data[field_offset..(field_offset + field.typ.size())].into(),
                    };

                    output += &*member_value.to_string_no_unit()?;

                    if f != struct_type.fields.len() - 1 {
                        output += ", ";
                    }
                }

                output += " }";

                output
            }
            Bool => {
                if self.data[0] == 0 {
                    "false".into()
                } else {
                    "true".into()
                }
            }
            _ => return None,
        };

        Some(string)
    }

    #[cfg(not(feature = "backend-interpreter"))]
    pub fn to_string(&self, unit: &mut Unit) -> Option<String> {
        if !self.typ.is_ref() && let Some(serialized_primitive) = self.to_string_no_unit() {
            return Some(serialized_primitive)
        } else {
            let query = FuncQuery {
                name: "to_string".into(),
                relation: TypeRelation::MethodOf(self.typ.clone()),
                ..Default::default()
            };

            unit.compile_func_set(std::iter::once(query.clone()).collect()).unwrap();

            let mut output = String::new();

            let func = unit.get_fn(query)?;
            let func = func.downcast::<(&mut String, *const usize), ()>();
            func(&mut output, self.const_ptr());

            Some(indent_parens(output))
        }
    }

    /// # Safety
    ///
    /// Ensure the generic type 'T' is the same as the type of the value.
    pub unsafe fn get_data_as<T>(&self) -> *const T { self.data.as_ptr() as *const T }

    /// # Safety
    ///
    /// Ensure the generic type 'T' is the same as the type of the value.
    pub unsafe fn get_data(self) -> Box<[u8]> { self.data }


    /// # Safety
    ///
    /// Ensure the generic type 'T' is the same as the type of the value.
    pub unsafe fn consume<T>(mut self) -> Box<T> {
        let data_ptr = self.data.as_mut_ptr();

        // Prevent data from being dropped
        let _: (u64, u64) = std::mem::transmute(self.data);

        Box::from_raw(data_ptr as *mut T)
    }

    pub fn checked_consume<T: Copy>(self) -> Result<T, ValueConsumeErr> {
        if self.typ.size() != self.data.len() || std::mem::size_of::<T>() != self.data.len() {
            Err(ValueConsumeErr::SizeMismatch)
        } else {
            Ok(unsafe { *(self.data.as_ptr() as *const T) })
        }
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
            let mut context = lexed.split(FuncParseData::default(), HashMap::new());

            parse::parse_expr(&mut context).unwrap().wrap()
        };

        let value_function_name = VarName::from("$val");

        let code = FuncCode {
            name: value_function_name.clone(),
            ..FuncCode::from_expr(expr)
        };

        let value_function_query = FuncQuery {
            name: value_function_name,
            ..Default::default()
        };

        self.comp_data.func_code.insert(code);

        {
            let mut func_set = HashSet::new();
            func_set.insert(value_function_query.clone());
            self.compile_func_set(func_set).unwrap();
        }

        let value_func_id = self.comp_data.query_for_id(&value_function_query).unwrap();

        let func_addr = self.backend.get_function(value_func_id);
        let func_info = &self.comp_data.processed[value_func_id];

        let ret_type = &func_info.typ.ret;
        #[cfg(feature = "backend-interpreter")]
        {
            Ok(func_addr.call_no_args())
        }

        #[cfg(not(feature = "backend-interpreter"))]
        {
            let value = unsafe {
                match ret_type.return_style(ABI::C) {
                    ReturnStyle::Direct | ReturnStyle::ThroughI64 | ReturnStyle::ThroughI32 => {
                        let new_func = func_addr.downcast::<(), i64>();
                        let out: [u8; 8] = new_func().to_ne_bytes();
                        Value::new_from_arr(ret_type.clone(), out)
                    },
                    ReturnStyle::ThroughI32I32
                    | ReturnStyle::ThroughF32F32
                    | ReturnStyle::ThroughI64I32
                    | ReturnStyle::ThroughI64I64 => {
                        let new_func = func_addr.downcast::<(), (i64, i64)>();
                        let out: [u8; 16] = transmute(new_func());
                        Value::new_from_arr(ret_type.clone(), out)
                    },
                    ReturnStyle::ThroughDouble => {
                        let new_func = func_addr.downcast::<(), f64>();
                        let out: [u8; 8] = transmute(new_func());
                        Value::new_from_arr(ret_type.clone(), out)
                    },
                    ReturnStyle::SRet => {
                        let new_func = func_addr.downcast::<(), MaxBytes>();

                        let data_vec = {
                            let mut bytes_lock = MAX_BYTES.lock().unwrap();
                            *bytes_lock = new_func();
                            bytes_lock[..ret_type.size()].to_vec()
                        };

                        Value::new_from_slice(ret_type.clone(), &*data_vec)
                    },
                    ReturnStyle::Void => {
                        panic!("value returns none!")
                    },
                }
            };

            Ok(value)
        }
    }
}
