use crate::{FuncType, Value, Type};

use super::interpreter::{val_to_slice, get_val_from_slice};
use std::mem::transmute;

pub fn call_with_boxes(
    pointer: *const usize, 
    func_type: FuncType, 
    args: Vec<Box<[u8]>>,
    sret: Option<*mut u8>,
) -> Box<[u8]> {
    let arg_sizes: Vec<_> = func_type.args.iter().map(Type::size).collect();
    let ret_size = func_type.ret.size();

    match (&*arg_sizes, ret_size) {
        ([], 1) => unsafe {
            let as_function = transmute::<_, fn() -> bool>(pointer);
            val_to_slice(as_function())
        },
        ([], 4) => unsafe {
            let as_function = transmute::<_, fn() -> u32>(pointer);
            val_to_slice(as_function())
        },
        ([8, 8], 20..) => unsafe {
            let as_function = transmute::<_, fn(u64, u64) -> (u64, u64, u64)>(pointer);
            let sret_transmuted = transmute::<_, &mut (u64, u64, u64)>(sret.unwrap());
            let arg_0 = get_val_from_slice(&*args[0]);
            let arg_1 = get_val_from_slice(&*args[1]);
            *sret_transmuted = as_function(arg_0, arg_1);
            Vec::new().into_boxed_slice()
        },
        ([8], 20..) => unsafe {
            let as_function = transmute::<_, fn(u64) -> (u64, u64, u64)>(pointer);
            let sret_transmuted = transmute::<_, &mut (u64, u64, u64)>(sret.unwrap());
            let arg_0 = get_val_from_slice(&*args[0]);
            *sret_transmuted = as_function(arg_0);
            Vec::new().into_boxed_slice()
        },
        ([8], 0) => unsafe {
            let as_function = transmute::<_, fn(u64) -> ()>(pointer);
            let arg_0 = get_val_from_slice(&*args[0]);
            as_function(arg_0);
            Vec::new().into_boxed_slice()
        },
        _ => todo!("{arg_sizes:?}, {ret_size}")
    }
}
