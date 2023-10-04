use crate::{FuncType, Type};

use std::mem::transmute;

macro_rules! call_no_sret {
    ($($in_type:ty),* => $out_type:ty, $($input:ident),*; $args:expr, $pointer:expr) => { unsafe {
        let as_function = transmute::<_, fn($($in_type),*) -> $out_type>($pointer);
        #[allow(unused_variables)]
        #[allow(unused_mut)]
        let mut arg_index = 0;
        $(
            let $input = get_val_from_slice(&*$args[arg_index]);
            #[allow(unused_assignments)]
            { arg_index += 1; }
        )*
        val_to_slice(as_function($($input),*))
    }}
}

pub fn call_with_boxes(
    pointer: *const usize, 
    func_type: FuncType, 
    args: Vec<Box<[u8]>>,
    sret: Option<*mut u8>,
) -> Box<[u8]> {
    let arg_sizes: Vec<_> = func_type.args.iter().map(Type::size).collect();
    let raw_ret = func_type.ret.raw_return_type();
    let ret_size = func_type.ret.size();
    let ret_field_sizes = if func_type.ret.is_primitive() { 
            if func_type.ret.size() == 0 {
                vec![]
            } else {
                vec![func_type.ret.size()]
            }
        } else { 
            Vec::from_iter(func_type.ret.primitive_fields_iter().map(|typ| typ.size()))
        };

    // big switchboard with all the different types of functions
    match (&*arg_sizes, &*ret_field_sizes) {
        ([], [1]) => call_no_sret!(=> bool, ; args, pointer),
        ([], [4]) if raw_ret.is_float() => call_no_sret!(=> f32, ; args, pointer),
        ([], [4]) => call_no_sret!(=> u32, ; args, pointer),
        ([], [8]) => call_no_sret!(=> u64, ; args, pointer),
        ([], [4, 4]) => call_no_sret!(=> (u32, u32), ; args, pointer),
        ([], [8, 4]) => call_no_sret!(=> (u64, u32), ; args, pointer),
        ([], [4, 8]) => call_no_sret!(=> (u32, u64), ; args, pointer),
        ([], [8, 8]) => call_no_sret!(=> (u64, u64), ; args, pointer),
        ([], [4, 4, 4]) => unsafe {
            let as_function = transmute::<_, fn() -> (u32, u32, u32)>(pointer);
            let output: (u32, u32, u32) = as_function();
            let moved: (u64, u32) = (transmute((output.0, output.1)), output.2);
            val_to_slice(moved)
        }
        ([], [4, 4, 4, 4]) => call_no_sret!(=> (u32, u32, u32, u32), ; args, pointer),
        ([], [16]) => call_no_sret!(=> u128, ; args, pointer),
        ([4], []) => call_no_sret!(u32 => (), a; args, pointer),
        ([8], []) => call_no_sret!(u64 => (), a; args, pointer),
        ([8], [8]) => call_no_sret!(u64 => u64, a; args, pointer),
        ([8, 4], [8]) => call_no_sret!(u64, u32 => u64, a, b; args, pointer),
        ([8, 4], [4, 4]) => call_no_sret!(u64, u32 => (u32, u32), a, b; args, pointer),
        ([], _) if ret_size >= 20 => unsafe {
            let as_function = transmute::<_, fn() -> (u64, u64, u64, u64)>(pointer);
            let sret_transmuted = transmute::<_, &mut (u64, u64, u64, u64)>(sret.unwrap());
            *sret_transmuted = as_function();
            Vec::new().into_boxed_slice()
        },
        ([8, 8], []) => call_no_sret!(u64, u64 => (), a, b; args, pointer),
        ([8, 8], _) if ret_size >= 20 => unsafe {
            let as_function = transmute::<_, fn(u64, u64) -> (u64, u64, u64)>(pointer);
            let sret_transmuted = transmute::<_, &mut (u64, u64, u64)>(sret.unwrap());
            let arg_0 = get_val_from_slice(&*args[0]);
            let arg_1 = get_val_from_slice(&*args[1]);
            *sret_transmuted = as_function(arg_0, arg_1);
            Vec::new().into_boxed_slice()
        },
        ([8, 8, 1], [8]) => call_no_sret!(u64, u64, bool => u64, a, b, c; args, pointer),
        ([8], _) if ret_size >= 20 => unsafe {
            let as_function = transmute::<_, fn(u64) -> (u64, u64, u64)>(pointer);
            let sret_transmuted = transmute::<_, &mut (u64, u64, u64)>(sret.unwrap());
            let arg_0 = get_val_from_slice(&*args[0]);
            *sret_transmuted = as_function(arg_0);
            Vec::new().into_boxed_slice()
        },
        _ => todo!("{arg_sizes:?}, {ret_size}, {ret_field_sizes:?}")
    }
}

unsafe fn val_to_slice<T: Copy>(val: T) -> Box<[u8]> {
    if std::mem::size_of::<T>() == 0 {
        return Vec::new().into_boxed_slice()
    };

    let mut data = vec![0; std::mem::size_of::<T>()];
    move_val_into_slice(&mut *data, &val);
    data.into_boxed_slice()
}

unsafe fn move_val_into_slice<T: Copy>(slice: &mut [u8], val: &T) {
    let val_as_ptr = val as *const T as *const u8; 
    checked_memcpy(val_as_ptr, slice.as_mut_ptr(), slice.len());
}

unsafe fn get_val_from_slice<T: Copy>(slice: &[u8]) -> T {
    unsafe { *(slice.as_ptr() as *const T) }
}

unsafe fn checked_memcpy(from: *const u8, to: *mut u8, len: usize) {
    for n in 0..len {
        *to.add(n) = *from.add(n);
    }
}
