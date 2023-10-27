use std::sync::{Arc, RwLock};

use crate::{unit::{FuncId, backends::CallableFunc, callable::CallInput}, Value};

use super::IntepreterState;
use super::interpreter::run_with_args;

pub struct LoweredInterpreterFunc<A, R: Copy> {
    pub(super) state: Arc<RwLock<IntepreterState>>,
    pub(super) func_id: FuncId,
    pub(super) _marker: std::marker::PhantomData<(A, R)>,
}

impl<A, R: Copy> CallableFunc<A, R> for LoweredInterpreterFunc<A, R> 
    where A: CallInput<R>, R: Copy {}

macro_rules! impl_interpreter_func {
    ($($t:ident)*) => {
        impl<$($t: Copy,)* R: Copy> FnOnce<($($t,)*)> for LoweredInterpreterFunc<($($t,)*), R>
        where
            ($($t,)*): CallInput<R>,
        {
            type Output = R;

            extern "rust-call" fn call_once(self, args: ($($t,)*)) -> Self::Output {
                self.call(args)
            }
        }

        impl<$($t: Copy,)* R: Copy> FnMut<($($t,)*)> for LoweredInterpreterFunc<($($t,)*), R>
        where
            ($($t,)*): CallInput<R>,
        {
            extern "rust-call" fn call_mut(&mut self, args: ($($t,)*)) -> Self::Output {
                self.call(args)
            }
        }

        impl<$($t: Copy,)* R: Copy> Fn<($($t,)*)> for LoweredInterpreterFunc<($($t,)*), R>
        where
            ($($t,)*): CallInput<R>,
        {
            extern "rust-call" fn call(&self, args: ($($t,)*)) -> Self::Output {
                let state = self.state.read().unwrap();
                #[allow(unused_mut)]
                let mut boxed_args = Vec::<Box<[u8]>>::new();

                #[allow(non_snake_case)]
                let ($($t,)*) = args;

                $(
                    let var_size = std::mem::size_of::<$t>();
                    if var_size > 16 {
                        let ptr = &$t;
                        let arg_arr: [u8; 8] = (ptr as *const $t as usize).to_ne_bytes();
                        boxed_args.push(arg_arr.into());
                    } else {
                        unsafe {
                            let slice = std::slice::from_raw_parts(
                                &$t as *const $t as *const u8, 
                                var_size
                            );

                            boxed_args.push(slice.into());
                        }
                    }
                )*

                unsafe {
                    *run_with_args(self.func_id, &*state, boxed_args, false).consume::<R>()
                }
            }
        }
    }
}

impl_interpreter_func!();
impl_interpreter_func!(A);
impl_interpreter_func!(A B);
impl_interpreter_func!(A B C);
impl_interpreter_func!(A B C D);
impl_interpreter_func!(A B C D E);
impl_interpreter_func!(A B C D E F G);
impl_interpreter_func!(A B C D E F G H);
impl_interpreter_func!(A B C D E F G H I);
impl_interpreter_func!(A B C D E F G H I J);

