use std::sync::{Arc, RwLock};

use crate::{unit::{FuncId, backends::CallableFunc, callable::CallInput}, Value};

use super::IntepreterState;
use super::interpreter::run;

pub struct LoweredInterpreterFunc<A, R> {
    pub(super) state: Arc<RwLock<IntepreterState>>,
    pub(super) func_id: FuncId,
    pub(super) _marker: std::marker::PhantomData<(A, R)>,
}

impl<A, R> CallableFunc<A, R> for LoweredInterpreterFunc<A, R> where A: CallInput<R> {}

macro_rules! impl_interpreter_func {
    ($($t:ident)*) => {
        impl<$($t,)* R> FnOnce<($($t,)*)> for LoweredInterpreterFunc<($($t,)*), R>
        where
            ($($t,)*): CallInput<R>,
        {
            type Output = Value;

            extern "rust-call" fn call_once(self, args: ($($t,)*)) -> Self::Output {
                self.call(args)
            }
        }

        impl<$($t,)* R> FnMut<($($t,)*)> for LoweredInterpreterFunc<($($t,)*), R>
        where
            ($($t,)*): CallInput<R>,
        {
            extern "rust-call" fn call_mut(&mut self, args: ($($t,)*)) -> Self::Output {
                self.call(args)
            }
        }

        impl<$($t,)* R> Fn<($($t,)*)> for LoweredInterpreterFunc<($($t,)*), R>
        where
            ($($t,)*): CallInput<R>,
        {
            extern "rust-call" fn call(&self, args: ($($t,)*)) -> Self::Output {
                let state = self.state.read().unwrap();
                run(self.func_id, &*state)
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

