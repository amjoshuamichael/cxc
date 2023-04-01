use super::functions::FuncDowncasted;
use crate::unit::Func;
use std::marker::Tuple;

pub trait CallInput<R>: Tuple + Sized {
    type CFunc;

    fn externalize(addr: *const usize) -> Self::CFunc;
    unsafe fn call(addr: *const usize, args: Self) -> R;
}

macro_rules! impl_externalize {
    ($($t:ident)*) => {
        impl<$($t,)* R> CallInput<R> for ($($t,)*) {
            type CFunc = unsafe extern "C" fn($($t,)*) -> R;

            #[inline(always)]
            fn externalize(addr: *const usize) -> Self::CFunc {
                unsafe { std::mem::transmute(addr) }
            }

            #[inline(always)]
            unsafe fn call(addr: *const usize, args: Self) -> R {
                #[allow(non_snake_case)]
                let ($($t,)*) = args;
                Self::externalize(addr)($($t),*)
            }
        }

        impl<$($t,)* R> FnOnce<($($t,)*)> for FuncDowncasted<($($t,)*), R>
        where
            ($($t,)*): CallInput<R>,
        {
            type Output = R;

            extern "rust-call" fn call_once(self, args: ($($t,)*)) -> Self::Output {
                // TODO: this gets set off if a function that returns a value doesn't have a 
                // return statement. I'm not sure why, it's something to look into
                let fn_ptr = self.inner.code().pointer().expect("function has not been initialized!");
                unsafe { <($($t,)*)>::call(fn_ptr, args) }
            }
        }

        impl<$($t,)* R> FnMut<($($t,)*)> for FuncDowncasted<($($t,)*), R>
        where
            ($($t,)*): CallInput<R>,
        {
            extern "rust-call" fn call_mut(&mut self, args: ($($t,)*)) -> Self::Output {
                let fn_ptr = self.inner.code().pointer().expect("function has not been initialized!");
                unsafe { <($($t,)*)>::call(fn_ptr, args) }
            }
        }

        impl<$($t,)* R> Fn<($($t,)*)> for FuncDowncasted<($($t,)*), R>
        where
            ($($t,)*): CallInput<R>,
        {
            extern "rust-call" fn call(&self, args: ($($t,)*)) -> Self::Output {
                let fn_ptr = self.inner.code().pointer().expect("function has not been initialized!");
                unsafe { <($($t,)*)>::call(fn_ptr, args) }
            }
        }
    }
}

impl_externalize!();
impl_externalize!(A);
impl_externalize!(A B);
impl_externalize!(A B C);
impl_externalize!(A B C D);
impl_externalize!(A B C D E);
impl_externalize!(A B C D E F G);
impl_externalize!(A B C D E F G H);
impl_externalize!(A B C D E F G H I);
impl_externalize!(A B C D E F G H I J);

impl Func {
    pub fn downcast<A, R>(&self) -> FuncDowncasted<A, R>
    where
        A: CallInput<R>,
    {
        FuncDowncasted {
            inner: self.clone(),
            _phantoms: (std::marker::PhantomData, std::marker::PhantomData),
        }
    }
}
