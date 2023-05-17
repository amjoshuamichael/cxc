use std::marker::Tuple;

pub trait CallInput<R>: Tuple + Sized {
    type CFunc;

    fn externalize(addr: *const usize) -> Self::CFunc;
    unsafe fn call(addr: *const usize, args: Self) -> R;
}

macro_rules! impl_call_input {
    ($($t:ident)*) => {
        impl<$($t,)* R> CallInput<R> for ($($t,)*) {
            type CFunc = unsafe extern "C" fn($($t,)*) -> R;

            #[inline(always)]
            fn externalize(addr: *const usize) -> Self::CFunc {
                unsafe { std::mem::transmute(addr) }
            }

            #[inline(always)]
            unsafe fn call(addr: *const usize, args: Self) -> R {
                #[cfg(feature = "ffi-assertions")]
                {
                    let mem_region = region::query(addr).unwrap();

                    if !mem_region.is_executable() {
                        panic!("Woah! The memory region of a function is not executable! That shouldn't happen. This error occured while calling a cxc function at address '{addr:?}'. If the issue persists, please submit an issue at https://github.com/amjoshuamichael/cxc/issues/new.");
                    }
                }

                #[allow(non_snake_case)]
                let ($($t,)*) = args;
                let out = Self::externalize(addr)($($t),*);

                out
            }
        }

        
    }
}

impl_call_input!();
impl_call_input!(A);
impl_call_input!(A B);
impl_call_input!(A B C);
impl_call_input!(A B C D);
impl_call_input!(A B C D E);
impl_call_input!(A B C D E F G);
impl_call_input!(A B C D E F G H);
impl_call_input!(A B C D E F G H I);
impl_call_input!(A B C D E F G H I J);
