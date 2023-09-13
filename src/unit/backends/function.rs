use std::ptr::NonNull;
use crate::FuncQuery;
use core::fmt::Debug;
use std::sync::{Arc, RwLock};

use cxc_derive::xc_opaque;

use crate::unit::callable::CallInput;
use crate::{unit::XcReflectMac, FuncType};
use crate as cxc;

use super::{LowerableFunc, CallableFunc};

// A set of reusable structs for handling heap-allocated function pointers across backends

macro_rules! impl_func_downcasted {
    ($($t:ident)*) => {
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

impl_func_downcasted!();
impl_func_downcasted!(A);
impl_func_downcasted!(A B);
impl_func_downcasted!(A B C);
impl_func_downcasted!(A B C D);
impl_func_downcasted!(A B C D E);
impl_func_downcasted!(A B C D E F G);
impl_func_downcasted!(A B C D E F G H);
impl_func_downcasted!(A B C D E F G H I);
impl_func_downcasted!(A B C D E F G H I J);

#[derive(Clone, Debug, XcReflectMac)]
#[xc_opaque] // TODO: add RwLock to cxc
pub struct Func {
    inner: Arc<RwLock<FuncInner>>,
}

pub struct FuncDowncasted<A, R> {
    pub inner: Func,
    pub _phantoms: (std::marker::PhantomData<A>, std::marker::PhantomData<R>),
}

impl<A, R> CallableFunc<A, R> for FuncDowncasted<A, R> where A: CallInput<R> {}

impl<A, R> Debug for FuncDowncasted<A, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.inner.code().pointer())
    }
}

impl<A, R> Clone for FuncDowncasted<A, R> {
    fn clone(&self) -> Self {
        FuncDowncasted {
            inner: self.inner.clone(),
            _phantoms: Default::default(),
        }
    }
}

// TODO: include name, relation, and generics in FuncInner

impl Func {
    fn new(inner: FuncInner) -> Func {
        Self {
            inner: Arc::new(RwLock::new(inner)),
        }
    }

    pub fn downcast<A, R>(&self) -> FuncDowncasted<A, R> where A: CallInput<R> {
        self.lower::<A, R>()
    }

    pub fn new_compiled(typ: FuncType) -> Func {
        Self::new(FuncInner {
            typ,
            code: FuncCodePtr::Compiled { pointer: None },
        })
    }

    pub fn new_external(
        typ: FuncType,
        pointer: *const usize,
    ) -> Func {
        Self::new(FuncInner {
            typ,
            code: FuncCodePtr::External {
                pointer,
            },
        })
    }

    pub fn typ(&self) -> FuncType { self.inner.read().unwrap().typ.clone() }
    pub fn code(&self) -> FuncCodePtr {
        let inner = self.inner.read().unwrap();
        inner.code.clone()
    }

    pub(crate) fn set_pointer(&self, pointer: *const usize) {
        let mut inner = self.inner.write().unwrap();
        inner.code = FuncCodePtr::Compiled {
            pointer: NonNull::new(pointer as *mut _),
        };
    }

    pub fn get_pointer(&self) -> *const usize {
        let inner = self.inner.read().unwrap();

        inner.code.pointer().unwrap()
    }
}

impl LowerableFunc for Func {
    type LowerTo<A, R> = FuncDowncasted<A, R>;

    fn lower<A, R>(&self) -> FuncDowncasted<A, R>
    where
        A: CallInput<R>,
    {
        FuncDowncasted {
            inner: self.clone(),
            _phantoms: (std::marker::PhantomData, std::marker::PhantomData),
        }
    }
}

#[derive(Debug, XcReflectMac)]
struct FuncInner {
    typ: FuncType,
    code: FuncCodePtr,
}

#[derive(Clone, Debug, XcReflectMac)]
#[xc_opaque] // TODO: we shouldn't have to do this
pub enum FuncCodePtr {
    Compiled {
        pointer: Option<NonNull<usize>>,
    },
    External {
        pointer: *const usize,
    },
}

impl FuncCodePtr {
    pub fn pointer(&self) -> Option<*const usize> {
        match self {
            FuncCodePtr::Compiled { ref pointer } => pointer.map(|x| x.as_ptr() as *const _),
            FuncCodePtr::External { pointer, .. } => Some(*pointer),
        }
    }
}
